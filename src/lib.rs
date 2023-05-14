mod tempdir;
#[cfg(test)]
mod tests;

use cc::Build;
pub use sealed::{Header, OptionalHeader};
use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::io::prelude::*;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicI32, Ordering};
use tempdir::TempDir;

static FILE_COUNTER: AtomicI32 = AtomicI32::new(0);
type BoxedError = Box<dyn std::error::Error + Send + Sync + 'static>;

pub struct Detector {
    /// Whether or not we are compiling with `cl.exe` (and not `clang.exe`) under `xxx-pc-windows-msvc`.
    is_cl: bool,
    temp: TempDir,
    toolchain: Build,
    verbose: bool,
}

macro_rules! snippet {
    ($name:expr) => {
        include_str!(concat!("../snippets/", $name))
    };
}

#[derive(Debug)]
pub struct CompilationError {
    output: Output,
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Compilation error: {}",
            String::from_utf8_lossy(&self.output.stderr)
        ))
    }
}

impl std::error::Error for CompilationError {}

fn output_or_err(output: Output) -> Result<String, BoxedError> {
    if output.status.success() {
        Ok(String::from_utf8(output.stdout)?)
    } else {
        Err(Box::new(CompilationError { output }))
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum BuildMode {
    Executable,
    ObjectFile,
}

/// Instruct Cargo to link the target object against `library`.
pub fn link_library(library: &str) {
    println!("cargo:rustc-link-lib={library}");
}

/// Instruct Cargo to link the target object against `libraries` in the order provided.
pub fn link_libraries<S: AsRef<str>>(libraries: &[S]) {
    for lib in libraries {
        println!("cargo:rustc-link-lib={}", lib.as_ref());
    }
}

impl Detector {
    const NONE: &[&'static str] = &[];

    /// Create a new rsconf instance from the configured [`cc::Build`] instance `toolchain`.
    ///
    /// All tests inherit their base configuration from `toolchain`, so make sure it is configured
    /// with the appropriate header and library search paths as needed.
    pub fn new(mut toolchain: cc::Build) -> std::io::Result<Detector> {
        let temp = if let Some(out_dir) = std::env::var_os("OUT_DIR") {
            TempDir::new_in(out_dir)?
        } else {
            // Configure Build's OUT_DIR if not set (e.g. for testing)
            let temp = TempDir::new()?;
            toolchain.out_dir(&temp);
            temp
        };

        let is_cl = cfg!(windows) && toolchain.get_compiler().is_like_msvc();

        Ok(Self {
            is_cl,
            temp,
            toolchain,
            verbose: false,
        })
    }

    /// Enables or disables verbose mode.
    ///
    /// In verbose mode, compiler output is displayed to stdout and stderr. It is not enabled by
    /// default.
    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    fn new_temp<S: AsRef<str>>(&self, stub: S, ext: &str) -> PathBuf {
        let file_num = FILE_COUNTER.fetch_add(1, Ordering::Release);
        let stub = stub.as_ref();
        let mut path = self.temp.to_owned();
        path.push(format!("{stub}-test-{file_num}{ext}"));
        path
    }

    fn build<S: AsRef<str>>(
        &self,
        stub: &str,
        mode: BuildMode,
        code: &str,
        libraries: &[S],
    ) -> Result<PathBuf, BoxedError> {
        let stub = fs_sanitize(stub);

        let in_path = self.new_temp(&stub, ".c");
        std::fs::File::create(&in_path)?.write_all(code.as_bytes())?;
        let exe_ext = if cfg!(unix) { ".out" } else { ".exe" };
        let obj_ext = if cfg!(unix) { ".o" } else { ".obj" };
        let out_path = match mode {
            BuildMode::Executable => self.new_temp(&stub, exe_ext),
            BuildMode::ObjectFile => self.new_temp(&stub, obj_ext),
        };
        let mut cmd = self.toolchain.try_get_compiler()?.to_command();

        let exe = mode == BuildMode::Executable;
        let link = exe || !libraries.is_empty();
        let output = if cfg!(unix) || !self.is_cl {
            cmd.args([in_path.as_os_str(), OsStr::new("-o"), out_path.as_os_str()]);
            if !link {
                cmd.arg("-c");
            } else if !libraries.is_empty() {
                for library in libraries {
                    cmd.arg(format!("-l{}", library.as_ref()));
                }
            }
            cmd
        } else {
            cmd.arg(in_path);
            let mut output = OsString::from(if exe { "/Fe:" } else { "/Fo:" });
            output.push(&out_path);
            cmd.arg(output);
            if !link {
                cmd.arg("/c");
            } else if !libraries.is_empty() {
                for library in libraries {
                    let mut library = Cow::from(library.as_ref());
                    if !library.contains('.') {
                        let owned = library.to_owned() + ".lib";
                        library = Cow::from(owned);
                    }
                    cmd.arg(library.as_ref());
                }
            }
            cmd
        }
        .output()?;

        // We want to output text in verbose mode but writing directly to stdout doesn't get
        // intercepted by the cargo test harness. In test mode, we use the slower `println!()`/
        // `eprintln!()` macros together w/ from_utf8_lossy() to suppress unnecessary output when
        // we're not investigating the details with `cargo test -- --nocapture`, but we use the
        // faster approach when we're being used in an actual build script.
        #[cfg(test)]
        if self.verbose {
            println!("{}", String::from_utf8_lossy(&output.stdout));
            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        }
        #[cfg(not(test))]
        if self.verbose {
            std::io::stdout().lock().write_all(&output.stdout).ok();
            std::io::stderr().lock().write_all(&output.stderr).ok();
        }
        // Handle custom `CompilationError` output if we failed to compile.
        output_or_err(output)?;

        // Return the path to the resulting exe
        assert!(out_path.exists());
        Ok(out_path)
    }

    /// Checks whether definition `definition` exists and has a value in the supplied `header`.
    ///
    /// If it is not possible to include `header` without including other headers as well (or to
    /// include no headers), use [`has_definition_in`](Self::has_definition_in) to check for a
    /// definition after including zero or more headers in the order they are provided.
    ///
    /// This operation does not link the output; only the header file is inspected.
    pub fn has_definition(&self, definition: &str, header: &str) -> bool {
        let snippet = format!(snippet!("has_definition.c"), to_include(header), definition);
        self.build(definition, BuildMode::ObjectFile, &snippet, Self::NONE)
            .is_ok()
    }

    /// Checks whether definition `definition` exists and has a value in the supplied `headers`.
    ///
    /// The `headers` are included in the order they are provided. See
    /// [`has_definition()`](Self::has_definition) for more info.
    pub fn has_definition_in(&self, definition: &str, headers: &[&str]) -> bool {
        let stub = format!("{}_multi", *headers.get(0).unwrap_or(&"has_definition_in"));
        let headers = to_includes(headers);
        let snippet = format!(snippet!("has_definition.c"), headers, definition);
        self.build(&stub, BuildMode::ObjectFile, &snippet, Self::NONE)
            .is_ok()
    }

    /// Checks whether or not the the requested `symbol` is exported by `library`.
    ///
    /// If `library` cannot be linked without also linking its transitive dependencies, use
    /// [`has_symbol_in()`](Self::has_symbol_in) to link against multiple libraries and test.
    ///
    /// This only checks for symbols exported by the C abi (so mangled names are required) and does
    /// not check for compile-time definitions provided by header files.
    ///
    /// See [`has_definition()`](Self::has_definition) to check for compile-time definitions. This
    /// function will return false if `library` could not be found or could not be linked; see
    /// [`has_library()`](Self::has_library) to test if `library` can be linked separately.
    pub fn has_symbol(&self, symbol: &str, library: &str) -> bool {
        let snippet = format!(snippet!("has_symbol.c"), symbol);
        self.build(symbol, BuildMode::Executable, &snippet, &[library])
            .is_ok()
    }

    /// Like [`has_symbol()`] but links against any number of `libraries` in the order they are
    /// provided in.
    ///
    /// This can be used when `symbol` is in a library that has its own transitive dependencies that
    /// must also be linked. See [`has_symbol()`] for more information.
    ///
    /// [`has_symbol()`]: Self::has_symbol()
    pub fn has_symbol_in<S: AsRef<str>>(&self, symbol: &str, libraries: &[S]) -> bool {
        let snippet = format!(snippet!("has_symbol.c"), symbol);
        self.build(symbol, BuildMode::Executable, &snippet, libraries)
            .is_ok()
    }

    /// Returns whether or not it was possible to link against `library`.
    ///
    /// If it is not possible to link against `library` without also linking against its transitive
    /// dependencies, use [`has_libraries()`](Self::has_libraries) to link against multiple
    /// libraries (in the order provided).
    ///
    /// You should normally pass the name of the library without any prefixes or suffixes. If a
    /// suffix is provided, it will not be removed.
    ///
    /// You may pass a full path to the library (again minus the extension) instead of just the
    /// library name in order to try linking against a library not in the library search path.
    /// Alternatively, configure the [`cc::Build`] instance with the search paths as needed before
    /// passing it to [`Detector::new()`].
    ///
    /// Under Windows, if `library` does not have an extension it will be suffixed with `.lib` prior
    /// to testing linking. (This way it works under under both `cl.exe` and `clang.exe`.)
    pub fn has_library(&self, library: &str) -> bool {
        let snippet = snippet!("empty.c");
        self.build(library, BuildMode::ObjectFile, snippet, &[library])
            .is_ok()
    }

    /// Returns whether or not it was possible to link against all of `libraries`. See
    /// [`has_library()`](Self::has_library()) for more information.
    ///
    /// Note that the order of linking may influence the outcome of this test. The libraries will be
    /// linked in the order they are provided in.
    pub fn has_libraries<S: AsRef<str>>(&self, libraries: &[S]) -> bool {
        let stub = libraries
            .get(0)
            .map(|s| s.as_ref())
            .unwrap_or("has_libraries");
        let snippet = snippet!("empty.c");
        self.build(stub, BuildMode::ObjectFile, snippet, libraries)
            .is_ok()
    }

    /// A convenience function that links against `library` if it is found and linkable.
    ///
    /// This is internally a call to [`has_library()`](Self::has_library()) followed by a
    /// conditional call to [`link_library()`].
    pub fn try_link_library(&self, library: &str) -> bool {
        if self.has_library(library) {
            link_library(library);
            return true;
        }
        false
    }

    /// A convenience function that links against `libraries` only if they are all found and
    /// linkable.
    ///
    /// This is internally a call to [`has_libraries()`](Self::has_libraries()) followed by a
    /// conditional call to [`link_libraries()`].
    pub fn try_link_libraries<S: AsRef<str>>(&self, libraries: &[S]) -> bool {
        if self.has_libraries(libraries) {
            link_libraries(libraries);
            return true;
        }
        false
    }

    /// Attempts to retrieve the definition of `ident` as an `i32` value. Returns `Ok` in case
    /// `ident` was defined, has a concrete value, is a compile-time constant (i.e. does not need to
    /// be linked to retrieve the value), and is a valid `i32` value.
    ///
    /// # Cross-compliation note:
    ///
    /// The `get_xxx_value()` methods do not currently support cross-compilation scenarios as they
    /// require being able to run a binary compiled for the target platform.
    pub fn get_i32_value<'a, H>(&self, ident: &str, header: H) -> Result<i32, BoxedError>
    where
        H: Header<'a>,
    {
        let mut h = header.to_header_lines();
        h.push_str(header.preview());
        let snippet = format!(snippet!("get_i32_value.c"), h, ident);
        let exe = self.build(ident, BuildMode::Executable, &snippet, Self::NONE)?;

        let output = Command::new(exe).output().map_err(|err| {
            format!(
                "Failed to run the test executable: {err}!\n{}",
                "Note that get_i32_value() does not support cross-compilation!"
            )
        })?;
        Ok(std::str::from_utf8(&output.stdout)?.parse()?)
    }

    /// Attempts to retrieve the definition of `ident` as a `u32` value. Returns `Ok` in case
    /// `ident` was defined, has a concrete value, is a compile-time constant (i.e. does not need to
    /// be linked to retrieve the value), and is a valid `u32` value.
    ///
    /// # Cross-compliation note:
    ///
    /// The `get_xxx_value()` methods do not currently support cross-compilation scenarios as they
    /// require being able to run a binary compiled for the target platform.
    pub fn get_u32_value<'a, H>(&self, ident: &str, header: H) -> Result<u32, BoxedError>
    where
        H: Header<'a>,
    {
        let snippet = format!(snippet!("get_u32_value.c"), header.to_header_lines(), ident);
        let exe = self.build(ident, BuildMode::Executable, &snippet, Self::NONE)?;

        let output = Command::new(exe).output().map_err(|err| {
            format!(
                "Failed to run the test executable: {err}!\n{}",
                "Note that get_u32_value() does not support cross-compilation!"
            )
        })?;
        Ok(std::str::from_utf8(&output.stdout)?.parse()?)
    }

    /// Attempts to retrieve the definition of `ident` as an `i64` value. Returns `Ok` in case
    /// `ident` was defined, has a concrete value, is a compile-time constant (i.e. does not need to
    /// be linked to retrieve the value), and is a valid `i64` value.
    ///
    /// # Cross-compliation note:
    ///
    /// The `get_xxx_value()` methods do not currently support cross-compilation scenarios as they
    /// require being able to run a binary compiled for the target platform.
    pub fn get_i64_value<'a, H>(&self, ident: &str, header: H) -> Result<i64, BoxedError>
    where
        H: Header<'a>,
    {
        let snippet = format!(snippet!("get_i64_value.c"), header.to_header_lines(), ident);
        let exe = self.build(ident, BuildMode::Executable, &snippet, Self::NONE)?;

        let output = Command::new(exe).output().map_err(|err| {
            format!(
                "Failed to run the test executable: {err}!\n{}",
                "Note that get_i64_value() does not support cross-compilation!"
            )
        })?;
        Ok(std::str::from_utf8(&output.stdout)?.parse()?)
    }

    /// Attempts to retrieve the definition of `ident` as a `u64` value. Returns `Ok` in case
    /// `ident` was defined, has a concrete value, is a compile-time constant (i.e. does not need to
    /// be linked to retrieve the value), and is a valid `u64` value.
    ///
    /// # Cross-compliation note:
    ///
    /// The `get_xxx_value()` methods do not currently support cross-compilation scenarios as they
    /// require being able to run a binary compiled for the target platform.
    pub fn get_u64_value<'a, H>(&self, ident: &str, header: H) -> Result<u64, BoxedError>
    where
        H: Header<'a>,
    {
        let snippet = format!(snippet!("get_u64_value.c"), header.to_header_lines(), ident);
        let exe = self.build(ident, BuildMode::Executable, &snippet, Self::NONE)?;

        let output = Command::new(exe).output().map_err(|err| {
            format!(
                "Failed to run the test executable: {err}!\n{}",
                "Note that get_u64_value() does not support cross-compilation!"
            )
        })?;
        Ok(std::str::from_utf8(&output.stdout)?.parse()?)
    }

    /// Checks whether the [`cc::Build`] passed to [`Detector::new()`] as configured can pull in the
    /// named `header` file.
    ///
    /// If including `header` requires pulling in additional headers before it, use
    /// [`has_headers()`](Self::has_headers) instead to include multiple headers in the order
    /// they're specified.
    pub fn has_header(&self, header: &str) -> bool {
        let snippet = format!(snippet!("has_header.c"), to_include(header));
        self.build(
            header.preview(),
            BuildMode::ObjectFile,
            &snippet,
            Self::NONE,
        )
        .is_ok()
    }

    /// Checks whether the [`cc::Build`] passed to [`Detector::new()`] as configured can pull in the
    /// named `headers` in the order they're provided.
    pub fn has_headers<S: AsRef<str>>(&self, headers: &[S]) -> bool {
        let stub = headers.get(0).map(|s| s.as_ref()).unwrap_or("has_headers");
        let snippet = format!(snippet!("has_header.c"), to_includes(headers));
        self.build(stub, BuildMode::ObjectFile, &snippet, Self::NONE)
            .is_ok()
    }

    /// Evaluates whether or not `define` is an extant preprocessor definition.
    ///
    /// This is the C equivalent of `#ifdef xxxx` and does not check if there is a value associated
    /// with the definition. (You can use [`if()`](Self::if()) to test if a define has a particular
    /// value.)
    pub fn ifdef<'a, H>(&self, define: &str, headers: H) -> bool
    where
        H: OptionalHeader<'a>,
    {
        let snippet = format!(snippet!("ifdef.c"), headers.to_header_lines(), define);
        self.build(define, BuildMode::ObjectFile, &snippet, Self::NONE)
            .is_ok()
    }

    /// Evaluates whether or not `condition` evaluates to true at preprocessor time.
    ///
    /// This can be used with `condition` set to `defined(FOO)` to perform the equivalent of
    /// [`ifdef()`](Self::ifdef) or it can be used to check for specific values e.g. with
    /// `condition` set to something like `FOO != 0`.
    pub fn r#if<'a, H>(&self, condition: &str, headers: H) -> bool
    where
        H: OptionalHeader<'a>,
    {
        let snippet = format!(snippet!("if.c"), headers.to_header_lines(), condition);
        self.build(condition, BuildMode::ObjectFile, &snippet, Self::NONE)
            .is_ok()
    }
}

/// Sanitizes a string for use in a file name
fn fs_sanitize(s: &str) -> Cow<'_, str> {
    if s.chars().all(|c| c.is_ascii_alphanumeric()) {
        return Cow::Borrowed(s);
    }

    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if !c.is_ascii_alphanumeric() {
            out.push('_');
        } else {
            out.push(c);
        }
    }
    Cow::Owned(out)
}

/// Convert header filename `header` to a `#include <..>` statement.
fn to_include(header: &str) -> String {
    format!("#include <{}>", header)
}

fn to_includes<S: AsRef<str>>(headers: &[S]) -> String {
    let mut vec = Vec::with_capacity(headers.len());
    // TODO: Use collect_into() once it's stabilized instead of looping.
    for line in headers.iter().map(|s| s.as_ref()).map(to_include) {
        vec.push(line);
    }
    vec.join("\n")
}

mod sealed {
    use crate::{to_include, to_includes};

    /// An abstraction to make it possible to check for or include zero or more headers. Headers are
    /// included in the same order they are provided in.
    ///
    /// Implemented for all [`Header`] types as well as a literal `None`.
    pub trait OptionalHeader<'a> {
        fn to_header_lines(&self) -> String;
        fn preview(&self) -> &'a str;
    }

    impl<'a> OptionalHeader<'a> for &'a str {
        fn to_header_lines(&self) -> String {
            to_include(self)
        }

        fn preview(&self) -> &'a str {
            *self
        }
    }

    impl<'a> OptionalHeader<'a> for &'a String {
        fn to_header_lines(&self) -> String {
            self.as_str().to_header_lines()
        }

        fn preview(&self) -> &'a str {
            self.as_str()
        }
    }

    impl<'a> OptionalHeader<'a> for &[&'a str] {
        fn to_header_lines(&self) -> String {
            to_includes(self)
        }

        fn preview(&self) -> &'a str {
            self.get(0).map(|s| *s).unwrap_or("")
        }
    }

    impl<'a, const N: usize> OptionalHeader<'a> for [&'a str; N] {
        fn to_header_lines(&self) -> String {
            self.as_slice().to_header_lines()
        }

        fn preview(&self) -> &'a str {
            self.as_slice().preview()
        }
    }

    pub enum Never {}

    impl<'a> OptionalHeader<'a> for Option<Never> {
        fn to_header_lines(&self) -> String {
            String::new()
        }

        fn preview(&self) -> &'a str {
            ""
        }
    }

    /// An abstraction to make it possible to check for or include one or more headers. Headers are
    /// included in the same order they are provided in.
    ///
    /// Implemented for `&str`, `&String`, `&[&str]`, and `[&str; N]`.
    pub trait Header<'a>: OptionalHeader<'a> {}

    impl<'a> Header<'a> for &'a str {}
    impl<'a> Header<'a> for &'a String {}
    impl<'a, const N: usize> Header<'a> for [&'a str; N] {}
}
