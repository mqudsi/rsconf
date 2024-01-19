mod tempdir;
#[cfg(test)]
mod tests;

use cc::Build;
pub use sealed::{Header, OptionalHeader, OptionalLibrary};
use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::io::prelude::*;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicI32, Ordering};
use tempdir::TempDir;

static FILE_COUNTER: AtomicI32 = AtomicI32::new(0);
type BoxedError = Box<dyn std::error::Error + Send + Sync + 'static>;

/// Exposes an interface for testing whether the target system supports a particular feature or
/// provides certain functionality. This is the bulk of the `rsconf` api.
pub struct Target {
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum BuildMode {
    Executable,
    ObjectFile,
}

/// Specifies how a dependency library is linked.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum LinkType {
    /// Cargo is instructed to link the library without specifying/overriding how linking is
    /// performed. If an environment variable `LIBNAME_STATIC` is present, the dependency will be
    /// statically linked. (This way, downstream consumers of the crate may influence how the
    /// dependency is linked without modifying the build script and/or features.)
    ///
    /// Cargo is instructed to automatically rerun the build script if an environment variable by
    /// this name exists; you do not have to call [`rebuild_if_env_changed()`] yourself.
    #[default]
    Default,
    /// Cargo will be instructed to explicitly dynamically link against the target library,
    /// overriding the default configuration specified by the configuration or the toolchain.
    Dynamic,
    /// Cargo will be instructed to explicitly statically link against the target library,
    /// overriding the default configuration specified by the configuration or the toolchain.
    Static,
}

impl LinkType {
    fn emit_link_line(&self, lib: &str) {
        match self {
            LinkType::Static => println!("cargo:rustc-link-lib=static={lib}"),
            LinkType::Dynamic => println!("cargo:rustc-link-lib=dylib={lib}"),
            LinkType::Default => {
                // We do not specify the build type unless the LIBNAME_STATIC environment variable
                // is defined (and not set to 0), in which was we emit a static linkage instruction.
                let name = format!("{}_STATIC", lib.to_ascii_uppercase());
                println!("cargo:rerun-if-env-changed={name}");
                match std::env::var(name).as_deref() {
                    Err(_) | Ok("0") => println!("cargo:rustc-link-lib={lib}"),
                    _ => LinkType::Static.emit_link_line(lib),
                }
            }
        }
    }
}

/// Instruct Cargo to link the target object against `library`.
pub fn link_library(library: &str, how: LinkType) {
    how.emit_link_line(library)
}

/// Instruct Cargo to link the target object against `libraries` in the order provided.
pub fn link_libraries<S: AsRef<str>>(libraries: &[S], how: LinkType) {
    for lib in libraries {
        how.emit_link_line(lib.as_ref())
    }
}

/// Instruct Cargo to rerun the build script if the following path changes (based off the last
/// modification time). If the path is to a directory, the build script is re-run if any files under
/// that directory are modified.
///
/// By default, Cargo reruns the build script if any file in the source tree is modified. To make it
/// ignore changes, specify a file. To make it ignore all changes, call this with `"build.rs"` as
/// the target.
pub fn rebuild_if_path_changed(path: &str) {
    println!("cargo:rerun-if-changed={path}");
}

/// Instruct Cargo to rerun the build script if any of the following paths change.
///
/// See [`rebuild_if_path_changed()`] for more information.
pub fn rebuild_if_paths_changed<I, S: AsRef<str>>(paths: I)
where
    I: IntoIterator<Item = S>,
{
    for path in paths {
        rebuild_if_path_changed(path.as_ref())
    }
}

/// Instruct Cargo to rerun the build script if the named environment variable changes.
pub fn rebuild_if_env_changed(var: &str) {
    println!("cargo:rerun-if-env-changed={var}");
}

/// Instruct Cargo to rerun the build script if any of the named environment variables change.
pub fn rebuild_if_envs_changed<I, S: AsRef<str>>(vars: I)
where
    I: IntoIterator<Item = S>,
{
    for var in vars {
        rebuild_if_env_changed(var.as_ref());
    }
}

/// Emit a compile-time warning.
///
/// This is typically only shown for the current crate when building with `cargo build`, but
/// warnings for non-path dependencies can be shown by using `cargo build -vv`.
#[macro_export]
macro_rules! warn {
    ($msg:tt $(, $($arg:tt)*)?) => {{
        println!(concat!("cargo:warning=", $msg) $(, $($arg)*)?)
    }};
}

/// Enables a feature flag that compiles code annotated with `#[cfg(feature = "feature")]`.
///
/// The feature does not have to be named in `Cargo.toml` to be used here or in your code, but any
/// features dynamically enabled via this script will not participate in dependency resolution.
pub fn enable_feature(feature: &str) {
    if feature.chars().any(|c| c == '"') {
        panic!("Invalid feature name: {feature}");
    }
    println!("cargo:rustc-cfg=feature=\"{feature}\"");
}

/// Enables conditional compilation of code behind `#[cfg(name)]` or with `if cfg!(name)`
/// (without quotes around `name`).
///
/// See [`set_cfg_value()`] to set a `(name, value)` tuple to enable conditional compilation of the
/// form `#[cfg(name = "value")]` for cases where `name` is not a boolean cfg but rather takes any
/// of several discrete values.
///
/// Note the different from `#[cfg(feature = "name")]`! The configuration is invisible to end users
/// of your code (i.e. `name` does not appear anywhere in `Cargo.toml`) and does not participate in
/// dependency resolution.
pub fn enable_cfg(name: &str) {
    println!("cargo:rustc-cfg={name}");
}

/// Activates conditional compilation for code behind `#[cfg(name = "value")]` or with `if cfg!(name
/// = "value")`.
///
/// As with [`enable_cfg()`], this is entirely internal to your code: `name` should not appear in
/// `Cargo.toml` and this configuration does not participate in dependency resolution (which takes
/// place before your build script is called).
pub fn set_cfg_value(name: &str, value: &str) {
    if value.chars().any(|c| c == '"') {
        panic!("Invalid value {value} for cfg {name}");
    }
    println!("cargo:rustc-cfg={name}={value}\"");
}

/// Makes available an environment variable available to your code at build time, letting you use
/// the value as a compile-time constant with `env!(NAME)`.
pub fn set_env_value(name: &str, value: &str) {
    if value.chars().any(|c| c == '"') {
        panic!("Invalid value {value} for env var {name}");
    }
    println!("cargo:rustc-env={name}={value}");
}

/// Add a path to the list of directories rust will search when attempting to find a library to link
/// against.
///
/// The path does not have to exist as it could be created by the build script at a later date or
/// could be targeting a different platform altogether.
pub fn add_library_search_path(dir: &str) {
    println!("cargo:rustc-link-search={dir}");
}

impl Target {
    const NONE: &'static [&'static str] = &[];

    /// Create a new rsconf instance using the default [`cc::Build`] toolchain for the current
    /// compilation target.
    ///
    /// Use [`Target::new_from()`] to use a configured [`cc::Build`] instance instead.
    pub fn new() -> std::io::Result<Target> {
        let toolchain = cc::Build::new();
        Target::new_from(toolchain)
    }

    /// Create a new rsconf instance from the configured [`cc::Build`] instance `toolchain`.
    ///
    /// All tests inherit their base configuration from `toolchain`, so make sure it is configured
    /// with the appropriate header and library search paths as needed.
    pub fn new_from(mut toolchain: cc::Build) -> std::io::Result<Target> {
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
    /// In verbose mode, output of rsconf calls to the compiler are displayed to stdout and stderr.
    /// It is not enabled by default.
    ///
    /// Note that `cargo` suppresses all `build.rs` output in case of successful execution by
    /// default; intentionally fail the build (e.g. add a `panic!()` call) or compile with `cargo
    /// build -vv` to see verbose output.
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
        cmd.current_dir(&self.temp);

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
                cmd.arg("/link");
                for library in libraries {
                    let mut library = Cow::from(library.as_ref());
                    if !library.contains('.') {
                        let owned = library + ".lib";
                        library = owned;
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
    /// include no headers), use [`has_type_in`](Self::has_type_in) to check for a
    /// definition after including zero or more headers in the order they are provided.
    ///
    /// This operation does not link the output; only the header file is inspected.
    pub fn has_type<'a, H: OptionalHeader<'a>>(&'a self, definition: &str, header: H) -> bool {
        let snippet = format!(snippet!("has_type.c"), header.to_header_lines(), definition);
        self.build(definition, BuildMode::ObjectFile, &snippet, Self::NONE)
            .is_ok()
    }

    /// Checks whether definition `definition` exists and has a value in the supplied `headers`.
    ///
    /// The `headers` are included in the order they are provided. See
    /// [`has_type()`](Self::has_type) for more info.
    pub fn has_type_in(&self, definition: &str, headers: &[&str]) -> bool {
        let stub = format!("{}_multi", *headers.first().unwrap_or(&"has_type_in"));
        let headers = to_includes(headers);
        let snippet = format!(snippet!("has_type.c"), headers, definition);
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
    /// See [`has_type()`](Self::has_type) to check for compile-time definitions. This
    /// function will return false if `library` could not be found or could not be linked; see
    /// [`has_library()`](Self::has_library) to test if `library` can be linked separately.
    pub fn has_symbol<'a, L: OptionalLibrary<'a>>(&self, symbol: &str, library: L) -> bool {
        let snippet = format!(snippet!("has_symbol.c"), symbol);
        let libs = [library.preview_lib()];
        let libs = if libs[0].is_empty() {
            &libs[..0]
        } else {
            &libs
        };
        self.build(symbol, BuildMode::Executable, &snippet, libs)
            .is_ok()
    }

    /// Like [`has_symbol()`] but links against any number of `libraries` in the order they are
    /// provided when testing.
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

    /// Checks for the presence of all the named symbols, linking against all of `libraries`
    /// in the order they were provided when testing.
    ///
    /// See [`has_symbol()`] and [`has_symbol_in()`] for more information.
    ///
    /// [`has_symbol()`]: Self::has_symbol()
    /// [`has_symbol_in()`]: Self::has_symbol_in()
    pub fn has_symbols_in<S1: AsRef<str>, S2: AsRef<str>>(
        &self,
        symbols: &[S1],
        libraries: &[S2],
    ) -> bool {
        symbols
            .iter()
            .all(|symbol| self.has_symbol_in(symbol.as_ref(), libraries))
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
    /// passing it to [`Target::new()`].
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
    /// linked in the order they are provided in when testing.
    pub fn has_libraries<S: AsRef<str>>(&self, libraries: &[S]) -> bool {
        let stub = libraries
            .get(0)
            .map(|s| s.as_ref())
            .unwrap_or("has_libraries");
        let snippet = snippet!("empty.c");
        self.build(stub, BuildMode::ObjectFile, snippet, libraries)
            .is_ok()
    }

    /// Returns a reference to the first library name that was passed in that was ultimately found
    /// on the target system, or `None` if none were found. See
    /// [`has_library()`](Self::has_library()) for more information.
    pub fn find_first_library<'a, S: AsRef<str>>(&self, libraries: &'a [S]) -> Option<&'a str> {
        for lib in libraries {
            if self.has_library(lib.as_ref()) {
                return Some(lib.as_ref());
            }
        }
        None
    }

    /// Returns a reference to the first library name that was passed in that was ultimately found
    /// on the target system and contains all the symbol names provided, or `None` if no such
    /// library was found. See [`has_library()`](Self::has_library()) and [`has_symbol()`] for more
    /// information.
    ///
    /// [`has_symbol()`]: Self::has_symbol()
    pub fn find_first_library_with<'a, S1: AsRef<str>, S2: AsRef<str>>(
        &self,
        libraries: &'a [S1],
        symbols: &[S2],
    ) -> Option<&'a str> {
        for lib in libraries {
            if !self.has_library(lib.as_ref()) {
                continue;
            }
            if self.has_symbols_in(symbols, &[lib]) {
                return Some(lib.as_ref());
            }
        }
        None
    }

    /// Checks whether the [`cc::Build`] passed to [`Target::new()`] as configured can pull in the
    /// named `header` file.
    ///
    /// If including `header` requires pulling in additional headers before it to compile, use
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

    /// Checks whether the [`cc::Build`] passed to [`Target::new()`] as configured can pull in the
    /// named `headers` in the order they're provided.
    pub fn has_headers<S: AsRef<str>>(&self, headers: &[S]) -> bool {
        let stub = headers.get(0).map(|s| s.as_ref()).unwrap_or("has_headers");
        let snippet = format!(snippet!("has_header.c"), to_includes(headers));
        self.build(stub, BuildMode::ObjectFile, &snippet, Self::NONE)
            .is_ok()
    }

    /// A convenience function that links against `library` if it is found and linkable.
    ///
    /// This is internally a call to [`has_library()`](Self::has_library()) followed by a
    /// conditional call to [`link_library()`].
    pub fn try_link_library(&self, library: &str, how: LinkType) -> bool {
        if self.has_library(library) {
            link_library(library, how);
            return true;
        }
        false
    }

    /// A convenience function that links against `libraries` only if they are all found and
    /// linkable.
    ///
    /// This is internally a call to [`has_libraries()`](Self::has_libraries()) followed by a
    /// conditional call to [`link_libraries()`].
    pub fn try_link_libraries<S: AsRef<str>>(&self, libraries: &[S], how: LinkType) -> bool {
        if self.has_libraries(libraries) {
            link_libraries(libraries, how);
            return true;
        }
        false
    }

    /// Evaluates whether or not `define` is an extant preprocessor definition.
    ///
    /// This is the C equivalent of `#ifdef xxxx` and does not check if there is a value associated
    /// with the definition. (You can use [`r#if()`](Self::if()) to test if a define has a particular
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
}

impl From<cc::Build> for Target {
    fn from(build: cc::Build) -> Self {
        Self::new_from(build).unwrap()
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
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn to_header_lines(&self) -> String;
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview(&self) -> &'a str;
    }

    impl<'a> OptionalHeader<'a> for &'a str {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn to_header_lines(&self) -> String {
            to_include(self)
        }

        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview(&self) -> &'a str {
            self
        }
    }

    impl<'a> OptionalHeader<'a> for &'a String {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn to_header_lines(&self) -> String {
            self.as_str().to_header_lines()
        }

        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview(&self) -> &'a str {
            self.as_str()
        }
    }

    impl<'a> OptionalHeader<'a> for &[&'a str] {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn to_header_lines(&self) -> String {
            to_includes(self)
        }

        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview(&self) -> &'a str {
            self.first().copied().unwrap_or("")
        }
    }

    impl<'a, const N: usize> OptionalHeader<'a> for [&'a str; N] {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn to_header_lines(&self) -> String {
            self.as_slice().to_header_lines()
        }

        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview(&self) -> &'a str {
            self.as_slice().preview()
        }
    }

    #[cfg_attr(debug_assertions, doc(hidden))]
    pub enum Never {}

    impl<'a> OptionalHeader<'a> for Option<Never> {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn to_header_lines(&self) -> String {
            String::new()
        }

        #[cfg_attr(debug_assertions, doc(hidden))]
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

    /// An abstraction to make it possible to check for or include zero or more libraries. Libraries are
    /// included in the same order they are provided in.
    ///
    /// Implemented for `String` and `&str` types as well as a literal `None`.
    pub trait OptionalLibrary<'a> {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview_lib(&self) -> &'a str;
    }

    impl<'a> OptionalLibrary<'a> for &'a str {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview_lib(&self) -> &'a str {
            self
        }
    }

    impl<'a> OptionalLibrary<'a> for &'a String {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview_lib(&self) -> &'a str {
            self.as_str()
        }
    }

    impl<'a> OptionalLibrary<'a> for Option<Never> {
        #[cfg_attr(debug_assertions, doc(hidden))]
        fn preview_lib(&self) -> &'a str {
            ""
        }
    }
}
