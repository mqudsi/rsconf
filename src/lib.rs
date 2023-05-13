#[cfg(test)]
mod tests;

use cc::Build;
use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicI32, Ordering};

static FILE_COUNTER: AtomicI32 = AtomicI32::new(0);
type BoxedError = Box<dyn std::error::Error + Send + Sync + 'static>;

pub struct Detector {
    compiler: Build,
    temp: TempDir,
    verbose: bool,
    /// Whether or not we are compiling with cl.exe (and not clang.exe) under xxx-pc-windows-msvc.
    is_cl: bool,
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

impl Detector {
    pub fn new(mut compiler: cc::Build) -> std::io::Result<Detector> {
        let temp = if let Some(out_dir) = std::env::var_os("OUT_DIR") {
            TempDir::new_in(out_dir)?
        } else {
            // Configure Build's OUT_DIR if not set (e.g. for testing)
            let temp = TempDir::new()?;
            compiler.out_dir(&temp);
            temp
        };

        let is_cl = cfg!(windows) && compiler.get_compiler().is_like_msvc();

        Ok(Self {
            compiler,
            temp,
            is_cl,
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

    fn build(
        &self,
        stub: &str,
        mode: BuildMode,
        code: &str,
        library: Option<&str>,
    ) -> Result<PathBuf, BoxedError> {
        let stub = Self::fs_sanitize(stub);
        let mut library = library.map(Cow::from);
        // #[cfg(windows)]
        if library.as_ref().map(|lib| !lib.contains('.')).unwrap_or(false) {
            let owned = library.unwrap().into_owned() + ".lib";
            library = Some(Cow::from(owned));
        }

        let in_path = self.new_temp(&stub, ".c");
        std::fs::File::create(&in_path)?.write_all(code.as_bytes())?;
        let exe_ext = if cfg!(unix) { ".out" } else { ".exe" };
        let obj_ext = if cfg!(unix) { ".o" } else { ".obj" };
        let out_path = match mode {
            BuildMode::Executable => self.new_temp(&stub, exe_ext),
            BuildMode::ObjectFile => self.new_temp(&stub, obj_ext),
        };
        let mut cmd = self.compiler.try_get_compiler()?.to_command();

        let exe = mode == BuildMode::Executable;
        let link = exe || library.is_some();
        let output = if cfg!(unix) || !self.is_cl {
            cmd.args([in_path.as_os_str(), OsStr::new("-o"), out_path.as_os_str()]);
            if !link {
                cmd.arg("-c");
            } else if let Some(library) = library {
                cmd.arg(format!("-l{library}"));
            }
            cmd
        } else {
            cmd.arg(in_path);
            let mut output = OsString::from(if exe { "/Fe:" } else { "/Fo:" });
            output.push(&out_path);
            cmd.arg(output);
            if !link {
                cmd.arg("/c");
            } else if let Some(library) = library {
                cmd.arg(&*library);
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

    /// Checks whether a definition for `ident` exists in the supplied `header`.
    ///
    /// This operation does not link the output; only the header file is inspected.
    pub fn is_defined(&self, header: &str, ident: &str) -> bool {
        let snippet = format!(snippet!("is_defined.c"), header, ident);
        self.build(ident, BuildMode::ObjectFile, &snippet, None).is_ok()
    }

    /// Attempts to retrieve the definition of `ident` as an `i32` value. Returns `Ok` in case
    /// `ident` was defined, has a concrete value, is a compile-time constant (i.e. does not need to
    /// be linked to retrieve the value), and is a valid `i32` value.
    ///
    /// # Note on cross-compilation:
    ///
    /// The `get_xxx_value()` methods do not currently support cross-compilation scenarios as they
    /// require being able to run a binary compiled for the target platform.
    pub fn get_i32_value(&self, header: &str, ident: &str) -> Result<i32, BoxedError> {
        let snippet = format!(snippet!("get_i32_value.c"), header, ident);
        let exe = self.build(ident, BuildMode::Executable, &snippet, None)?;

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
    /// # Note on cross-compilation:
    ///
    /// The `get_xxx_value()` methods do not currently support cross-compilation scenarios as they
    /// require being able to run a binary compiled for the target platform.
    pub fn get_u32_value(&self, header: &str, ident: &str) -> Result<u32, BoxedError> {
        let snippet = format!(snippet!("get_u32_value.c"), header, ident);
        let exe = self.build(ident, BuildMode::Executable, &snippet, None)?;

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
    /// # Note on cross-compilation:
    ///
    /// The `get_xxx_value()` methods do not currently support cross-compilation scenarios as they
    /// require being able to run a binary compiled for the target platform.
    pub fn get_i64_value(&self, header: &str, ident: &str) -> Result<i64, BoxedError> {
        let snippet = format!(snippet!("get_i64_value.c"), header, ident);
        let exe = self.build(ident, BuildMode::Executable, &snippet, None)?;

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
    /// # Note on cross-compilation:
    ///
    /// The `get_xxx_value()` methods do not currently support cross-compilation scenarios as they
    /// require being able to run a binary compiled for the target platform.
    pub fn get_u64_value(&self, header: &str, ident: &str) -> Result<u64, BoxedError> {
        let snippet = format!(snippet!("get_u64_value.c"), header, ident);
        let exe = self.build(ident, BuildMode::Executable, &snippet, None)?;

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
    pub fn has_header(&self, header: &str) -> bool {
        let snippet = format!(snippet!("has_header.c"), header);
        self.build(header, BuildMode::ObjectFile, &snippet, None).is_ok()
    }

    /// Evaluates whether or not `define` is an extant preprocessor definition.
    ///
    /// This is the C equivalent of `#ifdef xxxx` and does not check if there is a value associated
    /// with the definition. (You can use [`if()`](Self::if()) to test if a define has a particular
    /// value.)
    pub fn ifdef(&self, header: Option<&str>, define: &str) -> bool {
        let header = header.unwrap_or("stdio.h");
        let snippet = format!(snippet!("ifdef.c"), header, define);
        self.build(define, BuildMode::ObjectFile, &snippet, None).is_ok()
    }

    /// Evaluates whether or not `condition` evaluates to true at preprocessor time.
    ///
    /// This can be used with `condition` set to `defined(XXX)` to perform the equivalent of
    /// [`ifdef()`](Self::ifdef) or it can be used to check for specific values e.g. with
    /// `condition` set to something like `XXX != 0`.
    pub fn r#if(&self, header: Option<&str>, condition: &str) -> bool {
        let header = header.unwrap_or("stdio.h");
        let snippet = format!(snippet!("if.c"), header, condition);
        self.build(condition, BuildMode::ObjectFile, &snippet, None).is_ok()
    }

    /// Returns whether or not it was possible to link against `library`.
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
        self.build(library, BuildMode::Executable, snippet, Some(library)).is_ok()
    }
}

/// A temporary directory deleted on `Drop`.
struct TempDir {
    path: PathBuf,
}

impl TempDir {
    /// Tries to create a new temp directory as a child of the given directory.
    pub fn new_in<P: Into<PathBuf>>(path: P) -> std::io::Result<Self> {
        use std::collections::hash_map::RandomState;
        use std::hash::{BuildHasher, Hasher};

        let mut rng = RandomState::new().build_hasher();
        rng.write(b"rsconf");
        let rand = rng.finish() as u32;
        let dir_name = format!(".rsconf-{rand}");
        let mut path = path.into();
        path.push(dir_name);

        std::fs::create_dir_all(&path)?;

        Ok(TempDir { path })
    }

    /// Tries to create a new temp directory in the system temporary directory.
    pub fn new() -> std::io::Result<Self> {
        let parent = std::env::temp_dir();
        Self::new_in(parent)
    }

    /// Convert the [`TmpDir`] instance into a [`PathBuf`], effectively suppressing the `Drop`
    /// behavior (i.e. the directory will no longer be automatically deleted).
    #[allow(unused)]
    pub fn into_path(mut self) -> PathBuf {
        let path = std::mem::replace(&mut self.path, PathBuf::new());
        std::mem::forget(self);
        path
    }
}

impl std::ops::Deref for TempDir {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl AsRef<Path> for TempDir {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.path);
    }
}
