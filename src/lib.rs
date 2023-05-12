#[cfg(test)]
mod tests;

use cc::Build;
use std::ffi::{OsStr, OsString};
use std::fs::File;
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

    fn new_temp(&self, ext: &str) -> PathBuf {
        let file_num = FILE_COUNTER.fetch_add(1, Ordering::Release);
        let mut path = self.temp.to_owned();
        path.push(format!("test-{file_num}{ext}"));
        path
    }

    fn build(&self, code: &str) -> Result<PathBuf, BoxedError> {
        let in_path = self.new_temp(".c");
        std::fs::File::create(&in_path)?.write_all(code.as_bytes())?;
        let out_path = self.new_temp(".o");
        let mut cmd = self.compiler.try_get_compiler()?.to_command();
        let output = if cfg!(unix) || !self.is_cl {
            cmd.args([in_path.as_os_str(), OsStr::new("-o"), out_path.as_os_str()])
        } else {
            let mut output = OsString::from("/out:");
            output.push(&out_path);
            cmd.args([in_path.as_os_str(), OsStr::new("/link"), &output])
        }
        .output()?;
        if self.verbose {
            std::io::stdout().lock().write_all(&output.stdout).ok();
            std::io::stderr().lock().write_all(&output.stderr).ok();
        }
        // Handle custom `CompilationError` output if we failed to compile.
        let _ = output_or_err(output)?;

        // Return the path to the resulting exe
        assert!(out_path.exists());
        Ok(out_path)
    }

    fn build_exe(&self, code: &str) -> Result<PathBuf, BoxedError> {
        let in_path = self.new_temp(".c");
        File::create(&in_path)?.write_all(code.as_bytes())?;
        let out_path = self.new_temp(".o");
        let mut cmd = self.compiler.try_get_compiler()?.to_command();
        let output = if cfg!(unix) || !self.is_cl {
            cmd.args([in_path.as_os_str(), OsStr::new("-o"), out_path.as_os_str()])
        } else {
            let mut output = OsString::from("/out:");
            output.push(&out_path);
            cmd.args([in_path.as_os_str(), OsStr::new("/link"), &output])
        }
        .output()?;
        if self.verbose {
            std::io::stdout().lock().write_all(&output.stdout).ok();
            std::io::stderr().lock().write_all(&output.stderr).ok();
        }
        // Handle custom `CompilationError` output if we failed to compile.
        let _ = output_or_err(output)?;

        // Return the path to the resulting exe
        assert!(out_path.exists());
        Ok(out_path)
    }

    pub fn symbol_is_defined(&self, header: &str, symbol: &str) -> bool {
        let snippet = format!(snippet!("symbol_is_defined.c"), header, symbol);
        self.build(&snippet).is_ok()
    }

    pub fn symbol_i32_value(&self, header: &str, symbol: &str) -> Result<i32, BoxedError> {
        let snippet = format!(snippet!("symbol_i32_value.c"), header, symbol);
        let exe = self.build_exe(&snippet)?;

        let output = Command::new(exe).output().map_err(|err| {
            format!(
                "Failed to run the test executable: {err}!\n{}",
                "Note that symbol_i32_value() does not support cross-compilation!"
            )
        })?;
        Ok(std::str::from_utf8(&output.stdout)?.parse()?)
    }

    pub fn symbol_u32_value(&self, header: &str, symbol: &str) -> Result<u32, BoxedError> {
        let snippet = format!(snippet!("symbol_u32_value.c"), header, symbol);
        let exe = self.build_exe(&snippet)?;

        let output = Command::new(exe).output().map_err(|err| {
            format!(
                "Failed to run the test executable: {err}!\n{}",
                "Note that symbol_u32_value() does not support cross-compilation!"
            )
        })?;
        Ok(std::str::from_utf8(&output.stdout)?.parse()?)
    }

    pub fn symbol_i64_value(&self, header: &str, symbol: &str) -> Result<i64, BoxedError> {
        let snippet = format!(snippet!("symbol_i64_value.c"), header, symbol);
        let exe = self.build_exe(&snippet)?;

        let output = Command::new(exe).output().map_err(|err| {
            format!(
                "Failed to run the test executable: {err}!\n{}",
                "Note that symbol_i64_value() does not support cross-compilation!"
            )
        })?;
        Ok(std::str::from_utf8(&output.stdout)?.parse()?)
    }

    pub fn symbol_u64_value(&self, header: &str, symbol: &str) -> Result<u64, BoxedError> {
        let snippet = format!(snippet!("symbol_u64_value.c"), header, symbol);
        let exe = self.build_exe(&snippet)?;

        let output = Command::new(exe).output().map_err(|err| {
            format!(
                "Failed to run the test executable: {err}!\n{}",
                "Note that symbol_u64_value() does not support cross-compilation!"
            )
        })?;
        Ok(std::str::from_utf8(&output.stdout)?.parse()?)
    }

    pub fn is_defined(&self, header: Option<&str>, define: &str) -> bool {
        let header = header.unwrap_or("stdio.h");
        let snippet = format!(snippet!("is_defined.c"), header, define);
        self.build(&snippet).is_ok()
    }

    pub fn has_header(&self, header: &str) -> bool {
        let snippet = format!(snippet!("has_header.c"), header);
        self.build(&snippet).is_ok()
    }

    pub fn r#if(&self, header: Option<&str>, condition: &str) -> bool {
        let header = header.unwrap_or("stdio.h");
        let snippet = format!(snippet!("if.c"), header, condition);
        self.build(&snippet).is_ok()
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
