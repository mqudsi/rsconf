use std::path::{Path, PathBuf};

/// A temporary directory deleted on `Drop`.
pub struct TempDir {
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

    /// Convert the [`TempDir`] instance into a [`PathBuf`], effectively suppressing the `Drop`
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
