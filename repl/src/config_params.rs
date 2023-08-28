
use std::path::{Path, PathBuf};

#[derive(Default, Debug)]
pub struct ReplParams {
    config_dir: PathBuf,
    include_paths: Vec<PathBuf>,
    pub history_file: Option<PathBuf>,
}

impl ReplParams {
    pub fn from_config_dir(path: &Path) -> Self {

        //Create the modules dir inside the config dir, if it doesn't already exist
        let modules_dir = path.join("modules");
        std::fs::create_dir_all(&modules_dir).unwrap();

        Self {
            config_dir: path.into(),
            include_paths: vec![],
            history_file: Some(path.join("history.txt")),
        }
    }
    pub fn push_include_paths(&mut self, paths: Vec<PathBuf>) {
        self.include_paths.extend(paths);
    }

    /// Returns the search paths, in order to search
    ///
    /// The current working directory is always returned first, and the modules directory
    /// is always last in the search path.  In the middle are the user-supplied search paths
    pub fn modules_search_paths<'a>(&'a self) -> impl Iterator<Item=PathBuf> + 'a {
        let cwd = match std::env::current_dir() {
            Ok(cwd) => cwd,
            Err(_) => PathBuf::from("./"),
        };
        [cwd].into_iter().chain(
            self.include_paths.iter().cloned().chain(
                [self.config_dir.join("modules")].into_iter()
            )
        )
    }

}
