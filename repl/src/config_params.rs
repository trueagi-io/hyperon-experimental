
use std::path::{Path, PathBuf};
use std::io::Write;
use std::fs;

const DEFAULT_CONFIG_METTA: &[u8] = include_bytes!("config.default.metta");

#[derive(Default, Debug)]
pub struct ReplParams {
    /// Path to the config dir for the whole repl, in an OS-specific location
    config_dir: PathBuf,

    /// Path to the config.metta file, used to configure the repl
    config_metta_path: PathBuf,

    /// Path to the dir containing the script being run, or the cwd the repl was invoked from in interactive mode
    metta_working_dir: PathBuf,

    /// Other include paths, specified either through command-line args or config settings
    include_paths: Vec<PathBuf>,

    /// A file for previous statements in the interactive repl
    pub history_file: Option<PathBuf>,
}

impl ReplParams {
    pub fn new(config_dir: &Path, include_paths: Vec<PathBuf>, metta_file: Option<&PathBuf>) -> Self {

        //If we have a metta_file, then the working dir is the parent of that file
        //If we are running in interactive mode, it's the working dir at the time the repl is invoked
        let metta_working_dir: PathBuf = match metta_file {
            Some(metta_file) => {
                metta_file.parent().unwrap().into()
            },
            None => {
                match std::env::current_dir() {
                    Ok(cwd) => cwd,
                    Err(_) => PathBuf::from("./").canonicalize().unwrap(),
                }
            }
        };

        //Create the modules dir inside the config dir, if it doesn't already exist
        let modules_dir = config_dir.join("modules");
        std::fs::create_dir_all(&modules_dir).unwrap();

        //Create the default config.metta file, if none exists
        let config_metta_path = config_dir.join("config.metta");
        if !config_metta_path.exists() {
            let mut file = fs::OpenOptions::new()
                .create(true)
                .write(true)
                .open(&config_metta_path)
                .expect(&format!("Error creating default config file at {config_metta_path:?}"));
            file.write_all(&DEFAULT_CONFIG_METTA).unwrap();
        }

        //Push the "modules" dir, as the last place to search after the paths specified on the cmd line
        //TODO: the config.metta file will be able to append / modify the search paths, and can choose not to
        // include the "modules" dir in the future.
        let mut include_paths = include_paths;
        include_paths.push(modules_dir);

        Self {
            config_dir: config_dir.into(),
            config_metta_path,
            metta_working_dir,
            include_paths,
            history_file: Some(config_dir.join("history.txt")),
        }
    }

    /// Returns the search paths, in order to search
    ///
    /// The metta_working_dir is always returned first
    pub fn modules_search_paths<'a>(&'a self) -> impl Iterator<Item=PathBuf> + 'a {
        [self.metta_working_dir.clone()].into_iter().chain(
            self.include_paths.iter().cloned())
    }

    /// A path to the config.metta file that's run to configure the repl environment
    pub fn config_metta_path(&self) -> &PathBuf {

        //TODO: Temporary access to avoid warning.  Delete soon
        let _ = self.config_dir;

        &self.config_metta_path
    }

}
