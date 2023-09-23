
use std::path::{Path, PathBuf};
use std::io::Write;
use std::fs;
use std::borrow::Borrow;

use directories::ProjectDirs;

/// Contains state and platform interfaces shared by all MeTTa runners.  This includes config settings
/// and logger
///
/// Generally there will be only one environment object needed, and it can be accessed by calling the [platform_env] method
#[derive(Debug)]
pub struct Environment {
    config_dir: Option<PathBuf>,
    init_metta_path: Option<PathBuf>,
    working_dir: Option<PathBuf>,
    extra_include_paths: Vec<PathBuf>,
}

const DEFAULT_INIT_METTA: &[u8] = include_bytes!("init.default.metta");

static PLATFORM_ENV: std::sync::OnceLock<Environment> = std::sync::OnceLock::new();

impl Environment {

    /// Returns a reference to the shared "platform" Environment
    pub fn platform_env() -> &'static Self {
        PLATFORM_ENV.get_or_init(|| Self::new_with_defaults(None))
    }

    /// Initializes the shared "platform" Environment with with the OS-Specific platform configuration
    ///
    /// Config directory locations will be:
    /// Linux: ~/.config/metta/
    /// Windows: ~\AppData\Roaming\TrueAGI\metta\config\
    /// Mac: ~/Library/Application Support/io.TrueAGI.metta/
    ///
    /// TODO: Repeat this documentation somewhere more prominent, like the top-level README
    ///
    /// NOTE: This method will panic if the platform Environment has already been initialized
    pub fn init(working_dir: Option<&Path>) {
        PLATFORM_ENV.set(Self::new_with_defaults(working_dir)).expect("Fatal Error: Platform Environment already initialized");
    }

    /// Initializes the shared "platform" Environment with with the configuration stored in the `config_dir`.  Will create
    /// `config_dir` and its contents if it does not exist
    ///
    /// NOTE: This method will panic if the platform Environment has already been initialized
    pub fn init_with_cfg_dir(working_dir: Option<&Path>, config_dir: &Path) {
        PLATFORM_ENV.set(Self::new_with_config_dir(working_dir, config_dir)).expect("Fatal Error: Platform Environment already initialized");
    }

    /// Returns the Path to the config dir, in an OS-specific location
    pub fn config_dir(&self) -> Option<&Path> {
        self.config_dir.as_deref()
    }

    /// Returns the path to the init.metta file, that is run to initialize a MeTTa runner and customize the MeTTa environment
    pub fn initialization_metta_file_path(&self) -> Option<&Path> {
        self.init_metta_path.as_deref()
    }

    /// Returns the search paths to look in for MeTTa modules, in search priority order
    ///
    /// The working_dir is always returned first
    pub fn modules_search_paths<'a>(&'a self) -> impl Iterator<Item=&Path> + 'a {
        [&self.working_dir].into_iter().filter_map(|opt| opt.as_deref())
            .chain(self.extra_include_paths.iter().map(|path| path.borrow()))
    }

    /// Returns a newly created Environment with the OS-specific platform defaults
    ///
    /// NOTE: Creating owned Environments is usually not necessary.  It is usually sufficient to use the [platform_env].
    pub fn new_with_defaults(working_dir: Option<&Path>) -> Self {
        match ProjectDirs::from("io", "TrueAGI",  "metta") {
            Some(proj_dirs) => {
                Self::new_with_config_dir(working_dir, proj_dirs.config_dir())
            },
            None => {
                eprint!("Failed to initialize config!");
                Self::new_without_config_dir(working_dir)
            }
        }
    }

    /// Returns a newly created Environment with the configuration stored in the `config_dir`.  Will create
    /// `config_dir` and its contents if it does not exist
    ///
    /// NOTE: Creating owned Environments is usually not necessary.  It is usually sufficient to use the [platform_env].
    pub fn new_with_config_dir(working_dir: Option<&Path>, config_dir: &Path) -> Self {

        //Create the modules dir inside the config dir, if it doesn't already exist.
        // This will create the cfg_dir iteslf in the process
        let modules_dir = config_dir.join("modules");
        std::fs::create_dir_all(&modules_dir).unwrap();

        //Create the default init.metta file if they don't already exist
        let init_metta_path = config_dir.join("init.metta");
        if !init_metta_path.exists() {
            let mut file = fs::OpenOptions::new()
                .create(true)
                .write(true)
                .open(&init_metta_path)
                .expect(&format!("Error creating default init file at {init_metta_path:?}"));
            file.write_all(&DEFAULT_INIT_METTA).unwrap();
        }

        //TODO_NOW, come back here and rethink what we do with include_paths.  ie. where they are stored
        // //Push the "modules" dir, as the last place to search after the paths specified on the cmd line
        // //TODO: the config.metta file will be able to append / modify the search paths, and can choose not to
        // // include the "modules" dir in the future.
        // let mut include_paths = include_paths;
        // include_paths.push(modules_dir);

        Self {
            config_dir: Some(config_dir.into()),
            init_metta_path: Some(init_metta_path),
            working_dir: working_dir.map(|dir| dir.into()),
            extra_include_paths: vec![], //TODO_NOW, need a way to pass in extra include paths.  This is the straw that pushes me over to a builder API
        }
    }

    /// Returns a newly created Environment with no specialized configuration.  This method will not touch any files.
    ///
    /// NOTE: Creating owned Environments is usually not necessary.  It is usually sufficient to use the [platform_env].
    pub fn new_without_config_dir(working_dir: Option<&Path>) -> Self {
        Self {
            config_dir: None,
            init_metta_path: None,
            working_dir: working_dir.map(|dir| dir.into()),
            extra_include_paths: vec![],
        }
    }
}

//TODO_NOW: Transition to a builder API
// pub struct EnvBuilder {
//     env: Environment
// }

// impl EnvBuilder {

//     /// Returns a new EnvBuilder, to set the parameters for the MeTTa Environment
//     pub fn new() -> Self {

//     }
// }