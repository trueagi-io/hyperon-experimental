
use std::path::{Path, PathBuf};
use std::io::Write;
use std::fs;
use std::borrow::Borrow;
use std::sync::Arc;

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
    is_test: bool,
}

const DEFAULT_INIT_METTA: &[u8] = include_bytes!("init.default.metta");

static PLATFORM_ENV: std::sync::OnceLock<Arc<Environment>> = std::sync::OnceLock::new();

impl Environment {

    /// Returns a reference to the shared "platform" Environment
    pub fn platform_env() -> &'static Self {
        PLATFORM_ENV.get_or_init(|| Arc::new(EnvBuilder::new().build()))
    }

    /// Internal function to get a copy of the platform Environment's Arc ptr
    pub(crate) fn platform_env_arc() -> Arc<Self> {
        PLATFORM_ENV.get_or_init(|| Arc::new(EnvBuilder::new().build())).clone()
    }

    /// Returns the Path to the config dir, in an OS-specific location
    pub fn config_dir(&self) -> Option<&Path> {
        self.config_dir.as_deref()
    }

    /// Returns the Path to the environment's working_dir
    ///
    /// NOTE: The Environment's working_dir is not the same as the process working directory, and
    /// changing the process's working directory will not affect the environment
    pub fn working_dir(&self) -> Option<&Path> {
        self.working_dir.as_deref()
    }

    /// Returns the path to the init.metta file, that is run to initialize a MeTTa runner and customize the MeTTa environment
    pub fn initialization_metta_file_path(&self) -> Option<&Path> {
        self.init_metta_path.as_deref()
    }

    /// Returns the extra search paths in the environment, in search priority order.  Results do not
    /// include the working_dir
    pub fn extra_include_paths<'a>(&'a self) -> impl Iterator<Item=&Path> + 'a {
        self.extra_include_paths.iter().map(|path| path.borrow())
    }

    /// Private "default" function
    fn new() -> Self {
        Self {
            config_dir: None,
            init_metta_path: None,
            working_dir: None,
            extra_include_paths: vec![],
            is_test: false,
        }
    }
}

/// Used to customize the [Environment] configuration
///
/// NOTE: It is not necessary to use the EnvBuilder if the default environment is acceptable
pub struct EnvBuilder {
    env: Environment,
    no_cfg_dir: bool,
}

impl EnvBuilder {

    /// Returns a new EnvBuilder, to set the parameters for the MeTTa Environment
    ///
    /// NOTE: Unless otherwise specified by calling either [set_no_config_dir] or [set_config_dir], the
    ///   [Environment] will be configured using the OS-Specific platform configuration files.
    ///
    /// Depending on the host OS, the config directory locations will be:
    /// * Linux: ~/.config/metta/
    /// * Windows: ~\AppData\Roaming\TrueAGI\metta\config\
    /// * Mac: ~/Library/Application Support/io.TrueAGI.metta/
    ///
    /// TODO: Repeat this documentation somewhere more prominent, like the top-level README
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
            no_cfg_dir: false,
        }
    }

    /// A convenience function to construct an environment suitable for unit tests
    ///
    /// The `test_env` Environment will not load or create any files.  Additionally
    /// this method will initialize the logger for the test environment
    pub fn test_env() -> Self {
        EnvBuilder::new().set_is_test(true).set_no_config_dir()
    }

    /// Sets (or unsets) the working_dir for the environment
    pub fn set_working_dir(mut self, working_dir: Option<&Path>) -> Self {
        self.env.working_dir = working_dir.map(|dir| dir.into());
        self
    }

    /// Sets the `config_dir` that the environment will load.  A directory at the specified path will
    /// be created its contents populated with default values, if one does not already exist
    pub fn set_config_dir(mut self, config_dir: &Path) -> Self {
        self.env.config_dir = Some(config_dir.into());
        if self.no_cfg_dir {
            panic!("Fatal Error: set_config_dir is incompatible with set_no_config_dir");
        }
        self
    }

    /// Configures the Environment not to load nor create any config files
    pub fn set_no_config_dir(mut self) -> Self {
        self.no_cfg_dir = true;
        if self.env.config_dir.is_some() {
            panic!("Fatal Error: set_config_dir is incompatible with set_no_config_dir");
        }
        self
    }

    /// Sets the `is_test` flag for the environment, to specify whether the environment is a unit-test
    ///
    /// NOTE: This currently applies to the logger, but may affect other behaviors in the future.
    ///     See [env_logger::is_test](https://docs.rs/env_logger/latest/env_logger/struct.Builder.html#method.is_test)
    pub fn set_is_test(mut self, is_test: bool) -> Self {
        self.env.is_test = is_test;
        self
    }

    /// Adds additional include paths to search for MeTTa modules
    ///
    /// NOTE: The most recently added paths will have the highest search priority, save for the `working_dir`,
    ///   and paths returned first by the iterator will have higher priority within the same call to add_include_paths.
    pub fn add_include_paths<P: Borrow<Path>, I: IntoIterator<Item=P>>(mut self, paths: I) -> Self {
        let mut additional_paths: Vec<PathBuf> = paths.into_iter().map(|path| path.borrow().into()).collect();
        additional_paths.extend(self.env.extra_include_paths);
        self.env.extra_include_paths = additional_paths;
        self
    }

    /// Initializes the shared platform Environment, accessible with [platform_env]
    ///
    /// NOTE: This method will panic if the platform Environment has already been initialized
    pub fn init_platform_env(self) {
        self.try_init_platform_env().expect("Fatal Error: Platform Environment already initialized");
    }

    /// Initializes the shared platform Environment.  Non-panicking version of [init_platform_env]
    pub fn try_init_platform_env(self) -> Result<(), &'static str> {
        PLATFORM_ENV.set(Arc::new(self.build())).map_err(|_| "Platform Environment already initialized")
    }

    /// Returns a newly created Environment from the builder configuration
    ///
    /// NOTE: Creating owned Environments is usually not necessary.  It is usually sufficient to use the [platform_env] method.
    pub(crate) fn build(self) -> Environment {

        let mut env = self.env;

        //Init the logger.  This will have no effect if the logger has already been initialized
        let _ = env_logger::builder().is_test(env.is_test).try_init();

        if !self.no_cfg_dir {
            if env.config_dir.is_none() {
                match ProjectDirs::from("io", "TrueAGI",  "metta") {
                    Some(proj_dirs) => {
                        env.config_dir = Some(proj_dirs.config_dir().into());
                    },
                    None => {
                        eprint!("Failed to initialize config with OS config directory!");
                    }
                }
            }
        }

        if let Some(config_dir) = &env.config_dir {

            //Create the modules dir inside the config dir, if it doesn't already exist.
            // This will create the cfg_dir iteslf in the process
            let modules_dir = config_dir.join("modules");
            std::fs::create_dir_all(&modules_dir).unwrap();

            //Push the "modules" dir, as the last place to search after the other paths that were specified
            //TODO: the config.metta file will be able to append / modify the search paths, and can choose not to
            // include the "modules" dir in the future.
            env.extra_include_paths.push(modules_dir);

            //Create the default init.metta file if it doesn't already exist
            let init_metta_path = config_dir.join("init.metta");
            if !init_metta_path.exists() {
                let mut file = fs::OpenOptions::new()
                    .create(true)
                    .write(true)
                    .open(&init_metta_path)
                    .expect(&format!("Error creating default init file at {init_metta_path:?}"));
                file.write_all(&DEFAULT_INIT_METTA).unwrap();
            }
            env.init_metta_path = Some(init_metta_path);
        }

        //TODO: This line below is a stop-gap to match old behavior
        //As discussed with Vitaly, searching the current working dir can be a potential security hole
        // However, that is mitigated by "." being the last directory searched.
        // Anyway, the issue is that the Metta::import_file method in runner.py relies on using the
        // same runner but being able to change the search path by changing the working dir.
        // A better fix is to fork a "child runner" with access to the same space and tokenizer,
        // but an updated search path.  This is really hard to implement currently given the ImportOp
        // actually owns a reference to the runner it's associated with.  However this must be fixed soon.
        env.extra_include_paths.push(".".into());

        env
    }

}
