
use std::path::{Path, PathBuf};
use std::io::Write;
use std::fs;
use std::sync::Arc;

use crate::metta::runner::modules::{ModuleCatalog, DirCatalog, FsModuleFormat, SingleFileModuleFmt, DirModuleFmt};

use directories::ProjectDirs;

/// Contains state and host platform interfaces shared by all MeTTa runners.  This includes config settings
/// and logger
///
/// Generally there will be only one environment object needed, and it can be accessed by calling the [common_env] method
#[derive(Debug)]
pub struct Environment {
    config_dir: Option<PathBuf>,
    init_metta_path: Option<PathBuf>,
    working_dir: Option<PathBuf>,
    is_test: bool,
    catalogs: Vec<Box<dyn ModuleCatalog>>,
    fs_mod_formats: Arc<Vec<Box<dyn FsModuleFormat>>>,
}

const DEFAULT_INIT_METTA: &[u8] = include_bytes!("init.default.metta");

static COMMON_ENV: std::sync::OnceLock<Arc<Environment>> = std::sync::OnceLock::new();

impl Environment {

    /// Returns a reference to the shared common Environment
    pub fn common_env() -> &'static Self {
        COMMON_ENV.get_or_init(|| Arc::new(EnvBuilder::new().build()))
    }

    /// Internal function to get a copy of the common Environment's Arc ptr
    pub(crate) fn common_env_arc() -> Arc<Self> {
        COMMON_ENV.get_or_init(|| Arc::new(EnvBuilder::new().build())).clone()
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

    /// Returns the [ModuleCatalog]s from the Environment, in search priority order
    pub fn catalogs<'a>(&'a self) -> impl Iterator<Item=&dyn ModuleCatalog> + 'a {
        self.catalogs.iter().map(|catalog| &**catalog as &dyn ModuleCatalog)
    }

    /// Returns the [FsModuleFormat]s from the Environment, in priority order
    pub fn fs_mod_formats<'a>(&'a self) -> impl Iterator<Item=&dyn FsModuleFormat> + 'a {
        self.fs_mod_formats.iter().map(|fmt| &**fmt as &dyn FsModuleFormat)
    }

    /// Private "default" function
    fn new() -> Self {
        Self {
            config_dir: None,
            init_metta_path: None,
            working_dir: None,
            is_test: false,
            catalogs: vec![],
            fs_mod_formats: Arc::new(vec![]),
        }
    }
}

/// Used to customize the [Environment] configuration
///
/// NOTE: It is not necessary to use the EnvBuilder if the default environment is acceptable
pub struct EnvBuilder {
    env: Environment,
    no_cfg_dir: bool,
    create_cfg_dir: bool,
    proto_catalogs: Vec<ProtoCatalog>,
    fs_mod_formats: Vec<Box<dyn FsModuleFormat>>
}

/// Private type representing something that will become an entry in the "Environment::catalogs" Vec
enum ProtoCatalog {
    Path(PathBuf),
    Other(Box<dyn ModuleCatalog>),
}

impl EnvBuilder {

    /// Returns a new EnvBuilder, to set the parameters for the MeTTa Environment
    ///
    /// NOTE: Unless otherwise specified by calling either [set_no_config_dir] or [set_config_dir], the
    ///   [Environment] will be configured using files in the OS-Specific configuration file locations.
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
            create_cfg_dir: false,
            proto_catalogs: vec![],
            fs_mod_formats: vec![],
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

    /// Configures the environment to create a config directory with default config files, if no directory is found
    ///
    /// NOTE: If the config directory exists but some config files are missing, default files will not be created.
    pub fn create_config_dir(mut self) -> Self {
        self.create_cfg_dir = true;
        if self.no_cfg_dir {
            panic!("Fatal Error: create_config_dir is incompatible with set_no_config_dir");
        }
        self
    }

    /// Configures the Environment not to load nor create any config files
    pub fn set_no_config_dir(mut self) -> Self {
        self.no_cfg_dir = true;
        if self.create_cfg_dir {
            panic!("Fatal Error: set_no_config_dir is incompatible with create_config_dir");
        }
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

    /// Adds additional search paths to search for MeTTa modules in the file system
    ///
    /// NOTE: include paths are a type of [ModuleCatalog], and the first catalog added will have the highest
    /// search priority, with subsequently added catalogs being search in order.  The `working_dir` will
    /// always be searched before any other catalogs.
    pub fn push_include_path<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.proto_catalogs.push(ProtoCatalog::Path(path.as_ref().into()));
        self
    }

    /// Adds an additional [ModuleCatalog] search for MeTTa modules
    ///
    /// NOTE: The first catalog added will have the highest search priority, with subsequently added catalogs
    /// being search in order.  The `working_dir` will always be searched before any other catalogs.
    pub fn push_module_catalog<C: ModuleCatalog + 'static>(mut self, catalog: C) -> Self {
        self.proto_catalogs.push(ProtoCatalog::Other(Box::new(catalog)));
        self
    }

    /// Registers a [FsModuleFormat] to identify and load modules stored on file-system media
    ///
    /// This is the mechanism used to detect and load modules in foreign formats, like a format specific
    /// to a host language such as Python
    ///
    /// NOTE: The first format added will have the highest search priority, with subsequently added formats
    /// being tried in order.  Built-in formats [SingleFileModuleFmt] and [DirModuleFmt] will be tried last.
    pub fn register_fs_module_format<F: FsModuleFormat + 'static>(mut self, fmt: F) -> Self {
        self.fs_mod_formats.push(Box::new(fmt));
        self
    }

    /// Initializes the shared common Environment, accessible with [common_env]
    ///
    /// NOTE: This method will panic if the common Environment has already been initialized
    pub fn init_common_env(self) {
        self.try_init_common_env().expect("Fatal Error: Common Environment already initialized");
    }

    /// Initializes the shared common Environment.  Non-panicking version of [init_common_env]
    pub fn try_init_common_env(self) -> Result<(), &'static str> {
        COMMON_ENV.set(Arc::new(self.build())).map_err(|_| "Common Environment already initialized")
    }

    /// Returns a newly created Environment from the builder configuration
    ///
    /// NOTE: Creating owned Environments is usually not necessary.  It is usually sufficient to use the [common_env] method.
    pub(crate) fn build(mut self) -> Environment {
        let mut env = self.env;

        //Init the logger.  This will have no effect if the logger has already been initialized
        let _ = env_logger::builder().is_test(env.is_test).try_init();

        //If we have a working_dir, make sure it gets searched first by building catalogs for it
        if let Some(working_dir) = &env.working_dir {
            self.proto_catalogs.insert(0, ProtoCatalog::Path(working_dir.into()));
        }

        //Construct the platform-specific config dir location, if an explicit location wasn't provided
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

            let modules_dir = config_dir.join("modules");
            let init_metta_path = config_dir.join("init.metta");

            //Create the default config dir, if that part of our directive
            if self.create_cfg_dir && !config_dir.exists() {

                //Create the modules dir inside the config dir
                // This will create the cfg_dir iteslf in the process
                std::fs::create_dir_all(&modules_dir).unwrap();

                //Create the default init.metta file
                let mut file = fs::OpenOptions::new()
                    .create(true)
                    .write(true)
                    .open(&init_metta_path)
                    .expect(&format!("Error creating default init file at {init_metta_path:?}"));
                file.write_all(&DEFAULT_INIT_METTA).unwrap();
            }

            //If the config_dir in the Environment still doesn't exist (and we couldn't create it), then set it to None
            if !config_dir.exists() {
                env.config_dir = None;
            }

            //Push the "modules" dir, as the last place to search after the other paths that were specified
            //TODO: the config.metta file should be able to append / modify the catalogs, and can choose not to
            // include the "modules" dir in the future.
            if modules_dir.exists() {
                self.proto_catalogs.push(ProtoCatalog::Path(modules_dir));
            }

            if init_metta_path.exists() {
                env.init_metta_path = Some(init_metta_path);
            }
        }

        // //LP-TODO-NEXT, remove this, since I think we don't need it anymore.  But first confirm
        // // all tests are passing, including Python sandbox examples
        // //TODO: This line below is a stop-gap to match old behavior
        // //As discussed with Vitaly, searching the current working dir can be a potential security hole
        // // However, that is mitigated by "." being the last directory searched.
        // // Anyway, the issue is that the Metta::import_file method in runner.py relies on using the
        // // same runner but being able to change the search path by changing the working dir.
        // // A better fix is to fork a "child runner" with access to the same space and tokenizer,
        // // but an updated search path.  This is really hard to implement currently given the ImportOp
        // // actually owns a reference to the runner it's associated with.  However this must be fixed soon.
        // env.extra_include_paths.push(".".into());

        //Append the built-in [FSModuleFormat]s, [SingleFileModuleFmt] and [DirModuleFmt]
        self.fs_mod_formats.push(Box::new(SingleFileModuleFmt));
        self.fs_mod_formats.push(Box::new(DirModuleFmt));

        //Wrap the fs_mod_formats in an Arc, so it can be shared with the instances of DirCatalog
        env.fs_mod_formats = Arc::new(self.fs_mod_formats);

        //Convert each proto_catalog into a real ModuleCatalog
        for proto in self.proto_catalogs.into_iter() {
            match proto {
                ProtoCatalog::Path(path) => {
                    //Make a DirCatalog for the directory
                    env.catalogs.push(Box::new(DirCatalog::new(path, env.fs_mod_formats.clone())));
                }
                ProtoCatalog::Other(catalog) => {
                    env.catalogs.push(catalog);
                }
            }
        }

        env
    }

}
