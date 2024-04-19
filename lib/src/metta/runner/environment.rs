
use std::path::{Path, PathBuf};
use std::io::Write;
use std::fs;
use std::sync::Arc;

#[cfg(feature = "pkg_mgmt")]
use crate::metta::runner::pkg_mgmt::{ModuleCatalog, DirCatalog, FsModuleFormat, SingleFileModuleFmt, DirModuleFmt, git_catalog::*};

use directories::ProjectDirs;

/// Contains state and host platform interfaces shared by all MeTTa runners.  This includes config settings
/// and logger
///
/// Generally there will be only one environment object needed, and it can be accessed by calling the [common_env] method
#[derive(Debug)]
pub struct Environment {
    config_dir: Option<PathBuf>,
    caches_dir: Option<PathBuf>,
    init_metta_path: Option<PathBuf>,
    working_dir: Option<PathBuf>,
    is_test: bool,
    #[cfg(feature = "pkg_mgmt")]
    catalogs: Vec<Box<dyn ModuleCatalog>>,
    #[cfg(feature = "pkg_mgmt")]
    pub(crate) fs_mod_formats: Arc<Vec<Box<dyn FsModuleFormat>>>,
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

    /// Returns the [Path] to the config dir, in an OS-specific location
    pub fn config_dir(&self) -> Option<&Path> {
        self.config_dir.as_deref()
    }

    /// Returns the [Path] to a directory where the MeTTa runner can put persistent caches
    ///
    /// NOTE: Currently the `caches_dir` dir is within `cfg_dir`, but there may be a reason
    ///  to move it in the future.
    pub fn caches_dir(&self) -> Option<&Path> {
        self.caches_dir.as_deref()
    }

    /// Returns the [Path] to the environment's working_dir
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
    #[cfg(feature = "pkg_mgmt")]
    pub fn catalogs<'a>(&'a self) -> impl Iterator<Item=&dyn ModuleCatalog> + 'a {
        self.catalogs.iter().map(|catalog| &**catalog as &dyn ModuleCatalog)
    }

    /// Returns the [FsModuleFormat]s from the Environment, in priority order
    #[cfg(feature = "pkg_mgmt")]
    pub fn fs_mod_formats<'a>(&'a self) -> impl Iterator<Item=&dyn FsModuleFormat> + 'a {
        self.fs_mod_formats.iter().map(|fmt| &**fmt as &dyn FsModuleFormat)
    }

    /// Private "default" function
    fn new() -> Self {
        Self {
            config_dir: None,
            caches_dir: None,
            init_metta_path: None,
            working_dir: std::env::current_dir().ok(),
            is_test: false,
            #[cfg(feature = "pkg_mgmt")]
            catalogs: vec![],
            #[cfg(feature = "pkg_mgmt")]
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
    #[cfg(feature = "pkg_mgmt")]
    proto_catalogs: Vec<ProtoCatalog>,
    #[cfg(feature = "pkg_mgmt")]
    fs_mod_formats: Vec<Box<dyn FsModuleFormat>>
}

/// Private type representing something that will become an entry in the "Environment::catalogs" Vec
#[cfg(feature = "pkg_mgmt")]
enum ProtoCatalog {
    Path(PathBuf),
    Other(Box<dyn ModuleCatalog>),
}

impl EnvBuilder {

    /// Returns a new EnvBuilder, to set the parameters for the MeTTa Environment
    ///
    /// NOTE: Unless otherwise specified, the default working directory will be the current process
    /// working dir (`cwd`)
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
            #[cfg(feature = "pkg_mgmt")]
            proto_catalogs: vec![],
            #[cfg(feature = "pkg_mgmt")]
            fs_mod_formats: vec![],
        }
    }

    /// A convenience function to construct an environment suitable for unit tests
    ///
    /// The `test_env` Environment will not load or create any files.  Additionally
    /// this method will initialize the logger for the test environment
    pub fn test_env() -> Self {
        EnvBuilder::new().set_working_dir(None).set_is_test(true).set_no_config_dir()
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
    #[cfg(feature = "pkg_mgmt")]
    pub fn push_include_path<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.proto_catalogs.push(ProtoCatalog::Path(path.as_ref().into()));
        self
    }

    /// Adds an additional [ModuleCatalog] search for MeTTa modules
    ///
    /// NOTE: The first catalog added will have the highest search priority, with subsequently added catalogs
    /// being search in order.  The `working_dir` will always be searched before any other catalogs.
    #[cfg(feature = "pkg_mgmt")]
    pub fn push_module_catalog<C: ModuleCatalog + 'static>(mut self, catalog: C) -> Self {
        self.proto_catalogs.push(ProtoCatalog::Other(Box::new(catalog)));
        self
    }

    /// Adds a [FsModuleFormat] to identify and load modules stored on file-system media
    ///
    /// This is the mechanism used to detect and load modules from the file system in foreign formats.
    /// For example a format specific to a host language such as Python
    ///
    /// NOTE: The first format added will have the highest search priority, with subsequently added formats
    /// being tried in order.  Built-in formats [SingleFileModuleFmt] and [DirModuleFmt] will be tried last.
    #[cfg(feature = "pkg_mgmt")]
    pub fn push_fs_module_format<F: FsModuleFormat + 'static>(mut self, fmt: F) -> Self {
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
    pub(crate) fn build(self) -> Environment {
        let mut env = self.env;
        #[cfg(feature = "pkg_mgmt")]
        let mut proto_catalogs = self.proto_catalogs;
        #[cfg(feature = "pkg_mgmt")]
        let mut fs_mod_formats = self.fs_mod_formats;

        //Init the logger.  This will have no effect if the logger has already been initialized
        let _ = env_logger::builder().is_test(env.is_test).try_init();

        //If we have a working_dir, make sure it gets searched first by building catalogs for it
        #[cfg(feature = "pkg_mgmt")]
        if let Some(working_dir) = &env.working_dir {
            proto_catalogs.insert(0, ProtoCatalog::Path(working_dir.into()));
        }

        //Construct the platform-specific config dir location, if an explicit location wasn't provided
        if !self.no_cfg_dir {
            if env.config_dir.is_none() {
                match ProjectDirs::from("io", "TrueAGI",  "metta") {
                    Some(proj_dirs) => {
                        let cfg_dir: PathBuf = proj_dirs.config_dir().into();
                        env.config_dir = Some(cfg_dir);
                    },
                    None => {
                        eprint!("Failed to initialize config with OS config directory!");
                    }
                }
            }
        }

        if let Some(config_dir) = &env.config_dir {

            #[cfg(feature = "pkg_mgmt")]
            let modules_dir = config_dir.join("modules");
            let init_metta_path = config_dir.join("init.metta");

            //Create the default config dir and its contents, if that part of our directive
            if self.create_cfg_dir && !config_dir.exists() {

                std::fs::create_dir_all(&config_dir).unwrap();

                #[cfg(feature = "pkg_mgmt")]
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

            // Set the caches dir within the config dir.  We may want to move it elsewhere in the future
            env.caches_dir = env.config_dir.as_ref().map(|cfg_dir| cfg_dir.join("caches"));

            //Push the "modules" dir, to search after the other paths that were specified
            //TODO: the config.metta file should be able to append / modify the catalogs, and can choose not to
            // include the "modules" dir in the future.
            #[cfg(feature = "pkg_mgmt")]
            if modules_dir.exists() {
                proto_catalogs.push(ProtoCatalog::Path(modules_dir));
            }

            if init_metta_path.exists() {
                env.init_metta_path = Some(init_metta_path);
            }
        }

        #[cfg(feature = "pkg_mgmt")]
        {
            //Append the built-in [FSModuleFormat]s, [SingleFileModuleFmt] and [DirModuleFmt]
            fs_mod_formats.push(Box::new(SingleFileModuleFmt));
            fs_mod_formats.push(Box::new(DirModuleFmt));

            //Wrap the fs_mod_formats in an Arc, so it can be shared with the instances of DirCatalog
            env.fs_mod_formats = Arc::new(fs_mod_formats);

            //Convert each proto_catalog into a real ModuleCatalog
            for proto in proto_catalogs.into_iter() {
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

            //Search the remote git-based catalog, if we have a caches dir to store the modules
            if let Some(caches_dir) = &env.caches_dir {
                //TODO: Catalog should be moved to trueagi github account, and catalog settings should come from config
                let refresh_time = 259200; //3 days = 3 days * 24 hrs * 60 minutes * 60 seconds
                env.catalogs.push(Box::new(GitCatalog::new(caches_dir, env.fs_mod_formats.clone(), "luketpeterson-catalog", "https://github.com/luketpeterson/metta-mod-catalog.git", refresh_time).unwrap()));
            }
        }

        env
    }

}
