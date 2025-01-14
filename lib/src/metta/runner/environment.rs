
use std::path::{Path, PathBuf};
use std::io::{Read, BufReader, Write};
use std::fs;
use std::sync::Arc;

use crate::{sym, ExpressionAtom, metta::GroundingSpace};

#[cfg(feature = "pkg_mgmt")]
use crate::metta::runner::pkg_mgmt::{ModuleCatalog, DirCatalog, LocalCatalog, FsModuleFormat, SingleFileModuleFmt, DirModuleFmt, git_catalog::*};

use directories::ProjectDirs;

/// Contains state and host platform interfaces shared by all MeTTa runners.  This includes config settings
/// and logger
///
/// Generally there will be only one environment object needed, and it can be accessed by calling the [Self::common_env] method
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
    /// The store for modules cached locally after loading from a specific location, for example, via git.
    #[cfg(feature = "pkg_mgmt")]
    pub(crate) specified_mods: Option<LocalCatalog>,
}

const DEFAULT_INIT_METTA: &[u8] = include_bytes!("init.default.metta");
const DEFAULT_ENVIRONMENT_METTA: &[u8] = include_bytes!("environment.default.metta");

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
            #[cfg(feature = "pkg_mgmt")]
            specified_mods: None,
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
    /// NOTE: Unless otherwise specified by calling either [Self::set_no_config_dir] or [Self::set_config_dir], the
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
            create_cfg_dir: true,
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

    /// Sets the `config_dir` that the environment will load
    pub fn set_config_dir(mut self, config_dir: &Path) -> Self {
        self.env.config_dir = Some(config_dir.into());
        if self.no_cfg_dir {
            panic!("Fatal Error: set_config_dir is incompatible with set_no_config_dir");
        }
        self
    }

    /// Sets whether or not a config directory with default config files will be created, if no directory is found
    ///
    /// NOTE: If the config directory exists but some config files are missing, default files will *not* be created.
    pub fn set_create_config_dir(mut self, should_create: bool) -> Self {
        self.create_cfg_dir = should_create;
        if self.no_cfg_dir && should_create {
            panic!("Fatal Error: set_create_config_dir(true) is incompatible with set_no_config_dir");
        }
        self
    }

    /// Configures the Environment not to load nor create any config files
    pub fn set_no_config_dir(mut self) -> Self {
        self.no_cfg_dir = true;
        self.create_cfg_dir = false;
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

    /// Initializes the shared common Environment, accessible with [Environment::common_env]
    ///
    /// NOTE: This method will panic if the common Environment has already been initialized
    pub fn init_common_env(self) {
        self.try_init_common_env().expect("Fatal Error: Common Environment already initialized");
    }

    /// Initializes the shared common Environment.  Non-panicking version of [Self::init_common_env]
    pub fn try_init_common_env(self) -> Result<(), &'static str> {
        COMMON_ENV.set(Arc::new(self.build())).map_err(|_| "Common Environment already initialized")
    }

    /// Returns a newly created Environment from the builder configuration
    ///
    /// NOTE: Creating owned Environments is usually not necessary.  It is usually sufficient to use the [Environment::common_env] method.
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

            let init_metta_path = config_dir.join("init.metta");

            //Create the default config dir and its contents, if that part of our directive
            if self.create_cfg_dir && !config_dir.exists() {

                std::fs::create_dir_all(&config_dir).unwrap();

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
        }

        if let Some(config_dir) = &env.config_dir {
            let env_metta_path = config_dir.join("environment.metta");

            //Create the default environment.metta file if it doesn't exist
            if !env_metta_path.exists() {
                let mut file = fs::OpenOptions::new()
                    .create(true)
                    .write(true)
                    .open(&env_metta_path)
                    .expect(&format!("Error creating default environment config file at {env_metta_path:?}"));
                file.write_all(&DEFAULT_ENVIRONMENT_METTA).unwrap();
            }

            interpret_environment_metta(env_metta_path, &mut env).unwrap_or_else(|e| {
                log::warn!("Error occurred interpreting environment.metta file: {e}");
            });
        }

        #[cfg(feature = "pkg_mgmt")]
        {
            //If we have a caches dir to cache modules locally then register remote catalogs
            if let Some(caches_dir) = &env.caches_dir {

                //Setup the specified_mods managed catalog to hold mods fetched by explicit means
                let mut specified_mods = LocalCatalog::new(caches_dir, "specified-mods").unwrap();
                let git_mod_catalog = GitCatalog::new_without_source_repo(caches_dir, env.fs_mod_formats.clone(), "specified-mods").unwrap();
                specified_mods.push_upstream_catalog(Box::new(git_mod_catalog));
                env.specified_mods = Some(specified_mods);
            }
        }

        env
    }

}

/// Interprets the file at `env_metta_path`, and modifies settings in the Environment
///
/// NOTE: I wonder if users will get confused by the fact that the full set of runner
/// features aren't available in the environment.metta file.  But there is a bootstrapping
/// problem trying to using a runner here
fn interpret_environment_metta<P: AsRef<Path>>(env_metta_path: P, env: &mut Environment) -> Result<(), String> {
    let file = fs::File::open(env_metta_path).map_err(|e| e.to_string())?;
    let mut buf_reader = BufReader::new(file);
    let mut file_contents = String::new();
    buf_reader.read_to_string(&mut file_contents).map_err(|e| e.to_string())?;

    let space = GroundingSpace::new();
    let tokenizer = crate::metta::runner::Tokenizer::new();
    let mut parser = crate::metta::runner::SExprParser::new(&file_contents);
    while let Some(atom) = parser.parse(&tokenizer)? {
        let atoms = crate::metta::runner::interpret(&space, &atom)?;
        let atom = if atoms.len() != 1 {
            return Err(format!("Error in environment.metta. Atom must evaluate into a single deterministic result.  Found {atoms:?}"));
        } else {
            atoms.into_iter().next().unwrap()
        };

        //TODO-FUTURE: Use atom-serde here to cut down on boilerplate from interpreting these atoms
        let expr = ExpressionAtom::try_from(atom)?;
        match expr.children().get(0) {
            Some(atom_0) if *atom_0 == sym!("#includePath") => {
                #[cfg(feature = "pkg_mgmt")]
                env.catalogs.push(include_path_from_cfg_atom(&expr, env)?);
                #[cfg(not(feature = "pkg_mgmt"))]
                {
                    let _ = &env;
                    log::warn!("#includePath in environment.metta not supported without pkg_mgmt feature");
                }
            },
            Some(atom_0) if *atom_0 == sym!("#gitCatalog") => {
                #[cfg(feature = "pkg_mgmt")]
                env.catalogs.push(git_catalog_from_cfg_atom(&expr, env)?);
                #[cfg(not(feature = "pkg_mgmt"))]
                log::warn!("#gitCatalog in environment.metta not supported without pkg_mgmt feature");
            },
            _ => return Err(format!("Error in environment.metta. Unrecognized setting: {expr:?}"))
        }
    }
    Ok(())
}

#[cfg(feature = "pkg_mgmt")]
fn git_catalog_from_cfg_atom(atom: &ExpressionAtom, env: &Environment) -> Result<Box<dyn ModuleCatalog>, String> {

    let mut catalog_name = None;
    let mut catalog_url = None;
    let mut refresh_time = None;

    let mut atom_iter = atom.children().iter();
    let _ = atom_iter.next();
    for atom in atom_iter {
        let expr = <&ExpressionAtom>::try_from(atom)?;
        if expr.children().len() < 1 {
            continue;
        }
        let key_atom = expr.children().get(0).unwrap();
        let val_atom = match expr.children().get(1) {
            Some(atom) => atom,
            None => return Err(format!("Error in environment.metta. Key without value: {key_atom}"))
        };

        match key_atom {
            _ if *key_atom == sym!("#name") => catalog_name = Some(<&crate::SymbolAtom>::try_from(val_atom)?.name()),
            _ if *key_atom == sym!("#url") => catalog_url = Some(<&crate::SymbolAtom>::try_from(val_atom)?.name()),
            _ if *key_atom == sym!("#refreshTime") => refresh_time = Some(<&crate::SymbolAtom>::try_from(val_atom)?.name()),
            _ => return Err(format!("Error in environment.metta. Unknown key: {key_atom}"))
        }
    }

    let caches_dir = env.caches_dir.as_ref().unwrap();
    let catalog_name = catalog_name.ok_or_else(|| format!("Error in environment.metta. \"name\" property required for #gitCatalog"))?;
    let catalog_url = catalog_url.ok_or_else(|| format!("Error in environment.metta. \"url\" property required for #gitCatalog"))?;
    let refresh_time = refresh_time.ok_or_else(|| format!("Error in environment.metta. \"refreshTime\" property required for #gitCatalog"))?
        .parse::<u64>().map_err(|e| format!("Error in environment.metta.  Error parsing \"refreshTime\": {e}"))?;

    let mut managed_remote_catalog = LocalCatalog::new(caches_dir, catalog_name).unwrap();
    let remote_catalog = GitCatalog::new(caches_dir, env.fs_mod_formats.clone(), catalog_name, catalog_url, refresh_time).unwrap();
    managed_remote_catalog.push_upstream_catalog(Box::new(remote_catalog));
    Ok(Box::new(managed_remote_catalog))
}

#[cfg(feature = "pkg_mgmt")]
fn include_path_from_cfg_atom(atom: &ExpressionAtom, env: &Environment) -> Result<Box<dyn ModuleCatalog>, String> {

    let mut atom_iter = atom.children().iter();
    let _ = atom_iter.next();
    let path_atom = match atom_iter.next() {
        Some(atom) => atom,
        None => return Err(format!("Error in environment.metta. #includePath missing path value"))
    };
    let path = <&crate::SymbolAtom>::try_from(path_atom)?.name();

    //TODO-FUTURE: In the future we may want to replace dyn-fmt with strfmt, and do something a
    // little bit nicer than this
    let path = match path.strip_prefix("{$cfgdir}/") {
        Some(rel_path) => env.config_dir().unwrap().join(rel_path),
        None => PathBuf::from(path)
    };

    if !path.exists() {
        log::info!("Creating search directory for modules: \"{}\"", path.display());
        std::fs::create_dir_all(&path).map_err(|e| e.to_string())?;
    }

    Ok(Box::new(DirCatalog::new(path, env.fs_mod_formats.clone())))
}
