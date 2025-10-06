//! Implements a [ModuleCatalog] serving remotely hosted modules via git
//!

use core::any::Any;
use std::path::{Path, PathBuf};
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::sync::Mutex;
use std::collections::BTreeMap;

use serde::{Serialize, Deserialize};

use crate::metta::runner::*;
use crate::metta::runner::pkg_mgmt::{*, git_cache::*};

/// A set of keys describing how to access a module via git.  Deserialized from within a [PkgInfo]
///  or a catalog file [CatalogFileFormat]
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ModuleGitLocation {
    /// Indicates that the dependency module should be fetched from the specified `git` URL
    #[serde(default)]
    pub git_url: Option<String>,

    /// A `git`` branch to fetch.  Will be ignored if `git_url` is `None`.  Uses the repo's
    /// default branch if left unspecified
    #[serde(default)]
    pub git_branch: Option<String>,

    /// A subdirectory within the git repo to use as the module, effectively ignoring the rest
    /// of the repo contents.  The subdir must be a relative path within the repo.
    #[serde(default)]
    pub git_subdir: Option<PathBuf>,

    /// A file within the git repo to use as the module.  The file path must be a relative path
    /// within the repo or `git_subdir` directory if provided.
    #[serde(default)]
    pub git_main_file: Option<PathBuf>,

    /// A path relative to the catalog repo, from which to load the module
    ///
    /// WARNING: This key will be ignored if a `git_url` is provided.
    #[serde(default)]
    pub local_path: Option<PathBuf>,
}

impl ModuleGitLocation {
    /// Fetches the module from git if it doesn't exist in `local_cache_dir`, and then returns
    /// a ModuleLoader & ModuleDescriptor pair for the module
    pub(crate) fn fetch_and_get_loader<'a, FmtIter: Iterator<Item=&'a dyn FsModuleFormat>>(&self, fmts: FmtIter, mod_name: &str, local_cache_dir: PathBuf, catalog_file_path: &Path, update_mode: UpdateMode) -> Result<Option<(Box<dyn ModuleLoader>, ModuleDescriptor)>, String> {

        //If a git URL is specified in the entry, see if we have it in the git-cache and
        // clone it locally if we don't
        if self.git_url.is_some() {
            let cached_repo = self.get_cache(mod_name, local_cache_dir)?;
            cached_repo.update(update_mode)?;

            let mod_path = match &self.git_main_file {
                Some(main_file) => cached_repo.local_path().join(main_file),
                None => cached_repo.local_path().to_owned(),
            };
            return loader_for_module_at_path(fmts, &mod_path, Some(mod_name), None);
        }

        //If a `local_path` was specified, then load the module from there
        match &self.local_path {
            Some(local_path) => {
                let mod_path = catalog_file_path.parent().unwrap().join(local_path);
                return loader_for_module_at_path(fmts, &mod_path, Some(mod_name), None);
            },
            None => {}
        }

        Ok(None)
    }
    pub(crate) fn get_cache(&self, mod_name: &str, local_cache_dir: PathBuf) -> Result<CachedRepo, String> {
        let url = self.git_url.as_ref().unwrap();
        let branch = self.git_branch.as_ref().map(|s| s.as_str());
        let subdir = self.git_subdir.as_ref().map(|p| p.as_path());
        CachedRepo::new(mod_name, local_cache_dir, url, branch, subdir)
    }
    pub(crate) fn uid(&self) -> u64 {
        let subdir_string;
        let subdir_str = if let Some(p) = &self.git_subdir {
            subdir_string = format!("{p:?}");
            subdir_string.as_str()
        } else {""};
        let main_file_string;
        let main_file_str = if let Some(p) = &self.git_main_file {
            main_file_string = format!("{p:?}");
            main_file_string.as_str()
        } else {""};
        let unique_string = format!("{}-{}-{subdir_str}-{main_file_str}",
            self.git_url.as_ref().map(|s| s.as_str()).unwrap_or(""),
            self.git_branch.as_ref().map(|s| s.as_str()).unwrap_or(""),
        );
        ModuleDescriptor::uid_from_ident_bytes_and_fmt_id(unique_string.as_bytes(), 0)
    }
    /// Returns a new ModuleGitLocation.  This is a convenience; the usual interface involves deserializing this struct
    pub(crate) fn new(url: String) -> Self {
        let mut new_self = Self::default();
        new_self.git_url = Some(url);
        new_self
    }
    pub(crate) fn get_url(&self) -> Option<&str> {
        self.git_url.as_ref().map(|s| s.as_str())
    }
}

/// Struct that matches the catalog.json file fetched from the `_catalog.repo`
#[derive(Serialize, Deserialize, Debug, Default)]
struct CatalogFileFormat {
    modules: BTreeMap<String, Vec<CatalogFileMod>>
}

impl CatalogFileFormat {
    fn find_mods_with_name(&self, name: &str) -> Vec<ModuleDescriptor> {
        let mut results = vec![];
        if let Some(cat_mod_vec) = self.modules.get(name) {
            for cat_mod in cat_mod_vec {
                let uid = cat_mod.git_location.uid();
                let descriptor = ModuleDescriptor::new(name.to_string(), cat_mod.version.clone(), Some(uid));
                results.push(descriptor);
            }
        }
        results
    }
    /// Scans the catalog looking for a single module that matches the provided descriptor
    fn find_mod_with_descriptor(&self, descriptor: &ModuleDescriptor) -> Option<&CatalogFileMod> {
        if let Some(cat_mod_vec) = self.modules.get(descriptor.name()) {
            for cat_mod in cat_mod_vec.iter() {
                if cat_mod.version.as_ref() == descriptor.version() {
                    //NOTE ModuleDescriptors from GitCatalog always have a uid
                    if Some(cat_mod.git_location.uid()) == descriptor.uid() {
                        return Some(cat_mod);
                    }
                }
            }
        }
        None
    }
    fn add(&mut self, new_mod: CatalogFileMod) -> Result<ModuleDescriptor, String> {
        let uid = new_mod.git_location.uid();
        let descriptor = ModuleDescriptor::new(new_mod.name.clone(), new_mod.version.clone(), Some(uid));
        if self.find_mod_with_descriptor(&descriptor).is_none() {
            let cat_mod_vec = self.modules.entry(new_mod.name.clone()).or_insert(vec![]);
            cat_mod_vec.push(new_mod);
        }
        Ok(descriptor)
    }
}

/// A single module in a catalog.json file
#[derive(Clone, Debug, Serialize, Deserialize)]
struct CatalogFileMod {
    name: String,
    #[serde(default)]
    version: Option<semver::Version>,
    #[serde(flatten)]
    git_location: ModuleGitLocation,
}

impl CatalogFileMod {
    fn new(name: String, version: Option<semver::Version>, git_location: ModuleGitLocation) -> Self {
        Self {name, version, git_location}
    }
}

/// Provides an interface to a git repo hosting a table of available modules
#[derive(Debug)]
pub struct GitCatalog {
    /// The name of this catalog
    name: String,
    /// The FsModuleFormats from the environment, to load the modules from their respective repositories
    fmts: Arc<Vec<Box<dyn FsModuleFormat>>>,
    /// An interval in seconds to control the refresh of the catalog from the upstream source
    refresh_time: u64,
    /// The git repo for the catalog info.  This is the table-of-contents for the catalog, not the modules
    catalog_repo: Option<CachedRepo>,
    /// A path to a directory where this catalog may cache local files.  When a `GitCatalog` is upstream of
    /// a [LocalCatalog], this directory will not contain modules, but modules will be stored here when the
    /// `GitCatalog` is used by itself
    caches_dir: PathBuf,
    /// The path to the catalog file(s), to store the metadata to connect a module to its source location
    /// parameters.  This path does not have any reliable connection to the on-disk location of the modules
    catalog_file_path: PathBuf,
    /// The in-memory catalog object, mirroring what is on disk
    catalog: Mutex<Option<CatalogFileFormat>>,
}

impl GitCatalog {
    fn new_internal(fmts: Arc<Vec<Box<dyn FsModuleFormat>>>, name: &str, caches_dir: PathBuf, catalog_file_path: PathBuf, catalog: Option<CatalogFileFormat>) -> Self {
        Self {
            name: name.to_string(),
            fmts,
            refresh_time: 0,
            catalog_repo: None,
            caches_dir,
            catalog_file_path,
            catalog: Mutex::new(catalog),
        }
    }
    /// Creates a new GitCatalog with the name and url specified.  `refresh_time` is the time, in
    /// seconds, between refreshes of the catalog file
    pub fn new(caches_dir: &Path, fmts: Arc<Vec<Box<dyn FsModuleFormat>>>, name: &str, url: &str, refresh_time: u64) -> Result<Self, String> {
        let caches_dir = caches_dir.join(name);
        let catalog_repo_dir = caches_dir.join("_catalog.repo");
        let catalog_repo_name = format!("{name}-catalog.repo");
        let catalog_repo = CachedRepo::new(&catalog_repo_name, catalog_repo_dir, url, None, None)?;
        let mut new_self = Self::new_internal(fmts, name, caches_dir, catalog_repo.local_path().join("catalog.json"), None);
        new_self.refresh_time = refresh_time;
        new_self.catalog_repo = Some(catalog_repo);
        Ok(new_self)
    }
    /// Used for a git-based catalog that isn't synced to a remote source
    ///
    /// The primary use of this method is to make a `GitCatalog` to back the `git-module!` MeTTa operation,
    /// so it will track the locations of explicitly sourced modules.  It's a fairly special-purpose object
    /// designed to associate git information (repo url, branch, etc.) with some modules available through a
    /// [LocalCatalog], so the user can "install" modules by fetching them from wherever, and then pull the
    /// latest version without needing to re-supply the details about where to fetch the module from.
    pub fn new_without_source_repo(caches_dir: &Path, fmts: Arc<Vec<Box<dyn FsModuleFormat>>>, name: &str) -> Result<Self, String> {
        let caches_dir = caches_dir.join(name);
        let catalog_file_path = caches_dir.join("_catalog.json");
        let new_self = Self::new_internal(fmts, name, caches_dir, catalog_file_path, Some(CatalogFileFormat::default()));
        if new_self.catalog_file_path.exists() {
            new_self.parse_catalog()?
        } else {
            new_self.write_catalog()?;
        }
        Ok(new_self)
    }
    /// Registers a new module in the catalog with a specified remote location, and returns
    /// the [ModuleDescriptor] to refer to that module
    ///
    /// WARNING: This method is incompatible with a catalog synced to an upstream source
    pub(crate) fn register_mod(&self, mod_name: &str, version: Option<&semver::Version>, git_location: &ModuleGitLocation) -> Result<ModuleDescriptor, String> {
        if self.catalog_repo.is_some() {
            return Err(format!("cannot explicitly register module in a catalog synced to an upstream source"));
        }
        let descriptor = {
            let mut catalog_ref = self.catalog.lock().unwrap();
            catalog_ref.as_mut().unwrap().add(CatalogFileMod::new(mod_name.to_string(), version.cloned(), git_location.clone()))?
        };
        self.write_catalog()?;
        Ok(descriptor)
    }
    /// Scans the catalog and finds all the modules with a given name
    fn find_mods_with_name(&self, name: &str) -> Vec<ModuleDescriptor> {
        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        catalog.find_mods_with_name(name)
    }
    fn refresh_catalog(&self, update_mode: UpdateMode) -> Result<(), String> {
        if let Some(catalog_repo) = &self.catalog_repo {
            //Update the catalog from the git cache
            let did_update = match catalog_repo.update(update_mode) {
                Ok(did_update) => did_update,
                Err(e) => {
                    log::warn!("Warning: error encountered attempting to fetch remote catalog: {}, {e}", self.name);
                    false
                }
            };

            //Parse the catalog JSON file if we need to
            if did_update || self.catalog_is_uninit() {
                self.parse_catalog()?;
            }
        }
        Ok(())
    }
    fn catalog_is_uninit(&self) -> bool {
        let catalog = self.catalog.lock().unwrap();
        catalog.is_none()
    }
    fn parse_catalog(&self) -> Result<(), String> {
        let mut catalog = self.catalog.lock().unwrap();
        match File::open(&self.catalog_file_path) {
            Ok(file) => {
                let reader = BufReader::new(file);
                *catalog = Some(serde_json::from_reader(reader).unwrap());
                Ok(())
            },
            Err(e) => {
                Err(format!("Error reading catalog file. remote catalog unavailable: {}, {e}", self.name))
            }
        }
    }
    /// Writes the catalog to a file, overwriting the file that is currently on disk
    fn write_catalog(&self) -> Result<(), String> {
        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        let file = File::create(&self.catalog_file_path).map_err(|e| e.to_string())?;
        let writer = BufWriter::new(file);
        serde_json::to_writer(writer, catalog).map_err(|e| e.to_string())?;
        Ok(())
    }
}

impl ModuleCatalog for GitCatalog {
    fn display_name(&self) -> String {
        self.name.clone()
    }
    fn lookup(&self, name: &str) -> Vec<ModuleDescriptor> {
        match self.refresh_catalog(UpdateMode::TryFetchIfOlderThan(self.refresh_time)) {
            Ok(_) => {},
            Err(e) => {
                log::warn!("{e}");
                return vec![]
            }
        }

        //Find the modules that match in the catalog
        self.find_mods_with_name(name)
    }
    fn get_loader(&self, descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String> {
        self.refresh_catalog(UpdateMode::TryFetchIfOlderThan(self.refresh_time))?;

        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        let module = catalog.find_mod_with_descriptor(descriptor)
            .ok_or_else(|| format!("Error: module {descriptor} no longer exists in catalog {}", self.display_name()))?;

        let mod_dir_name = super::managed_catalog::dir_name_from_descriptor(descriptor);
        let fallback_mod_dir = self.caches_dir.join(mod_dir_name);

        Ok(Box::new(GitModLoader{
            module: module.clone(),
            fmts: self.fmts.clone(),
            fallback_mod_dir,
            catalog_file_path: self.catalog_file_path.clone(),
        }))
    }
    fn sync_toc(&self, update_mode: UpdateMode) -> Result<(), String> {
        self.refresh_catalog(update_mode)?;
        Ok(())
    }
    fn as_any(&self) -> Option<&dyn Any> {
        Some(self as &dyn Any)
    }
}

#[derive(Debug)]
pub struct GitModLoader {
    module: CatalogFileMod,
    fmts: Arc<Vec<Box<dyn FsModuleFormat>>>,
    /// A local path to clone the module into, if a path isn't provided by a downstream LocalCatalog
    fallback_mod_dir: PathBuf,
    /// The location of the catalog file from which the `module` field was deserialized
    catalog_file_path: PathBuf,
}

impl ModuleLoader for GitModLoader {
    fn prepare(&self, local_dir: Option<&Path>, update_mode: UpdateMode) -> Result<Option<Box<dyn ModuleLoader>>, String> {
        let local_dir = match local_dir {
            Some(local_dir) => local_dir,
            None => &self.fallback_mod_dir
        };
        let loader = match self.module.git_location.fetch_and_get_loader(self.fmts.iter().map(|f| &**f), &self.module.name, local_dir.to_owned(), &self.catalog_file_path, update_mode)? {
            Some((loader, _)) => loader,
            None => unreachable!(),
        };
        Ok(Some(loader))
    }
    fn load(&self, _context: &mut RunContext) -> Result<(), String> {
        unreachable!()
    }
}

/// This test is similar to `git_remote_catalog_fetch_test` in [crate::metta::runner::pkg_mgmt::catalog],
/// but tests the `GitCatalog` without a `LocalCatalog` and also loads a local catalog so
/// the test can run without a network connection.

#[test]
#[cfg(feature="online-test")]
fn git_catalog_direct_test() {
    let gitcat = GitCatalog::new(&std::env::temp_dir(), EnvBuilder::test_env().build().fs_mod_formats, "metta-catalog", "https://github.com/trueagi-io/metta-catalog/",0).unwrap();
    let runner = Metta::new(Some(EnvBuilder::test_env().push_module_catalog(gitcat).set_caches_dir(&std::env::temp_dir().join("hyperon-test"))));

    let result = runner.run(SExprParser::new("!(import! &self example)"));
    assert_eq!(result, Ok(vec![vec![expr!()]]));

    runner.display_loaded_modules();

    let result = runner.run(SExprParser::new("!(fact 5)"));
    assert_eq!(result, Ok(vec![vec![expr!({hyperon_atom::gnd::number::Number::Integer(120)})]]));
}

