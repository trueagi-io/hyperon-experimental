//! Implements a [ModuleCatalog] serving remotely hosted modules via git
//!

use core::any::Any;
use std::path::{Path, PathBuf};
use std::fs::read_to_string;
use std::sync::Mutex;

use serde::Deserialize;

use crate::metta::runner::*;
use crate::metta::runner::pkg_mgmt::{*, git_cache::*};

/// A set of keys describing how to access a module via git.  Deserialized from within a [PkgInfo]
///  or a catalog file [CatalogFileFormat]
#[derive(Clone, Debug, Default, Deserialize)]
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
}

impl ModuleGitLocation {
    /// Fetches the module from git if it doesn't exist in `local_cache_dir`, and then returns
    /// a ModuleLoader & ModuleDescriptor pair for the module
    pub(crate) fn fetch_and_get_loader<'a, FmtIter: Iterator<Item=&'a dyn FsModuleFormat>>(&self, fmts: FmtIter, mod_name: &str, local_cache_dir: PathBuf, update_mode: UpdateMode) -> Result<Option<(Box<dyn ModuleLoader>, ModuleDescriptor)>, String> {

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

        Ok(None)
    }
    /// Gets a loader for a module identified by a ModuleGitLocation, using the [Environment]'s managed `explicit_git_mods` catalog
    pub(crate) fn get_loader_in_explicit_catalog(&self, mod_name: &str, should_refresh: bool, env: &Environment) -> Result<Option<(Box<dyn ModuleLoader>, ModuleDescriptor)>, String> {
        if self.get_url().is_some() {
            if let Some(explicit_git_catalog) = env.explicit_git_mods.as_ref() {
                let descriptor = explicit_git_catalog.upstream_catalogs().first().unwrap().downcast::<GitCatalog>().unwrap().register_mod(mod_name, None, self)?;
                let loader = explicit_git_catalog.get_loader_with_explicit_refresh(&descriptor, should_refresh)?;
                Ok(Some((loader, descriptor)))
            } else {
                Err(format!("Unable to pull module \"{mod_name}\" from git; no local \"caches\" directory available"))
            }
        } else {
            Ok(None)
        }
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

/// Struct that matches the catalog.json file fetched from the `catalog.repo`
#[derive(Deserialize, Debug, Default)]
struct CatalogFileFormat {
    //TODO-NOW.  Upon reflection, I see no good reason not to use a HashMap here instead of a Vec
    modules: Vec<CatalogFileMod>
}

impl CatalogFileFormat {
    fn find_mods_with_name(&self, name: &str) -> Vec<ModuleDescriptor> {
        let mut results = vec![];
        for cat_mod in self.modules.iter() {
            if cat_mod.name == name {
                let uid = cat_mod.git_location.uid();
                let descriptor = ModuleDescriptor::new(name.to_string(), cat_mod.version.clone(), Some(uid));
                results.push(descriptor);
            }
        }
        results
    }
    fn find_mod_idx_with_descriptor(&self, descriptor: &ModuleDescriptor) -> Option<usize> {
        for (mod_idx, cat_mod) in self.modules.iter().enumerate() {
            if cat_mod.name == descriptor.name() && cat_mod.version.as_ref() == descriptor.version() {
                if Some(cat_mod.git_location.uid()) == descriptor.uid() {
                    return Some(mod_idx);
                }
            }
        }
        None
    }
    fn add(&mut self, new_mod: CatalogFileMod) -> Result<ModuleDescriptor, String> {
        let uid = new_mod.git_location.uid();
        let descriptor = ModuleDescriptor::new(new_mod.name.clone(), new_mod.version.clone(), Some(uid));
        if self.find_mod_idx_with_descriptor(&descriptor).is_none() {
            self.modules.push(new_mod);
        }
        Ok(descriptor)
    }
}

/// A single module in a catalog.json file
#[derive(Clone, Deserialize, Debug)]
struct CatalogFileMod {
    name: String,
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
    name: String,
    fmts: Arc<Vec<Box<dyn FsModuleFormat>>>,
    refresh_time: u64,
    catalog_repo: Option<CachedRepo>,
    catalog: Mutex<Option<CatalogFileFormat>>,
}

impl GitCatalog {
    /// Creates a new GitCatalog with the name and url specified.  `refresh_time` is the time, in
    /// seconds, between refreshes of the catalog file
    pub fn new(caches_dir: &Path, fmts: Arc<Vec<Box<dyn FsModuleFormat>>>, name: &str, url: &str, refresh_time: u64) -> Result<Self, String> {
        let catalog_repo_dir = caches_dir.join(name).join("catalog.repo");
        let catalog_repo_name = format!("{name}-catalog.repo");
        let catalog_repo = CachedRepo::new(&catalog_repo_name, catalog_repo_dir, url, None, None)?;
        let mut new_self = Self::new_without_source_repo(fmts, name)?;
        new_self.refresh_time = refresh_time;
        new_self.catalog_repo = Some(catalog_repo);
        Ok(new_self)
    }
    /// Used for a git-based catalog that isn't synced to a remote source
    pub fn new_without_source_repo(fmts: Arc<Vec<Box<dyn FsModuleFormat>>>, name: &str) -> Result<Self, String> {
        Ok(Self {
            name: name.to_string(),
            fmts,
            refresh_time: 0,
            catalog_repo: None,
            catalog: Mutex::new(None),
        })
    }
    /// Registers a new module in the catalog with a specified remote location, and returns the [ModuleDescriptor] to refer to that module
    pub(crate) fn register_mod(&self, mod_name: &str, version: Option<&semver::Version>, git_location: &ModuleGitLocation) -> Result<ModuleDescriptor, String> {
        self.refresh_catalog()?;
        let mut catalog_ref = self.catalog.lock().unwrap();
        let descriptor = catalog_ref.as_mut().unwrap().add(CatalogFileMod::new(mod_name.to_string(), version.cloned(), git_location.clone()))?;
        Ok(descriptor)
    }
    /// Scans the catalog and finds all the modules with a given name
    fn find_mods_with_name(&self, name: &str) -> Vec<ModuleDescriptor> {
        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        catalog.find_mods_with_name(name)
    }
    /// Scans the catalog looking for a single module that matches the provided descriptor
    fn find_mod_idx_with_descriptor(&self, descriptor: &ModuleDescriptor) -> Option<usize> {
        let cat_lock = self.catalog.lock().unwrap();
        match &*cat_lock {
            Some(catalog) => catalog.find_mod_idx_with_descriptor(descriptor),
            None => None
        }
    }
    fn refresh_catalog(&self) -> Result<bool, String> {
        if let Some(catalog_repo) = &self.catalog_repo {
            //Get the catalog from the git cache
            let did_update = match catalog_repo.update(UpdateMode::TryPullIfOlderThan(self.refresh_time)) {
                Ok(did_update) => did_update,
                Err(e) => {
                    log::warn!("Warning: error encountered attempting to fetch remote catalog: {}, {e}", self.name);
                    false
                }
            };

            //Parse the catalog JSON file if we need to
            let mut catalog = self.catalog.lock().unwrap();
            if did_update || catalog.is_none() {
                let catalog_file_path = catalog_repo.local_path().join("catalog.json");
                match read_to_string(&catalog_file_path) {
                    Ok(file_contents) => {
                        *catalog = Some(serde_json::from_str(&file_contents).unwrap());
                        return Ok(true)
                    },
                    Err(e) => {
                        return Err(format!("Error reading catalog file. remote catalog unavailable: {}, {e}", self.name))
                    }
                }
            }
        } else {
            let mut catalog = self.catalog.lock().unwrap();
            if catalog.is_none() {
                *catalog = Some(CatalogFileFormat::default());
            }
        }
        Ok(false)
    }
}

impl ModuleCatalog for GitCatalog {
    fn display_name(&self) -> String {
        self.name.clone()
    }
    fn lookup(&self, name: &str) -> Vec<ModuleDescriptor> {
        match self.refresh_catalog() {
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
        self.refresh_catalog()?;

        let mod_idx = self.find_mod_idx_with_descriptor(descriptor).unwrap();

        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        let module = catalog.modules.get(mod_idx).unwrap();

        Ok(Box::new(GitModLoader{
            module: module.clone(),
            fmts: self.fmts.clone(),
        }))
    }
    fn as_any(&self) -> Option<&dyn Any> {
        Some(self as &dyn Any)
    }
}

#[derive(Debug)]
pub struct GitModLoader {
    module: CatalogFileMod,
    fmts: Arc<Vec<Box<dyn FsModuleFormat>>>,
}

impl ModuleLoader for GitModLoader {
    fn prepare(&self, local_dir: Option<&Path>, should_refresh: bool) -> Result<Option<Box<dyn ModuleLoader>>, String> {
        let update_mode = match should_refresh {
            true => UpdateMode::TryPullLatest,
            false => UpdateMode::PullIfMissing
        };
        let local_dir = match local_dir {
            Some(local_dir) => local_dir,
            None => return Err("GitCatalog: Cannot prepare git-based module without local cache directory".to_string())
        };
        let loader = match self.module.git_location.fetch_and_get_loader(self.fmts.iter().map(|f| &**f), &self.module.name, local_dir.to_owned(), update_mode)? {
            Some((loader, _)) => loader,
            None => unreachable!(),
        };
        Ok(Some(loader))
    }
    fn load(&self, _context: &mut RunContext) -> Result<(), String> {
        unreachable!()
    }
}


//TODO-NOW Add some status output when modules are fetched from GIT
//TODO-NOW implement the managed catalog trait on the local catalog
//TODO-NOW implement ops to manage the catalog
//TODO-NOW Implement a MeTTaMod that separates apart the catalog management functions
//TODO-NOW Implement a builtin-catalog for acccess to std mods
//TODO-NOW Fix the build without pkg_mgmt feature
//TODO-NOW Get the repl to immeditately display logs of warn!() or err!()
