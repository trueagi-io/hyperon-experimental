//! Implements a [ModuleCatalog] serving remotely hosted modules via git
//!

use std::path::{Path, PathBuf};
use std::fs::read_to_string;
use std::sync::Mutex;

use serde::Deserialize;

use crate::metta::runner::*;
use crate::metta::runner::pkg_mgmt::{*, git_cache::*};

//TODO:
// * Need a function to clean up local repos that have been removed from the catalog file
// * Need a function to delete a whole catalog cache.  Both of these interfaces should probably
//    be added to the catalog trait as optional methods.
// * Funtion to trigger explicit updates.  Accessible from metta ops
//   - Update specific module, update to a specific version, latest, or latest stable
//   - update all modules, to latest or latest stable
//   - implemented in a way that also works on the EXPLICIT_GIT_MOD_CACHE (e.g. by cache dir)

/// The name of the cache for modules loaded explicitly by git URL
pub(crate) const EXPLICIT_GIT_MOD_CACHE: &'static str = "git-modules";

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
    /// Returns a ModuleLoader & ModuleDescriptor pair for a module hosted at a specific git location.
    /// Checks the cache to avoid unnecessaryily fetching the module if we have it locally
    pub(crate) fn get_loader<'a, FmtIter: Iterator<Item=&'a dyn FsModuleFormat>>(&self, fmts: FmtIter, caches_dir: Option<&Path>, cache_name: &str, mod_name: &str, version: Option<&semver::Version>, ident_str: Option<&str>) -> Result<Option<(Box<dyn ModuleLoader>, ModuleDescriptor)>, String> {

        //If a git URL is specified in the entry, see if we have it in the git-cache and
        // clone it locally if we don't
        if self.git_url.is_some() {
            let cached_repo = self.get_cache(caches_dir, cache_name, mod_name, version, ident_str)?;
            //TODO We want "version-locking" behavior from this cache.  ie. don't update once
            // we pulled a version, but we need a way to cause it to pull the latest.
            //The tricky part is how the user will specify the modules.  Doing by url sounds tedious.
            // but doing it by mod_name might update modules in unwanted ways.
            //At the very least, we need to offer a "update everything" command that can run across
            // an entire cache
            cached_repo.update(UpdateMode::PullIfMissing)?;

            let mod_path = match &self.git_main_file {
                Some(main_file) => cached_repo.local_path().join(main_file),
                None => cached_repo.local_path().to_owned(),
            };
            return loader_for_module_at_path(fmts, &mod_path, Some(mod_name), None);
        }

        Ok(None)
    }
    pub(crate) fn get_cache(&self, caches_dir: Option<&Path>, cache_name: &str, mod_name: &str, version: Option<&semver::Version>, ident_str: Option<&str>) -> Result<CachedRepo, String> {
        let caches_dir = caches_dir.ok_or_else(|| "Unable to clone git repository; no local \"caches\" directory available".to_string())?;
        let url = self.git_url.as_ref().unwrap();
        let ident_str = match ident_str {
            Some(ident_str) => ident_str,
            None => url,
        };
        let repo_name_string;
        let mod_repo_name = match version {
            Some(version) => {
                repo_name_string = format!("{mod_name}-{version}");
                &repo_name_string
            },
            None => mod_name
        };
        CachedRepo::new(caches_dir, cache_name, mod_repo_name, ident_str, url, self.git_branch.as_ref().map(|s| s.as_str()), self.git_subdir.as_ref().map(|p| p.as_path()))
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
#[derive(Deserialize, Debug)]
struct CatalogFileFormat {
    modules: Vec<CatalogFileMod>
}

/// A single module in a catalog.json file
#[derive(Deserialize, Debug)]
struct CatalogFileMod {
    name: String,
    version: Option<semver::Version>,
    #[serde(flatten)]
    git_location: ModuleGitLocation,
}

#[derive(Debug)]
pub struct GitCatalog {
    name: String,
    caches_dir: PathBuf,
    fmts: Arc<Vec<Box<dyn FsModuleFormat>>>,
    refresh_time: u64,
    catalog_repo: CachedRepo,
    catalog: Mutex<Option<CatalogFileFormat>>,
}

impl GitCatalog {
    /// Creates a new GitCatalog with the name and url specified.  `refresh_time` is the time, in
    /// seconds, between refreshes of the catalog file
    pub fn new(caches_dir: &Path, fmts: Arc<Vec<Box<dyn FsModuleFormat>>>, name: &str, url: &str, refresh_time: u64) -> Result<Self, String> {
        let catalog_repo = CachedRepo::new(caches_dir, &name, "catalog.repo", "", url, None, None)?;
        Ok(Self {
            name: name.to_string(),
            fmts,
            refresh_time,
            caches_dir: caches_dir.to_owned(),
            catalog_repo,
            catalog: Mutex::new(None),
        })
    }
    /// Scans the catalog and finds all the modules with a given name
    fn find_mods_with_name(&self, name: &str) -> Vec<ModuleDescriptor> {
        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        let mut results = vec![];
        for cat_mod in catalog.modules.iter() {
            if cat_mod.name == name {
                let descriptor = ModuleDescriptor::new_with_ident_bytes_and_fmt_id(name.to_string(), cat_mod.version.clone(), cat_mod.git_location.get_url().unwrap().as_bytes(), 0);
                results.push(descriptor);
            }
        }
        results
    }
    /// Scans the catalog looking for a single module that matches the provided descriptor
    fn find_mod_idx_with_descriptor(&self, descriptor: &ModuleDescriptor) -> Option<usize> {
        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        for (mod_idx, cat_mod) in catalog.modules.iter().enumerate() {
            if cat_mod.name == descriptor.name() &&
                cat_mod.version.as_ref() == descriptor.version() &&
                descriptor.ident_bytes_and_fmt_id_matches(cat_mod.git_location.get_url().unwrap().as_bytes(), 0) {
                return Some(mod_idx);
            }
        }
        None
    }
}

impl ModuleCatalog for GitCatalog {
    fn lookup(&self, name: &str) -> Vec<ModuleDescriptor> {

        //Get the catalog from the git cache
        let did_update = match self.catalog_repo.update(UpdateMode::TryPullIfOlderThan(self.refresh_time)) {
            Ok(did_update) => did_update,
            Err(e) => {
                log::warn!("Warning: error encountered attempting to fetch remote catalog: {}, {e}", self.name);
                return vec![];
            }
        };

        //Parse the catalog JSON file if we need to
        {
            let mut catalog = self.catalog.lock().unwrap();
            if did_update || catalog.is_none() {
                let catalog_file_path = self.catalog_repo.local_path().join("catalog.json");
                match read_to_string(&catalog_file_path) {
                    Ok(file_contents) => {
                        *catalog = Some(serde_json::from_str(&file_contents).unwrap());
                    },
                    Err(e) => {
                        log::warn!("Warning: Error reading catalog file. remote catalog appears to be corrupt: {}, {e}", self.name);
                        return vec![];
                    }
                }
            }
        }

        //Find the modules that match in the catalog
        self.find_mods_with_name(name)
    }
    fn get_loader(&self, descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String> {
        let mod_idx = self.find_mod_idx_with_descriptor(descriptor).unwrap();

        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        let module = catalog.modules.get(mod_idx).unwrap();

        let loader = match module.git_location.get_loader(self.fmts.iter().map(|f| &**f), Some(&self.caches_dir), &self.name, descriptor.name(), descriptor.version(), None)? {
            Some((loader, _)) => loader,
            None => unreachable!(),
        };

        Ok(loader)
    }
}