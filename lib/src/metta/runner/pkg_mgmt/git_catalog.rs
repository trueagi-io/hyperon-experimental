//! Implements a [ModuleCatalog] serving remotely hosted modules via git
//!

use std::path::{Path, PathBuf};
use std::fs::read_to_string;
use std::sync::Mutex;

use serde::Deserialize;

use crate::metta::runner::*;
use crate::metta::runner::pkg_mgmt::{*, git_cache::*};

/// Struct that matches the catalog.json file fetched from the `catalog.repo`
#[derive(Deserialize, Debug)]
struct CatalogFileFormat {
    modules: Vec<CatalogFileMod>
}

/// A single module in a catalog.json file
#[derive(Deserialize, Debug)]
struct CatalogFileMod {
    name: String,
    remote_url: String,
    #[serde(default)]
    branch: Option<String>,
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
        let catalog_repo = CachedRepo::new(caches_dir, Some(&name), "catalog.repo", "", url, None)?;
        Ok(Self {
            name: name.to_string(),
            fmts,
            refresh_time,
            caches_dir: caches_dir.to_owned(),
            catalog_repo,
            catalog: Mutex::new(None),
        })
    }
    fn find_mods_with_name(&self, name: &str) -> Vec<ModuleDescriptor> {
        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        let mut results = vec![];
        for cat_mod in catalog.modules.iter() {
            if cat_mod.name == name {
                //TODO: incorporate the name into the descriptor
                let descriptor = ModuleDescriptor::new_with_ident_bytes_and_fmt_id(name.to_string(), cat_mod.remote_url.as_bytes(), 0);
                results.push(descriptor);
            }
        }
        results
    }
    fn find_mod_idx_with_descriptor(&self, descriptor: &ModuleDescriptor) -> Option<usize> {
        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        for (mod_idx, cat_mod) in catalog.modules.iter().enumerate() {
            //TODO: Also check version here
            if cat_mod.name == descriptor.name() && descriptor.ident_bytes_and_fmt_id_matches(cat_mod.remote_url.as_bytes(), 0) {
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
        let version_str = ""; //TODO, get the version from the descriptor

        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        let module = catalog.modules.get(mod_idx).unwrap();

        let mod_repo = CachedRepo::new(&self.caches_dir, Some(&self.name), descriptor.name(), version_str, &module.remote_url, module.branch.as_ref().map(|s| s.as_str()))?;
        let _ = mod_repo.update(UpdateMode::PullIfMissing)?;
        let loader = match loader_for_module_at_path(self.fmts.iter().map(|f| &**f), mod_repo.local_path(), Some(descriptor.name()), None)? {
            Some((loader, _)) => loader,
            None => unreachable!()
        };

        Ok(loader)
    }
}