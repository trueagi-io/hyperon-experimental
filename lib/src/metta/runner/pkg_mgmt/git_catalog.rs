//! Implements a [ModuleCatalog] serving remotely hosted modules via git
//!

use std::fs::read_to_string;
use std::sync::Mutex;

use serde::Deserialize;

use crate::metta::runner::modules::*;
use crate::metta::runner::pkg_mgmt::{*, git_cache::*};
use crate::metta::runner::environment::Environment;

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
}

#[derive(Debug)]
pub struct GitCatalog {
    name: String,
    refresh_time: u64,
    catalog_repo: CachedRepo,
    catalog: Mutex<Option<CatalogFileFormat>>,
}

impl GitCatalog {
    /// Creates a new GitCatalog with the name and url specified.  `refresh_time` is the time, in
    /// seconds, between refreshes of the catalog file
    pub fn new(env: &Environment, name: &str, url: &str, refresh_time: u64) -> Result<Self, String> {
        let catalog_repo = CachedRepo::new(env, Some(&name), "catalog.repo", "", url, None)?;
        Ok(Self {
            name: name.to_string(),
            refresh_time,
            catalog_repo,
            catalog: Mutex::new(None),
        })
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

        //Parse the catalog JSON file
        if did_update {
            let catalog_file_path = self.catalog_repo.local_path().join("catalog.json");
            match read_to_string(&catalog_file_path) {
                Ok(file_contents) => {
                    let mut catalog = self.catalog.lock().unwrap();
                    *catalog = Some(serde_json::from_str(&file_contents).unwrap());
                },
                Err(e) => {
                    log::warn!("Warning: Error reading catalog file. remote catalog appears to be corrupt: {}, {e}", self.name);
                    return vec![];
                }
            }
        }

        //Find the modules that match in the catalog
        let cat_lock = self.catalog.lock().unwrap();
        let catalog = cat_lock.as_ref().unwrap();
        let mut results = vec![];
        for cat_mod in catalog.modules.iter() {
            if cat_mod.name == name {
                let descriptor = ModuleDescriptor::new_with_ident_bytes_and_fmt_id(name.to_string(), cat_mod.remote_url.as_bytes(), 0);
                results.push(descriptor);
            }
        }

        results
    }
    fn get_loader(&self, descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String> {

        //TODO-NOW: Make a ModuleLoader object that contains the CachedRepo for the URL for the module being loaded

        Err("TODO-NOW".to_string())
    }
}