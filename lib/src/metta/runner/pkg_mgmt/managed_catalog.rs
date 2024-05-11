
use std::path::{Path, PathBuf};
use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::metta::runner::*;
use crate::metta::runner::pkg_mgmt::*;

/// An interface to facilitate explicit management of a catalog, usually as a local mirror
/// of one or more remote catalogs used by a user to insulate them from upstream changes
//
//NOTE FOR THE FUTURE: There are two major problems with this `fetch_newest_for_all`
// interface.
// 1. There is no way to know which modules may be deleted from the catalog and which must
//    be kept.  Therefore it is impossible to simply "upgrade" a module - ie. pulling a
//    new version and removing the old.
//
//    This is because an older version of the module may satisfy a dependency that is not
//    satisfied by the newer version.  And this object does not have enough visibility to
//    know.
//
// 2. Relatedly, there is no way to automatically fetch the latest module for a given
//    dependency.  For example, if the catalog has v0.1.3 of a mod, and the upstream
//    catalog contains v0.2.0 and v0.1.5, there is no way to know which is needed between
//    those two, in the context of the importer's requirements.
//
//PROPOSAL: Requirement API.  A ManagedCatalog would need to track which requirements are
// satisfied by each module, so that if a requirement were better satisfied by another
// module then the old module could be removed.
//
// There are a number of unanswered questions however:
// - How should the managed catalog interact with modules from other catalogs? Should
//  the managed catalog track dependencies outside the upstream catalog?  A module from
//  any catalog can theoretically satisfy a dependency so what happens if a local dir
//  catalog mod satisfies a sub-dependency, but a newer version of the mod exists on the
//  remote catalog?
// - How will the managed catalog logic work with regard to the sat-set solving?
//   See "QUESTION on shared base dependencies".  In other words, the best dependency mod
//   in isolation might not be the best when considered holistically.  The Requirement API
//   needs to take that into account.
//
pub trait ManagedCatalog: ModuleCatalog {

    /// Clears all locally stored modules, resetting the local catalog to an empty state
    fn clear_all(&self) -> Result<(), String>;

    /// Fetch a specific module from the UpstreamCatalog.  Returns `Ok(())`` if the module
    /// already exists in the catalog
    ///
    /// NOTE: This method will likely become async in the future
    fn fetch(&self, descriptor: &ModuleDescriptor) -> Result<(), String>;

    /// Remove a specific module from the catalog
    fn remove(&self, descriptor: &ModuleDescriptor) -> Result<(), String>;

    /// AKA "upgrade".  Fetches the newest version for each module that currently exists in
    /// the catalog
    ///
    /// NOTE: This API will likely change in the future.  See "NOTE FOR THE FUTURE" in comments
    /// for `ManagedCatalog`
    fn fetch_newest_for_all(&self) -> Result<(), String> {
        let iter = self.list_name_uid_pairs()
            .ok_or_else(|| "managed catalog must support `list` method".to_string())?;
        for (name, uid) in iter {
            if let Some(desc) = self.lookup_newest_with_uid_and_version_req(&name, uid, None) {
                self.fetch(&desc)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct LocalCatalog {
    name: String,
    upstream_catalogs: Vec<Box<dyn ModuleCatalog>>,
    storage_dir: PathBuf,
    local_toc: Mutex<LocalCatalogTOC>,
}

impl LocalCatalog {
    pub fn new(caches_dir: &Path, name: &str) -> Result<Self, String> {
        let storage_dir = caches_dir.join(name);
        let local_toc = LocalCatalogTOC::build_from_dir(&storage_dir)?;

        Ok(Self {
            name: name.to_string(),
            upstream_catalogs: vec![],
            storage_dir,
            local_toc: Mutex::new(local_toc),
        })
    }
    pub fn push_upstream_catalog(&mut self, catalog: Box<dyn ModuleCatalog>) {
        self.upstream_catalogs.push(catalog);
    }
    pub fn upstream_catalogs(&self) -> &[Box<dyn ModuleCatalog>] {
        &self.upstream_catalogs[..]
    }
    fn lookup_by_name_in_toc(&self, name: &str) -> Option<Vec<ModuleDescriptor>> {
        let local_toc = self.local_toc.lock().unwrap();
        local_toc.lookup_by_name(name)
    }
    /// Adds the [ModuleDescriptor] to the TOC if it doesn't exist.  Won't create duplicates
    fn add_to_toc(&self, descriptor: ModuleDescriptor) -> Result<(), String> {
        let mut local_toc = self.local_toc.lock().unwrap();
        local_toc.add_descriptor(descriptor)
    }
    fn list_toc(&self) -> Vec<ModuleDescriptor> {
        let local_toc = self.local_toc.lock().unwrap();
        local_toc.all_sorted_descriptors()
    }
    pub(crate) fn get_loader_with_explicit_refresh(&self, descriptor: &ModuleDescriptor, should_refresh: bool) -> Result<Box<dyn ModuleLoader>, String> {

        //Figure out which upstream catalog furnished this descriptor by trying each one
        let mut upstream_loader = None;
        for upstream in self.upstream_catalogs.iter() {
            match upstream.get_loader(descriptor) {
                Ok(loader) => {
                    upstream_loader = Some(loader);
                    break
                },
                Err(_) => {}
            }
        }
        let upstream_loader = match upstream_loader {
            Some(loader) => loader,
            None => {
                // TODO: It would be nice to have the option here to pull a different but compatible
                // mod from the upstream catalogs; however we don't have the original requirement info,
                // so currently we cannot do that.  See the write-up above about the "Requirement API".
                return Err(format!("Upstream Catalogs can no longer supply module {}", descriptor.name()));
            }
        };

        //Resolve the local dir to use as the local cache
        let cache_dir_name = dir_name_from_descriptor(descriptor);
        let local_cache_dir = self.storage_dir.join(cache_dir_name);

        //Make sure this mod is in the TOC
        self.add_to_toc(descriptor.to_owned())?;

        //Wrap the upstream loader in a loader object from this catalog
        let wrapper_loader = LocalCatalogLoader {local_cache_dir, upstream_loader, should_refresh};
        Ok(Box::new(wrapper_loader))
    }
}

impl ModuleCatalog for LocalCatalog {
    fn display_name(&self) -> String {
        self.name.clone()
    }
    fn lookup(&self, name: &str) -> Vec<ModuleDescriptor> {

        //If we have some matching modules in the local cache then return them
        if let Some(descriptors) = self.lookup_by_name_in_toc(name) {
            return descriptors;
        }

        //If we don't have anything locally, check the upstream catalogs in order until one
        // of them returns some results
        for upstream in self.upstream_catalogs.iter() {
            let upstream_results = upstream.lookup(name);
            if upstream_results.len() > 0 {
                return upstream_results;
            }
        }

        //We didn't find any matching modules, locally or upstream 
        vec![]
    }
    fn get_loader(&self, descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String> {
        self.get_loader_with_explicit_refresh(descriptor, false)
    }
    fn list<'a>(&'a self) -> Option<Box<dyn Iterator<Item=ModuleDescriptor> + 'a>> {
        Some(Box::new(self.list_toc().into_iter()))
    }
}

/// A [ModuleLoader] for a [LocalCatalog] that wraps another ModuleLoader for an upstream [ModuleCatalog]
#[derive(Debug)]
struct LocalCatalogLoader {
    local_cache_dir: PathBuf,
    should_refresh: bool,
    upstream_loader: Box<dyn ModuleLoader>
}

impl ModuleLoader for LocalCatalogLoader {
    fn prepare(&self, _local_dir: Option<&Path>, should_refresh: bool) -> Result<Option<Box<dyn ModuleLoader>>, String> {
        self.upstream_loader.prepare(Some(&self.local_cache_dir), should_refresh | self.should_refresh)
    }
    fn load(&self, _context: &mut RunContext) -> Result<(), String> {
        unreachable!() //We will substitute the `upstream_loader` during prepare
    }
}

/// A Table of Contents (TOC) for a LocalCatalog
#[derive(Debug)]
struct LocalCatalogTOC {
    mods_by_name: BTreeMap<String, Vec<ModuleDescriptor>>
}

impl LocalCatalogTOC {
    /// Scans a directory and builds up a TOC from the contents
    fn build_from_dir(storage_dir: &Path) -> Result<Self, String> {
        if !storage_dir.exists() {
            std::fs::create_dir_all(&storage_dir).map_err(|e| e.to_string())?;
        } else {
            if !storage_dir.is_dir() {
                return Err(format!("Found file instead of directory at {}", storage_dir.display()));
            }
        }

        let mut new_self = Self {
            mods_by_name: BTreeMap::new()
        };

        for dir_item_handle in std::fs::read_dir(storage_dir).map_err(|e| e.to_string())? {
            let dir_entry = dir_item_handle.map_err(|e| e.to_string())?;
            let file_name = dir_entry.file_name();
            let name_str = file_name.to_str()
                .ok_or_else(|| format!("Invalid characters found in local cache at path: {}", dir_entry.path().display()))?;

            // Name reserved by GitCatalog.  We may generalize this "reserved" mechanism when
            // we support additional upstream catalog types
            if name_str != "catalog.repo" {
                let descriptor = parse_descriptor_from_dir_name(name_str)?;
                new_self.add_descriptor(descriptor)?;
            }
        }

        Ok(new_self)
    }
    fn lookup_by_name(&self, name: &str) -> Option<Vec<ModuleDescriptor>> {
        if let Some(descriptors) = self.mods_by_name.get(name) {
            if descriptors.len() > 0 {
                return Some(descriptors.clone());
            }
        }
        None
    }
    /// Returns a Vec containing all ModuleDescriptors in the TOC, sorted by name
    fn all_sorted_descriptors(&self) -> Vec<ModuleDescriptor> {
        self.mods_by_name.iter().flat_map(|(_name, desc_vec)| desc_vec).cloned().collect()
    }
    /// Adds a descriptor to a TOC.  Won't add a duplicate
    fn add_descriptor(&mut self, descriptor: ModuleDescriptor) -> Result<(), String> {
        let desc_vec = self.mods_by_name.entry(descriptor.name().to_owned()).or_insert(vec![]);
        if !desc_vec.contains(&descriptor) {
            desc_vec.push(descriptor);
            desc_vec.sort_by(|a, b| a.version().cmp(&b.version()));
        }
        Ok(())
    }
}

/// Returns a String that can be used as a directory to cache local files associated
/// with the module, such as build artifacts and/or downloads
pub(crate) fn dir_name_from_descriptor(desc: &ModuleDescriptor) -> String {
    let mod_dir_name = match desc.version() {
        Some(version) => format!("{}@{version}", desc.name()),
        None => desc.name().to_string()
    };
    match desc.uid() {
        Some(uid) => format!("{mod_dir_name}#{uid:016x}"),
        None => format!("{mod_dir_name}")
    }
}

/// Performs the inverse of [dir_name_from_descriptor], deconstructing a dir_name str into a [ModuleDescriptor]
pub(crate) fn parse_descriptor_from_dir_name(dir_name: &str) -> Result<ModuleDescriptor, String> {
    let (name_and_vers, uid) = match dir_name.rfind('#') {
        Some(pos) => (&dir_name[0..pos], Some(&dir_name[pos+1..])),
        None => (dir_name, None)
    };
    let (name, version) = match name_and_vers.find('@') {
        Some(pos) => (&name_and_vers[0..pos], Some(&name_and_vers[pos+1..])),
        None => (name_and_vers, None)
    };
    let version = match version {
        Some(ver_str) => Some(semver::Version::parse(ver_str).map_err(|e| e.to_string())?),
        None => None
    };
    let uid = match uid {
        Some(uid_str) => Some(u64::from_str_radix(uid_str, 16).map_err(|e| e.to_string())?),
        None => None
    };
    Ok(ModuleDescriptor::new(name.to_string(), version, uid))
}
