
use crate::metta::runner::pkg_mgmt::*;

/// An interface to facilitate explicit management of a catalog, usually as a local mirror
/// of one or more remote catalogs used by a user to insulate them from upstream changes
///
/// NOTE: ModuleDescriptors used by the catalog should be identical to those used by the
/// UpstreamCatalog(s)
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

