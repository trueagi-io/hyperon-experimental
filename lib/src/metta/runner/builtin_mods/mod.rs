
use crate::metta::runner::Metta;

/// Skeleton of the built-in module
mod skel;
mod fileio;
/// Op atoms for working with catalogs
#[cfg(feature = "pkg_mgmt")]
pub mod catalog_mods;

pub fn load_builtin_mods(metta: &Metta) -> Result<(), String> {
    // Built-in modules are loaded on the MeTTa initialization stage but not imported automatically
    let _mod_id = metta.load_module_direct(Box::new(skel::SkelModLoader), "skel").map_err(|e| format!("error loading builtin \"catalog\" module: {e}"))?;
    let _mod_id = metta.load_module_direct(Box::new(fileio::FileioModLoader), "fileio").map_err(|e| format!("error loading builtin \"catalog\" module: {e}"))?;
    #[cfg(feature = "pkg_mgmt")]
    let _mod_id = metta.load_module_direct(Box::new(catalog_mods::CatalogModLoader), "catalog").map_err(|e| format!("error loading builtin \"catalog\" module: {e}"))?;

    Ok(())
}
