
use crate::metta::runner::Metta;

/// Op atoms for working with catalogs
#[cfg(feature = "pkg_mgmt")]
pub mod catalog_mods;

pub fn load_builtin_mods(metta: &Metta) -> Result<(), String> {
    #[cfg(feature = "pkg_mgmt")]
    let _mod_id = metta.load_module_direct(Box::new(catalog_mods::CatalogModLoader), "catalog").map_err(|e| format!("error loading builtin \"catalog\" module: {e}"))?;

    Ok(())
}
