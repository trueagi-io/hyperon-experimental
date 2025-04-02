use crate::atom::{Atom, Grounded, ExecError, CustomExecute};
use crate::space::grounding::GroundingSpace;
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_SYMBOL, UNIT_TYPE};
use crate::metta::runner::{Metta, ModuleLoader, RunContext, DynSpace};
use crate::metta::runner::pkg_mgmt::{UpdateMode, ManagedCatalog};
use crate::metta::runner::stdlib::{regex, unit_result};
use crate::metta::runner::modules::MettaMod;

//DISCUSSION: We want to expose more of the pkg_mgmt / catalog system to MeTTa through programmatic
// interfaces, but the details are unclear.  Most importantly, the use cases are unclear, and those
// will inform all design decisions.
//
//## Potential wish-list for operations available directly in MeTTa:
// - Ability to work with semver version objects and requirements.  ie. parse versions and requirments
//    from strings, compare versions to other versions and reqs, etc.
// - Ability to resolve a module by name (and optionally with a version requirement) without loading it,
//    returning a ModuleDescriptor and the Catalog in which it was found
// - Ability to query within a specific catalog, rather than searching all catalogs in priority order
// - Ability to load an exact module from a specific catalog based on its ModuleDescriptor
// - Accessor for a resource (see [ResourceKey]) of a module
// - Ability to inspect / traverse a module dependency hierarchy
// - Full control over what is "installed".  (inspect, upgrade, install, remove)  See below.
//
//## The concept of "installed"
// The user-level concept of an "installed" module has a imprecise mapping to concepts in the pkg_mgmt /
// catalog system.  The loosest idea of "installed" is "available", but most users would not consider 
// software that's freely available on the internet to be "installed".  Most users probably feel like
// "installed" implies that a specific version of the module is local in their file system.
//
// Therefore, modules in remote catalogs wouldn't count as installed, while modules in local catalogs
// would.  Right now, there are 2 kinds of "local" catalogs.  DirCatalog, and LocalCatalog.
//
// DirCatalogs cannot be managed via MeTTa because it is a "read-only" interface over a directory.  We
// wouldn't want the user to do something that deletes items from a directory the user is managing
// themselves or under the control of another piece of software like pip or apt.  Additionally, there
// is no metadata available in a DirCatalog to automatically upgrade any of the contents.
//
// LocalCatalogs are designed to be managed by Hyperon, from MeTTa.  A LocalCatalog mirrors one or more
// upstream module sources (remote Catalogs), so it's expected that a user will install, upgrade, remove,
// etc. modules in a LocalCatalog. 
//
// Therefore, an interface for "managing" what is installed must be limited to LocalCatalogs.
//
//## Should it be supported to list the contents of a DirCatalog?
// A DirCatalog that contains only MeTTa modules, such as the 'modules' directory in the MeTTa config dir,
//    can be listed easily enough.  Things get complicated however because any directory can be loaded as
//    a MeTTa module.  Therefore when configuration adds a heterogeneous directory, such as `site-packages`,
//    the DirCatalog will report every subdir as a module.  Which is correct, but probably not useful.
//    At the very least, it makes `!(catalog-list! all)` much more noisy
//

/// Loader to Initialize the "catalog" module
#[derive(Debug)]
pub(crate) struct CatalogModLoader;

impl ModuleLoader for CatalogModLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);
        self.load_tokens(context.module(), context.metta)
    }

    fn load_tokens(&self, target: &MettaMod, metta: &Metta) -> Result<(), String> {
        let mut tref = target.tokenizer().borrow_mut();

        let catalog_list_op = Atom::gnd(CatalogListOp::new(metta.clone()));
        tref.register_token(regex(r"catalog-list!"), move |_| { catalog_list_op.clone() });
        let catalog_update_op = Atom::gnd(CatalogUpdateOp::new(metta.clone()));
        tref.register_token(regex(r"catalog-update!"), move |_| { catalog_update_op.clone() });
        let catalog_clear_op = Atom::gnd(CatalogClearOp::new(metta.clone()));
        tref.register_token(regex(r"catalog-clear!"), move |_| { catalog_clear_op.clone() });

        Ok(())
    }
}

/// Lists contents of all Catalogs that support the "list" method
#[derive(Clone, Debug)]
pub struct CatalogListOp {
    metta: Metta
}

grounded_op!(CatalogListOp, "catalog-list!");

impl CatalogListOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Grounded for CatalogListOp {
    fn type_(&self) -> Atom {
        //TODO-FUTURE, we may want to return the list as atoms, but now it just prints to stdout
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CatalogListOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "catalog-list! expects a catalog name, or \"all\" to list all available";
        let cat_name_arg_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;
        let cat_name = if let Atom::Symbol(cat_name) = cat_name_arg_atom {
            cat_name.name()
        } else {
            return Err(ExecError::from(arg_error));
        };

        fn list_catalog(cat: &dyn crate::metta::runner::ModuleCatalog) {
            if let Some(cat_iter) = cat.list() {
                println!("{}:", cat.display_name());
                for desc in cat_iter {
                    println!("   {desc}");
                }
            }
        }

        let mut found_one = false;
        if cat_name == "all" || cat_name == "specified-mods" {
            if let Some(specified_mods) = &self.metta.environment().specified_mods {
                list_catalog(specified_mods);
                found_one = true;
            }
        }
        for cat in self.metta.environment().catalogs() {
            if cat_name == "all" || cat_name == cat.display_name() {
                list_catalog(cat);
                found_one = true;
            }
        }

        if !found_one {
            return Err(ExecError::from(format!("no catalog(s) identified by \"{cat_name}\"")));
        }
        unit_result()
    }
}

/// Update all contents of all ManagedCatalogs to the latest version of all modules
#[derive(Clone, Debug)]
pub struct CatalogUpdateOp {
    metta: Metta
}

grounded_op!(CatalogUpdateOp, "catalog-update!");

impl CatalogUpdateOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Grounded for CatalogUpdateOp {
    fn type_(&self) -> Atom {
        //TODO-FUTURE, we may want to return the list as atoms, but now it just prints to stdout
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CatalogUpdateOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "catalog-update! expects a catalog name, or \"all\" to update all";
        let cat_name_arg_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;
        let cat_name = if let Atom::Symbol(cat_name) = cat_name_arg_atom {
            cat_name.name()
        } else {
            return Err(ExecError::from(arg_error));
        };

        let mut found_one = false;
        if cat_name == "all" || cat_name == "specified-mods" {
            if let Some(specified_mods) = &self.metta.environment().specified_mods {
                specified_mods.fetch_newest_for_all(UpdateMode::FetchLatest)?;
                found_one = true;
            }
        }

        for cat in self.metta.environment().catalogs() {
            match cat.as_managed() {
                Some(cat) => if cat_name == "all" || cat_name == cat.display_name() {
                    cat.fetch_newest_for_all(UpdateMode::FetchLatest)?;
                    found_one = true;
                },
                None => {}
            }
        }

        if !found_one {
            return Err(ExecError::from(format!("no catalog(s) identified by \"{cat_name}\"")));
        }
        unit_result()
    }
}

/// Clears the contents of all ManagedCatalogs
#[derive(Clone, Debug)]
pub struct CatalogClearOp {
    metta: Metta
}

grounded_op!(CatalogClearOp, "catalog-clear!");

impl CatalogClearOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Grounded for CatalogClearOp {
    fn type_(&self) -> Atom {
        //TODO-FUTURE, we may want to return the list as atoms, but now it just prints to stdout
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CatalogClearOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "catalog-clear! expects a catalog name, or \"all\" to clear all";
        let cat_name_arg_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;
        let cat_name = if let Atom::Symbol(cat_name) = cat_name_arg_atom {
            cat_name.name()
        } else {
            return Err(ExecError::from(arg_error));
        };

        let mut found_one = false;
        if cat_name == "all" || cat_name == "specified-mods" {
            if let Some(specified_mods) = &self.metta.environment().specified_mods {
                specified_mods.clear_all()?;
                found_one = true;
            }
        }

        for cat in self.metta.environment().catalogs().filter_map(|cat| cat.as_managed()) {
            if cat_name == "all" || cat_name == cat.display_name() {
                cat.clear_all()?;
                found_one = true;
            }
        }

        if !found_one {
            return Err(ExecError::from(format!("no catalog(s) identified by \"{cat_name}\"")));
        }
        unit_result()
    }
}
