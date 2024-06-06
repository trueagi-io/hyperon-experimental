
use std::fmt::Display;

use crate::atom::{Atom, Grounded, ExecError};
use crate::space::grounding::GroundingSpace;
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_SYMBOL, UNIT_TYPE};
use crate::metta::runner::{Metta, ModuleLoader, RunContext, DynSpace};
use crate::metta::runner::pkg_mgmt::{UpdateMode, ManagedCatalog};
use crate::metta::runner::stdlib::{regex, unit_result};

//TODO, delete these when merging with https://github.com/trueagi-io/hyperon-experimental/pull/706
use crate::metta::matcher::*;
use crate::atom::match_by_equality;

/// Loader to Initialize the "catalog" module
#[derive(Debug)]
pub(crate) struct CatalogModLoader;

impl ModuleLoader for CatalogModLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        let metta = context.metta();
        let mut tref = context.module().tokenizer().borrow_mut();

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

impl PartialEq for CatalogListOp {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl CatalogListOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Display for CatalogListOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "catalog-list!")
    }
}

impl Grounded for CatalogListOp {
    fn type_(&self) -> Atom {
        //TODO-FUTURE, we may want to return the list as atoms, but now it just prints to stdout
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, UNIT_TYPE()])
    }

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
        if cat_name == "all" || cat_name == "git-modules" {
            if let Some(explicit_git_catalog) = &self.metta.environment().explicit_git_mods {
                list_catalog(explicit_git_catalog);
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

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

/// Update all contents of all ManagedCatalogs to the latest version of all modules
#[derive(Clone, Debug)]
pub struct CatalogUpdateOp {
    metta: Metta
}

impl PartialEq for CatalogUpdateOp {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl CatalogUpdateOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Display for CatalogUpdateOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "catalog-update!")
    }
}

impl Grounded for CatalogUpdateOp {
    fn type_(&self) -> Atom {
        //TODO-FUTURE, we may want to return the list as atoms, but now it just prints to stdout
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "catalog-update! expects a catalog name, or \"all\" to update all";
        let cat_name_arg_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;
        let cat_name = if let Atom::Symbol(cat_name) = cat_name_arg_atom {
            cat_name.name()
        } else {
            return Err(ExecError::from(arg_error));
        };

        let mut found_one = false;
        if cat_name == "all" || cat_name == "git-modules" {
            if let Some(explicit_git_catalog) = &self.metta.environment().explicit_git_mods {
                explicit_git_catalog.fetch_newest_for_all(UpdateMode::FetchLatest)?;
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

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

/// Clears the contents of all ManagedCatalogs
#[derive(Clone, Debug)]
pub struct CatalogClearOp {
    metta: Metta
}

impl PartialEq for CatalogClearOp {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl CatalogClearOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Display for CatalogClearOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "catalog-clear!")
    }
}

impl Grounded for CatalogClearOp {
    fn type_(&self) -> Atom {
        //TODO-FUTURE, we may want to return the list as atoms, but now it just prints to stdout
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "catalog-clear! expects a catalog name, or \"all\" to clear all";
        let cat_name_arg_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;
        let cat_name = if let Atom::Symbol(cat_name) = cat_name_arg_atom {
            cat_name.name()
        } else {
            return Err(ExecError::from(arg_error));
        };

        let mut found_one = false;
        if cat_name == "all" || cat_name == "git-modules" {
            if let Some(explicit_git_catalog) = &self.metta.environment().explicit_git_mods {
                explicit_git_catalog.clear_all()?;
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

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}
