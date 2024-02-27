
use std::path::{Path, PathBuf};
use std::collections::HashMap;

use crate::metta::*;
use crate::metta::runner::*;

use regex::Regex;

#[cfg(not(feature = "minimal"))]
use super::stdlib::*;

#[cfg(feature = "minimal")]
use super::interpreter2::interpret;
#[cfg(feature = "minimal")]
use super::stdlib2::*;

#[cfg(feature = "pkg_mgmt")]
pub mod catalog;
#[cfg(feature = "pkg_mgmt")]
use catalog::*;

mod mod_names;
pub(crate) use mod_names::{ModNameNode, mod_name_from_path, mod_name_relative_path, module_name_is_legal};
pub use mod_names::{TOP_MOD_NAME, SELF_MOD_NAME, MOD_NAME_SEPARATOR};

/// Contains state associated with a loaded MeTTa module
#[derive(Debug)]
pub struct MettaMod {
    mod_path: String,
    resource_dir: Option<PathBuf>,
    space: DynSpace,
    tokenizer: Shared<Tokenizer>,
    imported_deps: Mutex<HashMap<ModId, DynSpace>>,
    sub_module_names: Option<ModNameNode>,
    #[cfg(feature = "pkg_mgmt")]
    pkg_info: PkgInfo,
}

impl MettaMod {

    /// Internal method to initialize an empty MettaMod
    pub(crate) fn new_with_tokenizer(metta: &Metta, mod_path: String, space: DynSpace, tokenizer: Shared<Tokenizer>, resource_dir: Option<PathBuf>, no_stdlib: bool) -> Self {

        //Give the space a name based on the module, if it doesn't already have one
        if let Some(any_space) = space.borrow_mut().as_any_mut() {
            if let Some(g_space) = any_space.downcast_mut::<GroundingSpace>() {
                if g_space.name().is_none() {
                    g_space.set_name(mod_path.clone());
                }
            }
        }

        let new_mod = Self {
            mod_path,
            space,
            tokenizer,
            imported_deps: Mutex::new(HashMap::new()),
            resource_dir,
            sub_module_names: Some(ModNameNode::new(ModId::INVALID)),
            #[cfg(feature = "pkg_mgmt")]
            pkg_info: PkgInfo::default(),
        };

        // Load the base tokens for the module's new Tokenizer
        register_runner_tokens(&mut *new_mod.tokenizer().borrow_mut(), new_mod.tokenizer().clone(), &new_mod.space, metta);
        register_common_tokens(&mut *new_mod.tokenizer().borrow_mut(), new_mod.tokenizer().clone(), &new_mod.space, metta);

        //Load the stdlib unless this module is no_std
        if !no_stdlib {
            if let Some(stdlib_mod_id) = metta.0.stdlib_mod.get() {
                new_mod.import_all_from_dependency(&metta, *stdlib_mod_id).unwrap();
            }
        }

        new_mod
    }

    /// Locates and retrieves a loaded module, or a sub-module relative to &self
    pub(crate) fn get_module_by_name(&self, runner: &Metta, mod_name: &str) -> Result<ModId, String> {
        let mod_name = self.normalize_module_name(mod_name)?;
        let mod_id = match &self.sub_module_names {
            Some(subtree) => {
                let module_names = runner.0.module_names.lock().unwrap();
                module_names.resolve_layered(&[(&self.mod_path, subtree)], &mod_name).ok_or_else(|| format!("Unable to locate module: {mod_name}"))
            },
            None => runner.get_module_by_name(&mod_name)
        }?;
        if mod_id == ModId::INVALID {
            Err(format!("Attempt to resolve module that is not yet loaded: {mod_name}"))
        } else {
            Ok(mod_id)
        }
    }

    /// Adds a sub-module to this module's subtree if a relative path was specified.  Otherwise adds
    /// the sub-module to the runner's main tree
    pub(crate) fn add_module_to_name_tree(&mut self, runner: &Metta, mod_name: &str, mod_id: ModId) -> Result<(), String> {
        let mod_name = self.normalize_module_name(mod_name)?;
        match &mut self.sub_module_names {
            Some(subtree) => {
                let mut module_names = runner.0.module_names.lock().unwrap();
                module_names.add_to_layered(&mut[(&mut self.mod_path, subtree)], &mod_name, mod_id)
            },
            None => runner.add_module_to_name_tree(&mod_name, mod_id)
        }
    }

    /// Join a relative module path as a sub-module to `&self`
    pub(crate) fn concat_relative_module_path(&self, relative_path: &str) -> String {
        if relative_path.len() > 0 {
            format!("{}:{}", self.path(), relative_path)
        } else {
            self.path().to_string()
        }
    }

    /// Normalize a module name into a canonical name-path form, and expanding a relative module-path
    pub(crate) fn normalize_module_name(&self, mod_name: &str) -> Result<String, String> {
        match mod_name_relative_path(mod_name) {
            Some(remainder) => Ok(self.concat_relative_module_path(remainder)),
            None => ModNameNode::normalize_name_path(mod_name),
        }
    }

    // Internal method as part of the loading process, for loading a MettaMod into a runner
    pub(crate) fn take_sub_module_names(&mut self) -> Option<ModNameNode> {
        core::mem::take(&mut self.sub_module_names)
    }

    /// Adds a loaded module as a dependency of the `&self` [MettaMod], and adds a [Tokenizer] entry to access
    /// the dependent module's Space.
    pub fn import_dependency_as(&self, metta: &Metta, mod_id: ModId, name: Option<String>) -> Result<(), String> {

        // Get the space and name associated with the dependent module
        let mut runner_state = RunnerState::new_with_module(metta, mod_id);
        let (dep_space, name) = runner_state.run_in_context(|context| {
            let dep_space = context.module().space().clone();
            let name = match name {
                Some(name) => name,
                None => context.module().name().to_string()
            };
            Ok((dep_space, name))
        })?;

        //If the space name doesn't begin with '&' then add it
        let new_space_token = if name.starts_with('&') {
            name
        } else {
            format!("&{name}")
        };

        // Add a new atom to the &self space, so we can access the dependent module
        let dep_space_atom = Atom::gnd(dep_space);
        self.tokenizer.borrow_mut().register_token_with_regex_str(&new_space_token, move |_| { dep_space_atom.clone() });

        Ok(())
    }

    /// Adds a specific atom and/or Tokenizer entry from a dependency module to the &self module
    ///
    /// Behavior:
    /// * If the `from_name` argument exactly matches a [Tokenizer] entry in the source module,
    ///     then that entry will be imported, and the `name` argument will be ignored. In this case
    ///     no atom is imported.
    /// * If an exact [Tokenizer] entry was not found, this method will attempt to resolve `from_name`
    ///     into an atom, using the [Tokenizer] and [Space] associated with the dependent module, and
    ///     the resolved atom will be imported into the `&self` [Space]
    /// * If `name` is provided, then if the resolved atom not a Symbol or if the resolved atom is a
    ///     symbol that doesn't perfectly match `name`, a new [Tokenizer] entry will be created to
    ///     access the atom in the &self module
    ///
    // QUESTION: This behavior of exactly matching a regex makes importing a Tokenizer pattern pretty
    // unfriendly.  Does it make sense to require Tokenizers entries to be associated with atoms, for
    // example "Type Atoms"?  For example, we could have an "Number" type that is tied to all the
    // Tokenizer regex patters used to parse different types of numbers?  Then a user could
    // "!(import! Number from Arithmetic)" or whatever, and get all the Tokenizer patterns that parse
    // numbers?
    //
    // More discussion on the topic of tokenizer entry names is here https://github.com/trueagi-io/hyperon-experimental/issues/510
    pub fn import_item_from_dependency_as(&self, metta: &Metta, from_name: &str, mod_id: ModId, name: Option<&str>) -> Result<(), String> {

        // Get the space and tokenizer associated with the dependent module
        let mut runner_state = RunnerState::new_with_module(metta, mod_id);
        let (dep_space, dep_tokenizer, src_mod_name) = runner_state.run_in_context(|context| {
            let dep_space = context.module().space().clone();
            let dep_tokenizer = context.module().tokenizer().clone();
            let src_mod_name = context.module().path().to_string();
            Ok((dep_space, dep_tokenizer, src_mod_name))
        })?;

        //See if there is a match in the dependent module's Tokenizer
        if let Some(found_constructor) = dep_tokenizer.borrow().find_exact(from_name) {

            // If so, this method just transplants the Tokenizer entry
            self.tokenizer.borrow_mut().register_token_with_func_ptr(Regex::new(from_name).unwrap(), found_constructor);
        } else {
            //Otherwise we will try and transplant an atom

            // Get and reduce the atom we are importing, from the dependent module
            let mut parser = SExprParser::new(from_name);
            let import_sym = parser.parse(&dep_tokenizer.borrow())?.ok_or_else(|| format!("Import failed to resolve \"{from_name}\""))?;
            if let Some(extra_atom) = parser.parse(&dep_tokenizer.borrow())? { return Err(format!("Extraneous token in import \"{extra_atom}\""));}
            let src_atom_vec = interpret(dep_space, &import_sym)?;
            match src_atom_vec.len() {
                0 => return Err(format!("Failed to resolve import \"{from_name}\" in module \"{src_mod_name}\"")),
                1 => {},
                _ => return Err(format!("Ambiguous import \"{from_name}\" in module \"{src_mod_name}\"")),
            }
            let src_atom = src_atom_vec.into_iter().next().unwrap();

            // Add the atom to our module's space
            self.add_atom(src_atom.clone(), false).map_err(|a| a.to_string())?;

            // Finally, Add a Tokenizer entry to access this atom, if one is needed
            let name = match name {
                Some(name) => name,
                None => from_name
            };
            //We will add Tokenizer entries for all non-symbols, and symbol atoms that don't match name
            let should_add_tok = match &src_atom {
                Atom::Symbol(s) => s.name() != name,
                _ => true,
            };
            if should_add_tok {
                self.tokenizer.borrow_mut().register_token_with_regex_str(&name, move |_| { src_atom.clone() });
            }
        }

        Ok(())
    }

    /// Effectively adds all atoms in a dependency module to the &self module, by adding the dependency
    /// module's space as an atom inside the &self module
    ///
    /// WARNING: Module import behavior is still WIP, specifically around "import *" behavior, and
    /// especially around transitive imports
    pub fn import_all_from_dependency(&self, metta: &Metta, mod_id: ModId) -> Result<(), String> {

        // See if the dependency has already been imported
        if self.contains_imported_dep(&mod_id) {
            return Ok(())
        }

        // Get the space associated with the dependent module
        let mut runner_state = RunnerState::new_with_module(metta, mod_id);
        let (dep_space, transitive_deps) = runner_state.run_in_context(|context| {
            log::info!("import_all_from_dependency: importing from {} (modid={mod_id:?}) into {}", context.module().path(), self.path());
            Ok(context.module().stripped_space())
        })?;

        // Add a new Grounded Space atom to the &self space, so we can access the dependent module
        self.insert_dep(mod_id, dep_space)?;

        // Add all the transitive deps from the dependency
        if let Some(transitive_deps) = transitive_deps {
            for (dep_mod_id, dep_space) in transitive_deps {
                self.insert_dep(dep_mod_id, dep_space)?;
            }
        }

        // Finally, Import the tokens from the dependency
        self.import_all_tokens_from_dependency(metta, mod_id)
    }

    /// Merges all Tokenizer entries in a dependency module into &self
    pub(crate) fn import_all_tokens_from_dependency(&self, metta: &Metta, mod_id: ModId) -> Result<(), String> {

        // Get the tokenizer associated with the dependent module
        let mut runner_state = RunnerState::new_with_module(metta, mod_id);
        let dep_tokenizer = runner_state.run_in_context(|context| Ok(context.module().tokenizer().clone()))?;

        //Import all the Tokenizer entries from the dependency
        let mut dep_tok_clone = dep_tokenizer.borrow().clone();
        self.tokenizer.borrow_mut().move_front(&mut dep_tok_clone);

        Ok(())
    }

    /// Returns `true` if a module used [import_all_from_dependency] to import a specific loaded dependency
    pub fn contains_imported_dep(&self, mod_id: &ModId) -> bool {
        let deps_table = self.imported_deps.lock().unwrap();
        deps_table.contains_key(&mod_id)
    }

    /// Private function to insert a dependency's space in a grounded atom into a module's space
    fn insert_dep(&self, mod_id: ModId, dep_space: DynSpace) -> Result<(), String> {
        let mut deps_table = self.imported_deps.lock().unwrap();
        if !deps_table.contains_key(&mod_id) {
            self.add_atom(Atom::gnd(dep_space.clone()), false).map_err(|a| a.to_string())?;
            deps_table.insert(mod_id, dep_space);
        }
        Ok(())
    }

    /// Private function that returns a deep copy of a module's space, with the module's dependency
    /// sub-spaces stripped out and returned separately
    //
    // HACK.  This is a terrible design.  It is a stop-gap to get around problems caused by transitive
    // imports described above, but it has some serious downsides, To name a few:
    //  - It means we must copy every atom in the space for every import
    //  - It only works when the dep module's space is a GroundingSpace
    fn stripped_space(&self) -> (DynSpace, Option<HashMap<ModId, DynSpace>>) {
        let deps_table = self.imported_deps.lock().unwrap();
        if deps_table.len() == 0 {
            (self.space.clone(), None)
        } else {
            if let Some(any_space) = self.space.borrow().as_any() {
                if let Some(dep_g_space) = any_space.downcast_ref::<GroundingSpace>() {

                    // Do a deep-clone of the dep-space atom-by-atom, because `space.remove()` doesn't recognize
                    // two GroundedAtoms wrapping DynSpaces as being the same, even if the underlying space is
                    let mut new_space = GroundingSpace::new();
                    new_space.set_name(self.path().to_string());
                    for atom in dep_g_space.atom_iter().unwrap() {
                        if let Some(sub_space) = atom.as_gnd::<DynSpace>() {
                            if !deps_table.values().any(|space| space == sub_space) {
                                new_space.add(atom.clone());
                            }
                        } else {
                            new_space.add(atom.clone());
                        }
                    }

                    return (DynSpace::new(new_space), Some(deps_table.clone()));
                }
            }
            log::warn!("import_all_from_dependency: Importing from module based on a non-GroundingSpace is currently unsupported");
            (self.space.clone(), None)
        }
    }

    /// Returns the full path of a loaded module.  For example: "top.parent_mod.this_mod"
    pub fn path(&self) -> &str {
        &self.mod_path
    }

    /// Returns the name of the loaded module.
    pub fn name(&self) -> &str {
        mod_name_from_path(&self.mod_path)
    }

    #[cfg(feature = "pkg_mgmt")]
    pub fn pkg_info(&self) -> &PkgInfo {
        &self.pkg_info
    }

    pub fn space(&self) -> &DynSpace {
        &self.space
    }

    pub fn tokenizer(&self) -> &Shared<Tokenizer> {
        &self.tokenizer
    }

    pub fn resource_dir(&self) -> Option<&Path> {
        self.resource_dir.as_deref()
    }

    /// A convenience to add an an atom to a module's Space, if it passes type-checking
    pub(crate) fn add_atom(&self, atom: Atom, type_check: bool) -> Result<(), Atom> {
        if type_check && !validate_atom(self.space.borrow().as_space(), &atom) {
            return Err(Atom::expr([ERROR_SYMBOL, atom, BAD_TYPE_SYMBOL]));
        }
        self.space.borrow_mut().add(atom);
        Ok(())
    }

}

/// Implemented to supply a loader functions for MeTTa modules
///
/// A ModuleLoader is responsible for loading a MeTTa module through the API.  Implementations of
/// ModuleLoader can be used to define a module format or to supply programmatically defined modules
pub trait ModuleLoader: std::fmt::Debug + Send + Sync {
    /// A function to load the module my making MeTTa API calls.  This function will be called by
    /// [Metta::get_or_init_module]
    fn load(&self, context: &mut RunContext) -> Result<(), String>;
}

//-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-
// TESTS
//-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-=-=-+-

#[derive(Debug)]
struct OuterLoader;

impl ModuleLoader for OuterLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        let parser = SExprParser::new("outer-module-test-atom");
        context.push_parser(Box::new(parser));

        Ok(())
    }
}

#[derive(Debug)]
struct InnerLoader;

impl ModuleLoader for InnerLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        let parser = SExprParser::new("inner-module-test-atom");
        context.push_parser(Box::new(parser));

        Ok(())
    }
}

/// This tests loading a module as a sub-module of another loaded module using a hierarchical
/// namespace path
#[test]
fn hierarchical_module_import_test() {
    let runner = Metta::new(Some(EnvBuilder::test_env()));

    //Make sure we get a reasonable error, if we try to load a sub-module to a module that doesn't exist
    let result = runner.load_module_direct(&InnerLoader, "outer:inner");
    assert!(result.is_err());

    //Make sure we can load sub-modules sucessfully
    let _outer_mod_id = runner.load_module_direct(&OuterLoader, "outer").unwrap();
    let _inner_mod_id = runner.load_module_direct(&InnerLoader, "outer:inner").unwrap();

    //Make sure we load the outer module sucessfully and can match the outer module's atom, but not
    // the inner module's
    let result = runner.run(SExprParser::new("!(import! &self outer)"));
    assert_eq!(result, Ok(vec![vec![expr!()]]));
    let result = runner.run(SExprParser::new("!(match &self outer-module-test-atom found!)"));
    assert_eq!(result, Ok(vec![vec![sym!("found!")]]));
    let result = runner.run(SExprParser::new("!(match &self inner-module-test-atom found!)"));
    assert_eq!(result, Ok(vec![vec![]]));

    //Now import the inner module by relative module namespace, and check to make sure we can match
    // its atom
    let result = runner.run(SExprParser::new("!(import! &self outer:inner)"));
    assert_eq!(result, Ok(vec![vec![expr!()]]));
    let result = runner.run(SExprParser::new("!(match &self inner-module-test-atom found!)"));
    assert_eq!(result, Ok(vec![vec![sym!("found!")]]));
}

#[derive(Debug)]
struct RelativeOuterLoader;

impl ModuleLoader for RelativeOuterLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        let _inner_mod_id = context.load_module_direct(&InnerLoader, "self:inner").unwrap();

        let parser = SExprParser::new("outer-module-test-atom");
        context.push_parser(Box::new(parser));

        //Test to see if I can resolve the module we just loaded,
        // but make sure we can't resolve "self" yet, since the loading isn't finished
        assert!(context.get_module_by_name("self:inner").is_ok());
        assert!(context.get_module_by_name("self").is_err());

        Ok(())
    }
}

/// This tests loading a sub-module from another module's runner, using a relative namespace path
#[test]
fn relative_submodule_import_test() {
    // let runner = Metta::new(Some(EnvBuilder::test_env()));

    // LP-TODO-NEXT: This test curently fails for reasons explained in the comment inside Metta::merge_sub_module_names.

    // //Load the "outer" module, which will load the inner module as part of its loader
    // let _outer_mod_id = runner.load_module_direct(&RelativeOuterLoader, "outer").unwrap();

    // //runner.display_loaded_modules();

    // //Make sure we didn't accidentally load "inner" at the top level
    // assert!(runner.get_module_by_name("inner").is_err());

    // //Confirm we didn't end up with a module called "self"
    // assert!(runner.get_module_by_name("self:inner").is_err());
    // assert!(runner.get_module_by_name("self").is_err());

    // //Now make sure we can actually resolve the loaded sub-module
    // runner.get_module_by_name("outer:inner").unwrap();

    // //LP-TODO-NEXT, test that I can add a second inner from the runner, by adding "top:outer:inner2",
    // // and then that I can import it directly into "outer" from within the runner's context using the "self:inner2" mod path

}

//LP-TODO-NEXT,  Make a test for an inner-loader that throws an error, blocking the outer-loader from loading sucessfully,
// and make sure neither module is loaded into the named index
//
//Also test the case where the inner loader is sucessul, but then the outer loader throws an error.  Also make sure neither
// module is loaded into the namespace
//
