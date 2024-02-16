
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::Mutex;

use crate::space::{Space, DynSpace};
use crate::metta::*;
use crate::metta::runner::*;
use crate::metta::types::validate_atom;
use crate::metta::text::{SExprParser, Tokenizer};
use crate::common::shared::Shared;

use regex::Regex;

#[cfg(not(feature = "minimal"))]
use super::interpreter::interpret;
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

/// The name of the top module in a runner
pub const TOP_MOD_NAME: &'static str = "top";

/// The name to refer to the current module in the module name hierarchy
pub const SELF_MOD_NAME: &'static str = "self";

/// The separator between parent and child module names in a module name path
pub const MOD_NAME_SEPARATOR: char = ':';

/// Contains state associated with a loaded MeTTa module
#[derive(Debug)]
pub struct MettaMod {
    mod_path: String,
    resource_dir: Option<PathBuf>,
    space: DynSpace,
    tokenizer: Shared<Tokenizer>,
    imported_deps: Mutex<HashMap<ModId, DynSpace>>,
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

    /// Returns the name of the module that the `ModuleLoader` is able to load
    fn name(&self) -> Result<String, String>;

    // TODO: implement this when I implement module versioning
    // fn version(&self) -> Result<Option<Version>, String>;

    /// A function to load the module my making MeTTa API calls.  This function will be called by
    /// [Metta::get_or_init_module]
    fn load(&self, context: &mut RunContext) -> Result<(), String>;
}

/// Private struct, A node in a tree to locate loaded mods by name
///
/// # Name Resolution Behavior
/// `top` is a reserved module name for the module at the top of the runner
/// `top.some_mod` and `some_mod` are equivalent.  In other words, `top` is optional in a path
/// `self` is an alias for the current module
/// `self.some_mod` is a private sub-module of the current module
///
#[derive(Debug)]
pub(crate) struct ModNameNode {
    mod_id: ModId,
    children: HashMap<String, ModNameNode>
}

//LP-TODO-NEXT.  Momentarily commented this function out to squish the unused warning.  This is part of
// the implementatino of relative module paths
// /// Returns `Some(path)`, with `path` being the relative portion following `self:`, if the name
// /// begins with "self:". Returns "" if the name exactly equals "self".  Otherwise returns None
// pub(crate) fn mod_name_relative_path(name: &str) -> Option<&str> {
//     if name.starts_with(SELF_MOD_NAME) {
//         if name.len() == SELF_MOD_NAME.len() {
//             Some("")
//         } else {
//             if name.as_bytes()[SELF_MOD_NAME.len()] == MOD_NAME_SEPARATOR as u8 {
//                 Some(&name[(SELF_MOD_NAME.len()+1)..])
//             } else {
//                 None
//             }
//         }
//     } else {
//         None
//     }
// }

/// Returns the part of a module name path after the last separator, or the entire path if it does not
/// contain a separator.  May panic if the path is invalid
pub(crate) fn mod_name_from_path(path: &str) -> &str {
    let mut start_idx = 0;
    for (idx, the_char) in path.char_indices() {
        if the_char == MOD_NAME_SEPARATOR {
            start_idx = idx+1;
        }
    }
    &path[start_idx..]
}

impl ModNameNode {

    /// Returns the node corresponding to the runner's "top" mod
    pub fn top() -> Self {
        Self::new(ModId::TOP)
    }

    /// Private constructor
    fn new(mod_id: ModId) -> Self {
        Self {
            mod_id,
            children: HashMap::new(),
        }
    }

    /// Adds a single new node to the tree.  Does NOT recursively add multiple nodes
    ///
    /// Reuturns `true` if the node was sucessfully added, otherwise `false`.  If an entry
    /// already exists at that name, the existing entry will be replaced
    pub fn add(&mut self, name: &str, mod_id: ModId) -> bool {
        if let Some((parent_node, mod_name)) = self.parse_parent_mut(name) {
            if mod_name == TOP_MOD_NAME || mod_name == SELF_MOD_NAME || mod_name.len() == 0 {
                return false; //Illegal names for a module
            }
            parent_node.children.insert(mod_name.to_string(), Self::new(mod_id));
            true
        } else {
            false
        }
    }

    /// Returns the [ModId] of a module name/path, or None if it can't be resolved
    pub fn resolve(&self, name: &str) -> Option<ModId> {
        self.name_to_node(name).map(|node| node.mod_id)
    }

    /// Resolves a module name/path within &self
    pub fn name_to_node(&self, name: &str) -> Option<&Self> {
        if let Some((parent_node, mod_name)) = self.parse_parent(name) {
            if mod_name == TOP_MOD_NAME || name.len() == 0 {
                Some(self)
            } else {
                parent_node.children.get(mod_name)
            }
        } else {
            None
        }
    }

    /// Returns the node corresponding to any part of the node before the last separator character,
    /// and the remaining substring
    pub fn parse_parent<'a, 'b>(&'a self, name: &'b str) -> Option<(&'a Self, &'b str)> {
        Self::parse_parent_generic(self, |node, name| node.children.get(name), name)
    }

    /// Same behavior as [parse_parent], but takes and returns a mutable reference
    pub fn parse_parent_mut<'a, 'b>(&'a mut self, name: &'b str) -> Option<(&'a mut Self, &'b str)> {
        Self::parse_parent_generic(self, |node, name| node.children.get_mut(name), name)
    }

    /// Internal generic `parse_parent` that can expand to mutable and const versions
    fn parse_parent_generic<'a, SelfT, GetF: Fn(SelfT, &str) -> Option<SelfT>>(gen_self: SelfT, get_f: GetF, name: &'a str) -> Option<(SelfT, &'a str)> {
        if name.len() == 0 {
            return Some((gen_self, &name))
        }
        let mut cur_node = gen_self;
        let mut sym_start = 0;
        for (idx, the_char) in name.char_indices() {
            match the_char {
                MOD_NAME_SEPARATOR => {
                    let sym = &name[sym_start..idx];
                    if sym == TOP_MOD_NAME && sym_start == 0 {
                        sym_start = idx+1;
                        continue;
                    }
                    match get_f(cur_node, sym) {
                        Some(new_node) => {
                            cur_node = new_node;
                            sym_start = idx+1;
                        },
                        None => return None
                    }
                },
                _ => {},
            }
        }
        if sym_start < name.len() {
            Some((cur_node, &name[sym_start..]))
        } else {
            None
        }
    }
}

/// Returns `true` if a str is a legal name for a module
///
/// A module name must be an ascii string, containing only alpha-numeric characters plus [`_`, '.', `-`]
pub(crate) fn module_name_is_legal(name: &str) -> bool {
    for the_char in name.chars() {
        if !the_char.is_ascii() {
            return false;
        }
        if !the_char.is_ascii_alphanumeric() &&
            the_char != '-' &&
            the_char != '_' &&
            the_char != '.' {
            return false;
        }
    }
    return true;
}

#[test]
fn name_parse_test() {
    let mut top = ModNameNode::top();

    assert_eq!(true, top.add("top:sub1", ModId(1)));
    assert_eq!(true, top.add("sub2", ModId(2)));
    assert_eq!(true, top.add("sub2:suba", ModId(3)));
    assert_eq!(true, top.add("sub2:suba:subA", ModId(4)));
    assert_eq!(true, top.add("top:sub1:subb", ModId(5)));

    assert_eq!(false, top.add("", ModId(6)));
    assert_eq!(false, top.add("top", ModId(6)));
    assert_eq!(false, top.add("sub2::suba:subB", ModId(7)));
    assert_eq!(false, top.add("sub2:suba:subB:", ModId(7)));

    assert_eq!(top.name_to_node("top").unwrap().mod_id, top.mod_id);
    assert_eq!(top.name_to_node("").unwrap().mod_id, top.mod_id);
    assert!(top.name_to_node("top:").is_none());
    assert!(top.name_to_node(":").is_none());

    assert_eq!(top.name_to_node("sub1").unwrap().mod_id, ModId(1));
    assert_eq!(top.name_to_node("top:sub2:suba:subA").unwrap().mod_id, ModId(4));
    assert!(top.name_to_node("sub2:suba:subA:").is_none());
    assert!(top.name_to_node("sub1:suba").is_none());

}

#[derive(Debug)]
struct OuterLoader;

impl ModuleLoader for OuterLoader {
    fn name(&self) -> Result<String, String> {
        Ok("outer".to_string())
    }
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
    fn name(&self) -> Result<String, String> {
        Ok("inner".to_string())
    }
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        let parser = SExprParser::new("inner-module-test-atom");
        context.push_parser(Box::new(parser));

        Ok(())
    }
}

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

//LP-TODO-NEXT, make a unit test for relative imports using the module hierarchical namespace.
//
// First, make relative loading and resolution work, then use relative loading in the "RecursiveOuterLoader"
// to load a sub-module,  So the test would be to run "!(import! &self outer:inner)", and have it
// do the right thing.
//

//LP-TODO-NEXT, make a unit test for recursive loading of parents modules based on hierarchical name import
//
// Next make sure the catalogs are able to do the recursive loading from the file system,
// using their working dirs.  Maybe make this second test a C API test to get better coverage
//

//LP-TODO-NEXT, when a catalog resolves a module in a relative location (specifically parent module resource dir),
// it should load that module into a relative module path, unless another path was explicitly specified in
// the API call.  Make a test, to make sure this works.

