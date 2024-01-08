
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;

use crate::*;
use crate::space::DynSpace;
use crate::metta::*;
use crate::metta::runner::{Metta, ModId, RunnerState};
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

mod catalog;
pub use catalog::*;

/// A data structure that uniquely identifies an exact version of a module with a particular provenance
///
/// If two modules have the same ModuleDescriptor, they are considered to be the same module
///
/// NOTE: It is possible for a module to have both a version and a uid.  This means two copies of the
/// same module may be loaded into the same runner independently.  Module version uniqueness is enforced
/// by the catalog(s)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModuleDescriptor {
    name: String,
    uid: Option<u64>,
    //TODO: version
}

impl ModuleDescriptor {
    /// Internal method to create a ModuleDescriptor for a runner's "top" module
    pub(crate) fn top() -> Self {
        Self::new("top".to_string())
    }
    /// Create a new ModuleDescriptor
    pub fn new(name: String) -> Self {
        Self { name, uid: None }
    }
    /// Create a new ModuleDescriptor
    pub fn new_with_uid(name: String, uid: u64) -> Self {
        Self { name, uid: Some(uid) }
    }
    /// Returns the name of the module represented by the ModuleDescriptor
    pub fn name(&self) -> &str {
        &self.name
    }
    /// Internal.  Use the Hash trait to get a uid for the whole ModuleDescriptor
    pub(crate) fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        std::hash::Hash::hash(self, &mut hasher);
        hasher.finish()
    }
}

/// Contains state associated with a loaded MeTTa module
#[derive(Debug)]
pub struct MettaMod {
    descriptor: ModuleDescriptor,
    working_dir: Option<PathBuf>,
    space: DynSpace,
    tokenizer: Shared<Tokenizer>,
    bom: ModuleBom,
}

impl Clone for MettaMod {
    fn clone(&self) -> Self {
        Self {
            descriptor: self.descriptor.clone(),
            space: self.space.clone(),
            tokenizer: self.tokenizer.clone(),
            working_dir: self.working_dir.clone(),
            bom: self.bom.clone(),
        }
    }
}

impl MettaMod {

    /// Internal method to initialize an empty MettaMod
    pub(crate) fn new_with_tokenizer(metta: &Metta, descriptor: ModuleDescriptor, space: DynSpace, tokenizer: Shared<Tokenizer>, working_dir: Option<PathBuf>, no_stdlib: bool) -> Self {
        let new_mod = Self {
            descriptor,
            space,
            tokenizer,
            working_dir,
            bom: ModuleBom::default(),
        };

        // Load the base tokens for the module's new Tokenizer
        register_runner_tokens(&mut *new_mod.tokenizer().borrow_mut(), new_mod.tokenizer().clone(), &new_mod.space, metta);
        register_common_tokens(&mut *new_mod.tokenizer().borrow_mut(), new_mod.tokenizer().clone(), metta);

        //Load the stdlib unless this module is no_std
        if !no_stdlib {
            if let Some(stdlib_mod_id) = metta.0.stdlib_mod.get() {
                new_mod.import_dependency_legacy(&metta, *stdlib_mod_id).unwrap();
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
                None => context.module().descriptor().name.clone()
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
    // "!(import! Number from Arithmetic)" or whatever, and get all the Tokenizer patters that parse
    // numbers?
    pub fn import_item_from_dependency_as(&self, metta: &Metta, from_name: &str, mod_id: ModId, name: Option<&str>) -> Result<(), String> {

        // Get the space and tokenizer associated with the dependent module
        let mut runner_state = RunnerState::new_with_module(metta, mod_id);
        let (dep_space, dep_tokenizer, src_mod_name) = runner_state.run_in_context(|context| {
            let dep_space = context.module().space().clone();
            let dep_tokenizer = context.module().tokenizer().clone();
            let src_mod_name = context.module().descriptor().name.clone();
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

    /// Effectively adds all atom in a dependency module to the &self module, by adding the dependency
    /// module's space as an atom inside the &self module
    ///
    //QUESTION: What do we do about tokenizer entries?  Currently they are not imported at all, but this
    // is certainly a problem
    //
    //QUESTION: Do we want to keep the behavior of the interpreter to flatten Space atoms contained within
    // a space?  This might be undesirable. 
    //
    //QUESTION: Depending on the answers to the above question(s) How do we prevent duplication of
    // transitive imports?  For example, consider: ModA imports ModOne and ModTwo.  ModOne imports ModB. 
    // ModTwo also imports ModB. How do we make sure ModA doesn't end up with duplicate definitions for
    // everything in ModB?
    //
    //This is usually solved in other languages using some combination of strategies.
    // 1.) Keep loaded modules contained within their own name-spaces. And allow aliasing between a parent's
    //  space and a child module's space.  This is not really sufficient for us because we also want to
    //  import Tokenizer entries and their associated functionality, such as the ability to parse integers
    // 2.) Allow a conditional "import-unique" brute-force import which is tantamount to inlining the dependency
    //  at the point of the import statement.  e.g. `#include` in C., but with the "ifdef" guards to make sure
    //  the same header isn't inlined multiple times.  We may want to offer this functionality, but I assume
    //  we'll want to offer this in addition to a more hygenic module system structure, given that most languages
    //  (Python, JavaScript, even Perl!) that started off with brute-force imports have retrofitted more
    //  sophisticated dependency management as they have matured
    // 3.) Force a module to be explicit about what can be exported from the module.  e.g. the `pub` visibility
    //  qualifiers in Rust, etc.
    //
    //Personally, I feel like 3. (being explicit about exported items) is the cleanest solution and provides
    //  the most flexibility into the future (such as the ability to unload modules, etc.)
    //
    pub fn import_all_from_dependency(&self, metta: &Metta, mod_id: ModId) -> Result<(), String> {

        // Get the space associated with the dependent module
        let mut runner_state = RunnerState::new_with_module(metta, mod_id);
        let dep_space = runner_state.run_in_context(|context| {
            let dep_space = context.module().space().clone();
            Ok(dep_space)
        })?;

        // Add a new Grounded Space atom to the &self space, so we can access the dependent module
        let dep_space_atom = Atom::gnd(dep_space);
        self.add_atom(dep_space_atom, false).map_err(|a| a.to_string())?;

        Ok(())
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

    /// Implements the prior import behavior of importing all atoms into a sub-space, loaded into the
    /// &self Space, and merging all tokens
    //TODO: This method is a stop-gap, until we figure out all the details of the import behavior we want
    pub fn import_dependency_legacy(&self, metta: &Metta, mod_id: ModId) -> Result<(), String> {
        self.import_all_from_dependency(metta, mod_id)?;
        self.import_all_tokens_from_dependency(metta, mod_id)
    }

    pub fn descriptor(&self) -> &ModuleDescriptor {
        &self.descriptor
    }

    pub fn bom(&self) -> &ModuleBom {
        &self.bom
    }

    pub fn space(&self) -> &DynSpace {
        &self.space
    }

    pub fn tokenizer(&self) -> &Shared<Tokenizer> {
        &self.tokenizer
    }

    pub fn working_dir(&self) -> Option<&Path> {
        self.working_dir.as_ref().map(|p| p.as_ref())
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

