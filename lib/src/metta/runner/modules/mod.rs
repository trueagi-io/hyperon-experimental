
use std::path::{Path, PathBuf};
use std::collections::HashMap;

use crate::metta::*;
use crate::metta::runner::*;

use regex::Regex;

#[cfg(not(feature = "minimal"))]
use super::stdlib::*;

#[cfg(feature = "minimal")]
use super::interpreter_minimal::interpret;
#[cfg(feature = "minimal")]
use super::stdlib_minimal::*;

#[cfg(feature = "pkg_mgmt")]
pub mod catalog;
#[cfg(feature = "pkg_mgmt")]
use catalog::*;

mod mod_names;
pub(crate) use mod_names::{ModNameNode, mod_name_from_path, normalize_relative_module_name, module_name_is_legal, remove_common_prefix, ModNameNodeDisplayWrapper};
pub use mod_names::{TOP_MOD_NAME, SELF_MOD_NAME, MOD_NAME_SEPARATOR};

/// A reference to a [MettaMod] that is loaded into a [Metta] runner
//
//NOTE: I don't love exposing the internals of ModId, but because the C bindings are in a separate crate
// it was a choice between that and using an unnecessary box
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModId(pub usize);

impl ModId {
    /// An invalid ModId that doesn't point to any loaded module
    pub const INVALID: ModId = ModId(usize::MAX);

    /// An reserved ModId for the runner's top module
    pub const TOP: ModId = ModId(0);

    pub(crate) fn new_relative(idx: usize) -> Self {
        //Set the highest bit to 1 to indicate a relative ID
        Self((!(usize::MAX >> 1)) | idx)
    }
    pub(crate) fn get_idx_from_relative(self) -> usize {
        self.0 & (usize::MAX >> 1)
    }
    pub(crate) fn is_relative(self) -> bool {
        self.0 & (!(usize::MAX >> 1)) > 0
    }
}

/// Contains state associated with a loaded MeTTa module
#[derive(Debug)]
pub struct MettaMod {
    mod_path: String,
    resource_dir: Option<PathBuf>,
    space: DynSpace,
    tokenizer: Shared<Tokenizer>,
    imported_deps: Mutex<HashMap<ModId, DynSpace>>,
    loader: Option<Box<dyn ModuleLoader>>,
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
            loader: None,
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

    /// Internal method to store the loader with its module, for resource access later on
    pub(crate) fn set_loader(&mut self, loader: Box<dyn ModuleLoader>) {
        self.loader = Some(loader);
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

    /// Returns the full path of a loaded module.  For example: "top:parent_mod:this_mod"
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

    pub fn get_resource(&self, res_key: ResourceKey) -> Result<Vec<u8>, String> {
        if let Some(loader) = &self.loader {
            loader.get_resource(res_key)
        } else {
            Err(format!("module resource loader not available"))
        }
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

pub(crate) struct ModuleInitFrame {
    /// The new module will get this name
    new_mod_name: Option<String>,
    /// The module, if we have initialized it
    pub the_mod: Option<MettaMod>,
    /// Names of the sub-modules, relative to the self mod
    pub sub_module_names: ModNameNode,
    /// Sub-modules, indexed by ModuleDescriptor
    module_descriptors: HashMap<ModuleDescriptor, ModId>,
    /// Any child modules in the process of loading
    pub sub_modules: Vec<Self>,
}

impl ModuleInitFrame {

    /// Returns ModuleInitFrame after initializing a module with the provided loader
    ///
    /// The init function will then call `context.init_self_module()` along with any other initialization code
    pub fn init_module(runner: &Metta, mod_name: &str, loader: Box<dyn ModuleLoader>) -> Result<Self, String> {

        //Create a new RunnerState in order to initialize the new module, and push the init function
        // to run within the new RunnerState.  The init function will then call `context.init_self_module()`
        let mut runner_state = RunnerState::new_internal(runner, Some(mod_name));
        runner_state.run_in_context(|context| {
            context.push_func(|context| loader.load(context));
            Ok(())
        })?;

//GOAT Ugh.  Somehow we need to create the frame in this function and attach it to this frame as a sub-module

        //Finish the execution
        while !runner_state.is_complete() {
            runner_state.run_step()?;
        }

        //Set the loader on the module, so its resource can be accessed later
        let mut frame = runner_state.into_init_frame()?;
        frame.try_borrow_mod_mut().unwrap().set_loader(loader);
        Ok(frame)
    }

    /// Creates a new ModuleInitFrame with a new module name.  Make sure this is normalized
    pub fn new_with_name(new_mod_name: String) -> Self {
        Self {
            new_mod_name: Some(new_mod_name),
            the_mod: None,
            sub_module_names: ModNameNode::new(ModId::INVALID),
            module_descriptors: HashMap::new(),
            sub_modules: vec![],
        }
    }
    pub fn try_borrow_mod(&self) -> Option<&MettaMod> {
        self.the_mod.as_ref()
    }
    pub fn try_borrow_mod_mut(&mut self) -> Option<&mut MettaMod> {
        self.the_mod.as_mut()
    }
    pub fn path(&self) -> &str {
        match &self.the_mod {
            Some(the_mod) => the_mod.path(),
            None => {
                match &self.new_mod_name {
                    Some(name) => name.as_str(),
                    None => panic!("Internal Error")
                }
            }
        }
    }
    pub fn init_self_module(&mut self, metta: &Metta, space: DynSpace, resource_dir: Option<PathBuf>) {
        if self.the_mod.is_some() {
            panic!("Module already initialized")
        }
        let tokenizer = Shared::new(Tokenizer::new());
        let mod_name = core::mem::take(&mut self.new_mod_name).unwrap();
        self.the_mod = Some(MettaMod::new_with_tokenizer(metta, mod_name, space, tokenizer, resource_dir, false));
    }
    /// Locates and retrieves a loaded module, or a sub-module relative to the module being loaded
    pub(crate) fn get_module_by_name(&self, runner: &Metta, mod_name: &str) -> Result<ModId, String> {
        let self_mod_path = self.path();
        let mod_name = normalize_relative_module_name(self_mod_path, mod_name)?;
        let mod_id = {
            let module_names = runner.0.module_names.lock().unwrap();
            module_names.resolve_layered(&[(self_mod_path, &self.sub_module_names)], &mod_name).ok_or_else(|| format!("Unable to locate module: {mod_name}"))
        }?;
        if mod_id == ModId::INVALID {
            Err(format!("Attempt to resolve module that is not yet loaded: {mod_name}"))
        } else {
            Ok(mod_id)
        }
    }

    pub fn merge_child_frame(&mut self, child_frame: Self) -> Result<ModId, String> {
        let module = child_frame.the_mod
            .ok_or_else(|| "Fatal Error: Module loader function exited without calling RunContext::init_self_module".to_string())?;

        //TODO-NOW, gotta add the child mods from the frame and remap the child mod indices
        //TODO-NOW, also gotta merge the child descriptors

        let adjusted_path = remove_common_prefix(module.path(), self.path()).to_owned();
        self.sub_module_names.merge_subtree_into(&adjusted_path, child_frame.sub_module_names)?;

        let mod_id = self.add_module(module)?;
        self.sub_module_names.add(&adjusted_path, mod_id)?;
        Ok(mod_id)
    }

    /// Internal method to add a MettaMod to the ModuleInitFrame, assigning it a temporary ModId
    fn add_module(&mut self, module: MettaMod) -> Result<ModId, String> {
        let new_idx = self.sub_modules.len();
//GOAT, this needs to work...
        // self.sub_modules.push(Rc::new(module));
        Ok(ModId::new_relative(new_idx))
    }

    /// Adds a sub-module to this module's subtree if a relative path was specified.  Otherwise adds
    /// the sub-module to the runner's main tree
    pub(crate) fn add_module_to_name_tree(&mut self, runner: &Metta, mod_name: &str, mod_id: ModId) -> Result<(), String> {
        //NOTE: impl of Self::path is duplicated here so we can split borrow. :-(
        let mut self_mod_path = match &self.the_mod {
            Some(the_mod) => the_mod.path(),
            None => {
                match &self.new_mod_name {
                    Some(name) => name.as_str(),
                    None => panic!("Internal Error")
                }
            }
        };
        let mod_name = normalize_relative_module_name(self_mod_path, mod_name)?;
        let subtree = &mut self.sub_module_names;
        let mut module_names = runner.0.module_names.lock().unwrap();
        module_names.add_to_layered(&mut[(&mut self_mod_path, subtree)], &mod_name, mod_id)
    }

    pub(crate) fn add_module_descriptor(&mut self, descriptor: ModuleDescriptor, mod_id: ModId) {
        self.module_descriptors.insert(descriptor, mod_id);
    }

    //TODO-NOW, delete this function
    // //we want to iterate the sub-modules and take the module and ModNameNode out together
    // // as we dismantle the frame to merge it into the runner
    // /// Internal method as part of the loading process, caller takes ownership of sub-module hierarchiy
    // /// to merge it into the runner's name hierarchy as part of loading a MettaMod into a runner
    // pub(crate) fn take_sub_module_names(&mut self) -> Option<ModNameNode> {
    //     //core::mem::take(&mut self.sub_module_names)
    //     Some(self.sub_module_names.clone())
    // }

    //TODO-NOW, delete this function
    // pub fn into_module(self) -> Result<(MettaMod, ModNameNode), String> {
    //     assert!(self.sub_modules.len() == 0);
    //     match self.the_mod {
    //         Some(the_mod) => Ok((the_mod, self.sub_module_names)),
    //         None => Err("Fatal Error: Module loader function exited without calling RunContext::init_self_module".to_string())
    //     }
    // }
}

/// Implemented to supply a loader functions for MeTTa modules
///
/// A ModuleLoader is responsible for loading a MeTTa module through the API.  Implementations of
/// ModuleLoader can be used to define a module format or to supply programmatically defined modules
pub trait ModuleLoader: std::fmt::Debug + Send + Sync {
    /// A function to load the module my making MeTTa API calls.  This function will be called
    /// as a downstream consequence of [Metta::load_module_at_path], [Metta::load_module_direct],
    /// [RunContext::load_module], or any other method that leads to the loading of modules
    fn load(&self, context: &mut RunContext) -> Result<(), String>;

    /// Returns a data blob containing a given named resource belonging to a module
    fn get_resource(&self, _res_key: ResourceKey) -> Result<Vec<u8>, String> {
        Err("resource not found".to_string())
    }
}

/// Identifies a resource to retrieve from a [ModuleLoader]
///
/// NOTE: Some resources may not be available from some modules or ModuleLoaders
pub enum ResourceKey<'a> {
    /// The MeTTa code for the module in S-Expression format, if the module is
    /// implemented as MeTTa code.
    ///
    /// NOTE: there is no guarantee the code in the `module.metta` resource will work outside
    /// the module's context.  This use case must be supported by each module individually.
    MainMettaSrc,
    /// A list of people or organizations responsible for the module **TODO**
    Authors,
    /// A short description of the module **TODO**
    Description,
    /// A custom identifier, to be interpreted by the [ModuleLoader] implementation
    Custom(&'a str)
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
    let result = runner.load_module_direct(Box::new(InnerLoader), "outer:inner");
    assert!(result.is_err());

    //Make sure we can load sub-modules sucessfully
    let _outer_mod_id = runner.load_module_direct(Box::new(OuterLoader), "outer").unwrap();
    let _inner_mod_id = runner.load_module_direct(Box::new(InnerLoader), "outer:inner").unwrap();

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

        let _inner_mod_id = context.load_module_direct(Box::new(InnerLoader), "self:inner").unwrap();

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
    let runner = Metta::new(Some(EnvBuilder::test_env()));

    // LP-TODO-NOW: This test curently fails for reasons explained in the comment inside Metta::merge_sub_module_names.

//GOAT, gotta test at least 3 levels

    //Load the "outer" module, which will load the inner module as part of its loader
    let _outer_mod_id = runner.load_module_direct(Box::new(RelativeOuterLoader), "outer").unwrap();

    // runner.display_loaded_modules();

    //Make sure we didn't accidentally load "inner" at the top level
    assert!(runner.get_module_by_name("inner").is_err());

    //Confirm we didn't end up with a module called "self"
    assert!(runner.get_module_by_name("self:inner").is_err());
    assert!(runner.get_module_by_name("self").is_err());

    //Now make sure we can actually resolve the loaded sub-module
    runner.get_module_by_name("outer:inner").unwrap();

    //LP-TODO-NEXT, test that I can add a second inner from the runner, by adding "top:outer:inner2",
    // and then that I can import it directly into "outer" from within the runner's context using the "self:inner2" mod path

}

//LP-TODO-NEXT,  Make a test for an inner-loader that throws an error, blocking the outer-loader from loading sucessfully,
// and make sure neither module is loaded into the named index
//
//Also test the case where the inner loader is sucessul, but then the outer loader throws an error.  Also make sure neither
// module is loaded into the namespace
//
