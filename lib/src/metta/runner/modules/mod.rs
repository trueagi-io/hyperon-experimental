
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::cell::RefCell;

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
pub(crate) use mod_names::{ModNameNode, mod_name_from_path, normalize_relative_module_name, module_name_is_legal, mod_name_remove_prefix, decompose_name_path, compose_name_path, ModNameNodeDisplayWrapper};
pub use mod_names::{TOP_MOD_NAME, SELF_MOD_NAME, MOD_NAME_SEPARATOR};

/// A reference to a [MettaMod] that is loaded into a [Metta] runner
//
//NOTE: I don't love exposing the internals of ModId, but because the C bindings are in a separate crate
// it was a choice between that and using an unnecessary box
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModId(pub usize);

impl Default for ModId {
    fn default() -> Self {
        ModId::INVALID
    }
}

impl ModId {
    /// An invalid ModId that doesn't point to any loaded module
    pub const INVALID: ModId = ModId(usize::MAX);

    /// An reserved ModId for the runner's top module
    pub const TOP: ModId = ModId(0);

    pub(crate) const fn new_relative(idx: usize) -> Self {
        //Set the highest bit to 1 to indicate a relative ID
        Self((!(usize::MAX >> 1)) | idx)
    }
    pub(crate) const fn get_idx_from_relative(self) -> usize {
        self.0 & (usize::MAX >> 1)
    }
    pub(crate) const fn is_relative(self) -> bool {
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
                new_mod.import_all_from_dependency(*stdlib_mod_id, metta.get_mod_ptr(*stdlib_mod_id)).unwrap();
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
    pub(crate) fn import_dependency_as(&self, mod_ptr: Rc<MettaMod>, name: Option<String>) -> Result<(), String> {

        // Get the space and name associated with the dependent module
        let dep_space = mod_ptr.space().clone();
        let name = match name {
            Some(name) => name,
            None => mod_ptr.name().to_string()
        };

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
    pub(crate) fn import_item_from_dependency_as(&self, from_name: &str, mod_ptr: Rc<MettaMod>, name: Option<&str>) -> Result<(), String> {

        // Get the space and tokenizer associated with the dependent module
        let dep_space = mod_ptr.space().clone();
        let dep_tokenizer = mod_ptr.tokenizer().clone();
        let src_mod_name = mod_ptr.path().to_string();

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
    pub(crate) fn import_all_from_dependency(&self, mod_id: ModId, mod_ptr: Rc<MettaMod>) -> Result<(), String> {

        // See if the dependency has already been imported
        if self.contains_imported_dep(&mod_id) {
            return Ok(())
        }

        // Get the space associated with the dependent module
        log::info!("import_all_from_dependency: importing from {} into {}", mod_ptr.path(), self.path());
        let (dep_space, transitive_deps) = mod_ptr.stripped_space();

        // Add a new Grounded Space atom to the &self space, so we can access the dependent module
        self.insert_dep(mod_id, dep_space)?;

        // Add all the transitive deps from the dependency
        if let Some(transitive_deps) = transitive_deps {
            for (dep_mod_id, dep_space) in transitive_deps {
                self.insert_dep(dep_mod_id, dep_space)?;
            }
        }

        // Finally, Import the tokens from the dependency
        self.import_all_tokens_from_dependency(mod_ptr)
    }

    /// Merges all Tokenizer entries in a dependency module into &self
    pub(crate) fn import_all_tokens_from_dependency(&self, mod_ptr: Rc<MettaMod>) -> Result<(), String> {

        // Get the tokenizer associated with the dependent module
        let dep_tokenizer = mod_ptr.tokenizer().clone();

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

    /// Private method to iterate a module's imported_deps and replace a reference to one ModId
    /// with another from a map.
    pub(crate) fn remap_imported_deps(&self, mapping: &HashMap<ModId, ModId>) {
        let mut deps = self.imported_deps.lock().unwrap();
        let mut temp = HashMap::with_capacity(deps.len());
        core::mem::swap(&mut temp, &mut *deps);
        for (dep_mod_id, space) in temp.into_iter() {
            let new_mod_id = match mapping.get(&dep_mod_id) {
                Some(mapped_id) => *mapped_id,
                None => dep_mod_id,
            };
            deps.insert(new_mod_id, space);
        }
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

/// ModuleInitState is a smart-pointer to a vec of [ModuleInitFrame]
pub(crate) enum ModuleInitState {
    /// Meaning: The RunnerState holding this pointer is not initializing modules
    None,
    /// Meaning: The RunnerState holding this pointer is the "top" of a module init process
    /// When the init function is finished, all of the InitFrames will be merged into the runner
    Root(Rc<RefCell<ModuleInitStateInsides>>),
    /// Meaning: The RunnerState holding this pointer is nested within a module init process
    /// When the init function is finished, the pointer will be simply dropped
    Child(Rc<RefCell<ModuleInitStateInsides>>)
}

pub(crate) struct ModuleInitStateInsides {
    frames: Vec<ModuleInitFrame>,
    module_descriptors: HashMap<ModuleDescriptor, ModId>,
}

impl Clone for ModuleInitState {
    fn clone(&self) -> Self {
        match self {
            Self::None => Self::None,
            Self::Root(rc) |
            Self::Child(rc) => Self::Child(rc.clone())
        }
    }
}

impl ModuleInitState {
    pub fn empty() -> Self {
        Self::None
    }
    pub fn new(mod_name: String) -> (Self, ModId) {
        let new_insides = ModuleInitStateInsides {
            frames: vec![ModuleInitFrame::new_with_name(mod_name)],
            module_descriptors: HashMap::new(),
        };
        let init_state = Self::Root(Rc::new(RefCell::new(new_insides)));
        (init_state, ModId::new_relative(0))
    }
    pub fn push(&mut self, mod_name: String) -> ModId {
        match self {
            Self::None => {
                let (new_state, new_id) = Self::new(mod_name);
                *self = new_state;
                new_id
            },
            Self::Root(cell) |
            Self::Child(cell) => {
                let mut insides_ref = cell.borrow_mut();
                let new_idx = insides_ref.frames.len();
                insides_ref.frames.push(ModuleInitFrame::new_with_name(mod_name));
                ModId::new_relative(new_idx)
            }
        }
    }
    pub fn is_root(&self) -> bool {
        match self {
            Self::Root(_) => true,
            _ => false
        }
    }
    pub fn decompose(self) -> (Vec<ModuleInitFrame>, HashMap<ModuleDescriptor, ModId>) {
        match self {
            Self::Root(cell) => {
                let mut insides_ref = cell.borrow_mut();
                let frames = core::mem::take(&mut insides_ref.frames);
                let descriptors = core::mem::take(&mut insides_ref.module_descriptors);
                (frames, descriptors)
            },
            _ => unreachable!()
        }
    }

    /// Internal method to retrieve the mod_ptr to a module that's either loading in the
    /// InitFrame, or loaded into the runner
    pub fn get_mod_ptr(&self, metta: &Metta, mod_id: ModId) -> Result<Rc<MettaMod>, String> {
        if mod_id.is_relative() {
            let frame_idx = mod_id.get_idx_from_relative();
            match &self {
                Self::Root(cell) |
                Self::Child(cell) => {
                    let insides_ref = cell.borrow();
                    match &insides_ref.frames.get(frame_idx).unwrap().the_mod {
                        Some(the_mod) => Ok(the_mod.clone()),
                        None => Err(format!("Attempt to access module before loader function has finished"))
                    }
                },
                Self::None => unreachable!()
            }
        } else {
            Ok(metta.get_mod_ptr(mod_id))
        }
    }

    /// Locates and retrieves a loaded module, or a sub-module relative to the module being loaded
    pub fn get_module_by_name(&self, runner: &Metta, mod_name: &str) -> Result<ModId, String> {
        let mod_id = match self {
            Self::Root(cell) |
            Self::Child(cell) => {
                let insides_ref = cell.borrow();
                let mut subtree_pairs = vec![];
                for frame in insides_ref.frames.iter() {
                    subtree_pairs.push((frame.path(), &frame.sub_module_names));
                }
                let module_names = runner.0.module_names.lock().unwrap();
                module_names.resolve_layered(&subtree_pairs[..], mod_name).ok_or_else(|| format!("Unable to locate module: {mod_name}"))
            },
            Self::None => runner.get_module_by_name(mod_name)
        }?;

        if mod_id == ModId::INVALID {
            Err(format!("Attempt to resolve module that is not yet loaded: {mod_name}"))
        } else {
            Ok(mod_id)
        }
    }

    pub fn add_module_to_name_tree(&self, runner: &Metta, frame_id: ModId, mod_name: &str, mod_id: ModId) -> Result<(), String> {
        match self {
            Self::Root(_) |
            Self::Child(_) => self.in_frame(frame_id, |frame| frame.add_module_to_name_tree(mod_name, mod_id)),
            _ => runner.add_module_to_name_tree(mod_name, mod_id)
        }
    }

    /// Runs the provided function in the context of the frame specified by `frame_mod`
    pub fn in_frame<R, F: FnOnce(&mut ModuleInitFrame)->R>(&self, frame_mod: ModId, func: F) -> R {
        match self {
            Self::Root(cell) |
            Self::Child(cell) => {
                let mut insides_ref = cell.borrow_mut();
                let frame_idx = frame_mod.get_idx_from_relative();
                let frame = insides_ref.frames.get_mut(frame_idx).unwrap();
                func(frame)
            },
            _ => unreachable!()
        }
    }

    /// Returns the ModId after initializing a module with the provided loader
    ///
    /// The init function will then call `context.init_self_module()` along with any other initialization code
    pub fn init_module(&mut self, runner: &Metta, mod_name: &str, loader: Box<dyn ModuleLoader>) -> Result<ModId, String> {

        //Create a new RunnerState in order to initialize the new module, and push the init function
        // to run within the new RunnerState.  The init function will then call `context.init_self_module()`
        let mut runner_state = RunnerState::new_for_loading(runner, mod_name, self);
        runner_state.run_in_context(|context| {
            context.push_func(|context| loader.load(context));
            Ok(())
        })?;

        //Finish the execution
        while !runner_state.is_complete() {
            runner_state.run_step()?;
        }
        let mod_id = runner_state.finalize_loading()?;

        //Set the loader on the module, so its resource can be accessed later
        self.in_frame(mod_id, |frame| Rc::get_mut(frame.the_mod.as_mut().unwrap()).unwrap().set_loader(loader));

        Ok(mod_id)
    }

    pub fn get_module_with_descriptor(&self, runner: &Metta, descriptor: &ModuleDescriptor) -> Option<ModId> {
        match self {
            Self::Root(cell) |
            Self::Child(cell) => {
                let insides_ref = cell.borrow_mut();
                match insides_ref.module_descriptors.get(descriptor) {
                    Some(mod_id) => Some(mod_id.clone()),
                    None => runner.get_module_with_descriptor(descriptor)
                }
            },
            Self::None => {
                runner.get_module_with_descriptor(descriptor)
            }
        }
    }

    pub fn add_module_descriptor(&self, runner: &Metta, descriptor: ModuleDescriptor, mod_id: ModId) {
        match self {
            Self::Root(cell) |
            Self::Child(cell) => {
                let mut insides_ref = cell.borrow_mut();
                insides_ref.module_descriptors.insert(descriptor, mod_id);
            },
            Self::None => {
                runner.add_module_descriptor(descriptor, mod_id);
            }
        }
    }

}

pub(crate) struct ModuleInitFrame {
    /// The new module will get this name
    pub new_mod_name: Option<String>,
    /// The new module, after the init has finished
    pub the_mod: Option<Rc<MettaMod>>,
    /// Names of additional sub-modules loaded for this frame, relative to `self::path`
    pub sub_module_names: ModNameNode,
}

impl ModuleInitFrame {
    /// Creates a new ModuleInitFrame with a new module name.  Make sure this is normalized
    pub fn new_with_name(new_mod_name: String) -> Self {
        Self {
            new_mod_name: Some(new_mod_name),
            the_mod: None,
            sub_module_names: ModNameNode::new(ModId::INVALID),
        }
    }
    pub fn path(&self) -> &str {
        match &self.new_mod_name {
            Some(name) => name.as_str(),
            None => self.the_mod.as_ref().unwrap().path()
        }
    }
    pub fn init_self_module(&mut self, self_mod_id: ModId, metta: &Metta, space: DynSpace, resource_dir: Option<PathBuf>) -> Rc<MettaMod> {
        let tokenizer = Shared::new(Tokenizer::new());
        let mod_name = self.new_mod_name.clone().unwrap();
        let new_mod = Rc::new(MettaMod::new_with_tokenizer(metta, mod_name, space, tokenizer, resource_dir, false));
        self.sub_module_names.update("top", self_mod_id).unwrap();
        new_mod
    }
    /// Adds a sub-module to this module's subtree
    //
    //DISCUSSION: Should a module loader be able to load modules into its parents?
    // The argument for No is mainly hygene and isolation.  The argument for Yes is convenience.
    //
    // Currently this method implements the No behavior.  This is mostly because I feel that is
    // correct, but partially because the Yes behavior raises an annoying paradox.
    // If we wanted to implement the Yes behavior, we would want to assemble a layered tree from
    // all the InitFrames, and use [ModNameNode::add_to_layered] to add the new node.  However,
    // the added module now belongs to the node it was placed in.  So if the disjoint sub-module
    // that added it went on to fail loading, the sub-module wouldn't get cleaned up.  Same goes
    // for modules imported into the runner directly.  So the question of ownership over the module
    // name-space gets a lot trickier if modules are allowed to add sub-modules outside themselves
    pub(crate) fn add_module_to_name_tree(&mut self, mod_name: &str, mod_id: ModId) -> Result<(), String> {
        let self_mod_path = &self.new_mod_name.as_ref().unwrap();
        match mod_name_remove_prefix(mod_name, &self_mod_path) {
            Some(sub_mod_name) => {
                if sub_mod_name.len() == 0 {
                    return Err(format!("Attempt to load {mod_name} recursively from within its own loader"));
                }
                self.sub_module_names.add(sub_mod_name, mod_id)
            },
            None => return Err(format!("Cannot load module {mod_name} from loader of {self_mod_path}.  Module loaders may only load sub-modules"))
        }
    }
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
        assert!(context.get_module_by_name("self:inner").is_ok());

        Ok(())
    }
}

/// This tests loading a sub-module from another module's runner, using a relative namespace path
#[test]
fn relative_submodule_import_test() {
    let runner = Metta::new(Some(EnvBuilder::test_env()));

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
