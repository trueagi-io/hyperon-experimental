
use std::path::Path;
use std::cell::RefCell;

use crate::metta::runner::*;
use crate::space::module::ModuleSpace;

use regex::Regex;

use super::interpreter::interpret;

use std::mem;

mod mod_names;
pub(crate) use mod_names::{ModNameNode, mod_name_from_path, normalize_relative_module_name, mod_name_remove_prefix, ModNameNodeDisplayWrapper};
#[cfg(feature = "pkg_mgmt")]
pub(crate) use mod_names::{module_name_is_legal, module_name_make_legal, decompose_name_path, compose_name_path};

pub use mod_names::{TOP_MOD_NAME, SELF_MOD_NAME, MOD_NAME_SEPARATOR};
use crate::metta::types::{AtomType, get_atom_types};

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
}

impl MettaMod {

    /// Internal method to initialize an empty MettaMod
    pub(crate) fn new_with_tokenizer(metta: &Metta, mod_path: String, space: DynSpace, tokenizer: Shared<Tokenizer>, resource_dir: Option<PathBuf>, no_stdlib: bool) -> Self {

        //Give the space a name based on the module, if it doesn't already have one
        if let Some(g_space) = space.borrow_mut().as_any_mut().downcast_mut::<GroundingSpace>() {
            if g_space.name().is_none() {
                g_space.set_name(mod_path.clone());
            }
        }
        let space = DynSpace::new(ModuleSpace::new(space));

        let new_mod = Self {
            mod_path,
            space,
            tokenizer,
            imported_deps: Mutex::new(HashMap::new()),
            resource_dir,
            loader: None,
        };
        //Load the stdlib unless this module is no_std
        if !no_stdlib {
            if let Some(corelib_mod_id) = metta.0.corelib_mod.get() {
                if new_mod.name() != "stdlib" {
                    new_mod.import_all_from_dependency(*corelib_mod_id, metta.get_mod_ptr(*corelib_mod_id), metta).unwrap();
                }
            }
            if let Some(stdlib_mod_id) = metta.0.stdlib_mod.get() {
                new_mod.import_all_from_dependency(*stdlib_mod_id, metta.get_mod_ptr(*stdlib_mod_id), metta).unwrap();
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
            self.add_atom(src_atom.clone(), false).expect("Unexpected type check error");

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
    pub(crate) fn import_all_from_dependency(&self, mod_id: ModId, mod_ptr: Rc<MettaMod>, metta: &Metta) -> Result<(), String> {

        // See if the dependency has already been imported
        if self.contains_imported_dep(&mod_id) {
            return Ok(())
        }

        // Get the space associated with the dependent module
        log::debug!("import_all_from_dependency: importing from {} into {}", mod_ptr.path(), self.path());
        let (dep_space, transitive_deps) = mod_ptr.stripped_space();

        // Add a new Grounded Space atom to the &self space, so we can access the dependent module
        self.insert_dep(mod_id, dep_space.clone())?;

        // Add all the transitive deps from the dependency
        if let Some(transitive_deps) = transitive_deps {
            for (dep_mod_id, dep_space) in transitive_deps {
                self.insert_dep(dep_mod_id, dep_space)?;
            }
        }

        // Finally, Import the tokens from the dependency
        match &mod_ptr.loader {
            Some(loader) => loader.load_tokens(self, metta.clone()),
            None => Ok(()), // no tokens are exported by mod_ptr
        }
    }

    /// Returns `true` if the `self` module has imported the `mod_id` module as a sub-dependency
    pub fn contains_imported_dep(&self, mod_id: &ModId) -> bool {
        let deps_table = self.imported_deps.lock().unwrap();
        deps_table.contains_key(&mod_id)
    }

    /// Private function to insert a dependency's space in a grounded atom into a module's space
    fn insert_dep(&self, mod_id: ModId, dep_space: DynSpace) -> Result<(), String> {
        let mut deps_table = self.imported_deps.lock().unwrap();
        if !deps_table.contains_key(&mod_id) {
            match self.space.borrow_mut().as_any_mut().downcast_mut::<ModuleSpace>() {
                Some(s) => s.add_dep(dep_space.clone()),
                None => unreachable!(),
            }
            deps_table.insert(mod_id, dep_space);
        }
        Ok(())
    }

    /// Private method to iterate a module's imported_deps and replace a reference to one ModId
    /// with another from a map.
    pub(crate) fn remap_imported_deps(&self, mapping: &HashMap<ModId, ModId>) {
        let mut deps = self.imported_deps.lock().unwrap();
        let mut temp = HashMap::with_capacity(deps.len());
        mem::swap(&mut temp, &mut *deps);
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
    fn stripped_space(&self) -> (DynSpace, Option<HashMap<ModId, DynSpace>>) {
        let deps_table = self.imported_deps.lock().unwrap();
        (self.space.clone(), Some(deps_table.clone()))
    }

    /// Returns the full path of a loaded module.  For example: "top:parent_mod:this_mod"
    pub fn path(&self) -> &str {
        &self.mod_path
    }

    /// Returns the name of the loaded module.
    pub fn name(&self) -> &str {
        mod_name_from_path(&self.mod_path)
    }

    /// Returns a reference to the module's [PkgInfo], if it has one
    #[cfg(feature = "pkg_mgmt")]
    pub fn pkg_info(&self) -> Option<&PkgInfo> {
        self.loader.as_ref().and_then(|loader| loader.pkg_info())
    }

    pub fn space(&self) -> DynSpace {
        self.space.clone()
    }

    pub fn tokenizer(&self) -> &Shared<Tokenizer> {
        &self.tokenizer
    }

    pub fn resource_dir(&self) -> Option<&Path> {
        self.resource_dir.as_deref()
    }

    pub fn get_resource(&self, res_key: ResourceKey) -> Result<Resource, String> {
        if let Some(loader) = &self.loader {
            loader.get_resource(res_key)
        } else {
            Err(format!("module resource loader not available"))
        }
    }

    /// A convenience to add an an atom to a module's Space, if it passes type-checking
    pub(crate) fn add_atom(&self, atom: Atom, type_check: bool) -> Result<(), Vec<Atom>> {
        if type_check {
            let types = get_atom_types(&self.space, &atom);
            if types.iter().all(AtomType::is_error) {
                return Err(types.into_iter().map(AtomType::into_error_unchecked).collect());
            }
        }
        self.space.borrow_mut().add(atom);
        Ok(())
    }
}

/// ModuleInitState is a smart-pointer to a state that contains some objects to be merged
/// into the runner after module initialization has completed sucessfully
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
    // Successful nested mutations stay here until root publication: an ancestor
    // can still fail. Checkpoints retain only offsets, not copies of metadata.
    undo: Vec<ModuleInitUndo>,
}

struct ModuleInitCheckpoint {
    frames: usize,
    undo: usize,
}

enum ModuleInitUndo {
    Name(usize, mod_names::ModNameUndo),
    #[cfg(feature = "pkg_mgmt")]
    Descriptor(ModuleDescriptor, Option<ModId>),
}

impl ModuleInitState {
    pub fn empty() -> Self {
        Self::None
    }
    pub fn new(mod_name: String) -> (Self, ModId) {
        let new_insides = ModuleInitStateInsides {
            frames: vec![ModuleInitFrame::new_with_name(mod_name)],
            module_descriptors: HashMap::new(),
            undo: Vec::new(),
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
    pub fn new_child(&self) -> Self {
        match self {
            Self::None => Self::None,
            Self::Root(rc) |
            Self::Child(rc) => Self::Child(rc.clone())
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
                let frames = mem::take(&mut insides_ref.frames);
                let descriptors = mem::take(&mut insides_ref.module_descriptors);
                insides_ref.undo.clear();
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
            Self::Root(cell) |
            Self::Child(cell) => {
                let mut insides = cell.borrow_mut();
                let idx = frame_id.get_idx_from_relative();
                let undo = insides.frames[idx].add_module_to_name_tree(mod_name, mod_id)?;
                insides.undo.push(ModuleInitUndo::Name(idx, undo));
                Ok(())
            },
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
        //If the new module fails to load, its transient metadata must not stay in the state,
        // otherwise it could be merged into the runner with the next successfully loaded module
        let checkpoint = self.checkpoint();
        let result = self.init_module_internal(runner, mod_name, loader);
        if result.is_err() {
            self.restore_checkpoint(checkpoint);
        }
        result
    }

    /// Marks the frame and mutation suffix owned by the next initialization.
    fn checkpoint(&self) -> Option<ModuleInitCheckpoint> {
        match self {
            Self::None => None,
            Self::Root(cell) |
            Self::Child(cell) => {
                let insides_ref = cell.borrow();
                Some(ModuleInitCheckpoint {
                    frames: insides_ref.frames.len(),
                    undo: insides_ref.undo.len(),
                })
            },
        }
    }

    /// Rewinds only this attempt's mutations, including aliases written into earlier
    /// surviving frames through `RunContext::in_mod_context`. No callback runs borrowed.
    fn restore_checkpoint(&mut self, checkpoint: Option<ModuleInitCheckpoint>) {
        match checkpoint {
            None => *self = Self::None,
            Some(checkpoint) => match self {
                Self::None => {},
                Self::Root(cell) |
                Self::Child(cell) => {
                    let mut insides_ref = cell.borrow_mut();
                    while insides_ref.undo.len() > checkpoint.undo {
                        match insides_ref.undo.pop().unwrap() {
                            ModuleInitUndo::Name(idx, undo) => {
                                insides_ref.frames[idx].sub_module_names.undo_add(undo);
                            },
                            #[cfg(feature = "pkg_mgmt")]
                            ModuleInitUndo::Descriptor(descriptor, previous) => {
                                match previous {
                                    Some(id) => { insides_ref.module_descriptors.insert(descriptor, id); },
                                    None => { insides_ref.module_descriptors.remove(&descriptor); },
                                }
                            },
                        }
                    }
                    insides_ref.frames.truncate(checkpoint.frames);
                },
            },
        }
    }

    fn init_module_internal(&mut self, runner: &Metta, mod_name: &str, loader: Box<dyn ModuleLoader>) -> Result<ModId, String> {

        //Give the prepare function a chance to run, in case it hasn't yet
        #[cfg(feature = "pkg_mgmt")]
        let loader = match loader.prepare(None, UpdateMode::FetchIfMissing)? {
            Some(new_loader) => new_loader,
            None => loader
        };

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

    #[cfg(feature = "pkg_mgmt")]
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

    #[cfg(feature = "pkg_mgmt")]
    pub fn add_module_descriptor(&self, runner: &Metta, descriptor: ModuleDescriptor, mod_id: ModId) {
        match self {
            Self::Root(cell) |
            Self::Child(cell) => {
                let mut insides_ref = cell.borrow_mut();
                let previous = insides_ref.module_descriptors.insert(descriptor.clone(), mod_id);
                insides_ref.undo.push(ModuleInitUndo::Descriptor(descriptor, previous));
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
        // Only the newly loading frame is initialized. Older contexts already
        // have a mod_ptr and cannot reinitialize; failure discards this frame.
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
    fn add_module_to_name_tree(&mut self, mod_name: &str, mod_id: ModId) -> Result<mod_names::ModNameUndo, String> {
        let self_mod_path = &self.new_mod_name.as_ref().unwrap();
        match mod_name_remove_prefix(mod_name, &self_mod_path) {
            Some(sub_mod_name) => {
                if sub_mod_name.len() == 0 {
                    return Err(format!("Attempt to load {mod_name} recursively from within its own loader"));
                }
                self.sub_module_names.add_with_undo(sub_mod_name, mod_id)
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

    /// A function to access the [PkgInfo] struct of meta-data associated with a module
    ///
    /// NOTE: Requires `pkg_mgmt` feature
    #[cfg(feature = "pkg_mgmt")]
    fn pkg_info(&self) -> Option<&PkgInfo> {
        None
    }

    /// Prepares a module for loading.  This method is responsible for fetching resources
    /// from the network, performing build or pre-computation steps, or any other operations
    /// that only need to be performed once and then may be cached locally
    ///
    /// If this method returns `Ok(Some(_))` then the loader will be dropped and the returned
    /// loader will replace it.
    ///
    /// NOTE: This method may become async in the future
    ///
    /// FUTURE-QUESTION: Should "Fetch" and "Build" be separated? Currently they are lumped
    /// together into one interface but it may make sense to split them into separate entry
    /// points. I will keep them as one until the need arises.
    #[cfg(feature = "pkg_mgmt")]
    fn prepare(&self, _local_dir: Option<&Path>, _update_mode: UpdateMode) -> Result<Option<Box<dyn ModuleLoader>>, String> {
        Ok(None)
    }

    /// Returns a data blob containing a given named resource belonging to a module
    fn get_resource(&self, _res_key: ResourceKey) -> Result<Resource, String> {
        Err("resource not found".to_string())
    }

    /// Loads module's tokens into target module. This method is used for both
    /// initial token loading and exporting module's tokens into importing
    /// module.
    fn load_tokens(&self, _target: &MettaMod, _metta: Metta) -> Result<(), String> {
        Ok(())
    }
}

/// Resource for loading
pub enum Resource {
    /// Resource backed by file
    File(std::io::BufReader<std::fs::File>),
    /// Resource backed by memory buffer
    Bytes(std::io::Cursor<Vec<u8>>),
}

impl From<std::fs::File> for Resource {
    fn from(file: std::fs::File) -> Self {
        Self::File(std::io::BufReader::new(file))
    }
}

impl From<Vec<u8>> for Resource {
    fn from(bytes: Vec<u8>) -> Self {
        Self::Bytes(std::io::Cursor::new(bytes))
    }
}

impl std::io::Read for Resource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Resource::File(file) => file.read(buf),
            Resource::Bytes(bytes) => bytes.read(buf),
        }
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
    /// A [semver compliant](https://semver.org) version string
    Version,
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

#[cfg(test)]
mod test {
    use hyperon_atom::gnd::GroundedFunctionAtom;
    use hyperon_atom::gnd::number::{Number, ATOM_TYPE_NUMBER};
    use super::*;
    use std::sync::Mutex;
    use std::sync::LazyLock;

    #[derive(Debug)]
    struct OuterLoader;

    impl ModuleLoader for OuterLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            let space = GroundingSpace::new();
            context.init_self_module(space.into(), None);

            let parser = SExprParser::new("outer-module-test-atom");
            context.push_parser(Box::new(parser));

            Ok(())
        }
    }

    #[derive(Debug)]
    struct InnerLoader;

    impl ModuleLoader for InnerLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            let space = GroundingSpace::new();
            context.init_self_module(space.into(), None);

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
            let space = GroundingSpace::new();
            context.init_self_module(space.into(), None);

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

    #[derive(Debug)]
    struct Issue982Loader;

    static ISSUE_982_CALLED: LazyLock<Mutex<i64>> = LazyLock::new(|| Mutex::new(0));

    pub static ISSUE982_METTA: &'static str = "!(i982 5)";

    fn i982(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let mut data = ISSUE_982_CALLED.lock().unwrap();
        *data = args.get(0).and_then(Number::from_atom).unwrap().into();
        Ok(vec![args.get(0).unwrap().clone()])
    }

    impl ModuleLoader for Issue982Loader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            // Initialize module's space
            let space = GroundingSpace::new();
            context.init_self_module(space.into(), None);

            // Load module's tokens
            let _ = self.load_tokens(context.module(), context.metta.clone())?;

            // Parse MeTTa code of the module
            let parser = SExprParser::new(ISSUE982_METTA);
            context.push_parser(Box::new(parser));

            Ok(())
        }

        fn load_tokens(&self, target: &MettaMod, _metta: Metta) -> Result<(), String> {
            let mut tref = target.tokenizer().borrow_mut();

            tref.register_function(GroundedFunctionAtom::new(
                r"i982".into(),
                Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER]),
                i982));

            Ok(())
        }
    }

    #[test]
    fn issue982_test() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));

        let _i982loader_mod_id = runner.load_module_direct(Box::new(Issue982Loader), "i982loader").unwrap();

        let result = runner.run(SExprParser::new("!(import! &self i982loader)"));
        assert_eq!(result, Ok(vec![vec![expr!()]]));

        assert_eq!(*ISSUE_982_CALLED.lock().unwrap(), 5);

        let result = runner.run(SExprParser::new("!(assertEqual (i982 6) 6)"));
        assert_eq!(result, Ok(vec![vec![expr!()]]));

        assert_eq!(*ISSUE_982_CALLED.lock().unwrap(), 6);
    }

    #[derive(Debug)]
    struct P08ModuleLoader(&'static str);

    impl ModuleLoader for P08ModuleLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            context.push_parser(Box::new(SExprParser::new(self.0)));
            Ok(())
        }
    }

    #[test]
    fn p08_checkpoint_keeps_wide_metadata_in_place() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        let (mut state, root) = ModuleInitState::new("top:p08-wide".to_string());
        for i in 0..16384 {
            state.add_module_to_name_tree(&runner, root, &format!("top:p08-wide:a{i}"), ModId::TOP).unwrap();
            #[cfg(feature = "pkg_mgmt")]
            state.add_module_descriptor(&runner,
                ModuleDescriptor::new(format!("p08-wide-{i}"), None, Some(i)), ModId::TOP);
        }
        assert_eq!(mem::size_of::<ModuleInitCheckpoint>(), 2 * mem::size_of::<usize>());
        let cell = match &state { ModuleInitState::Root(cell) => cell.clone(), _ => unreachable!() };
        let (node, entries) = {
            let insides = cell.borrow();
            (insides.frames[0].sub_module_names.name_to_node::<&ModNameNode>(&[], "a0").unwrap() as *const _,
                insides.undo.len())
        };
        let checkpoint = state.checkpoint().unwrap();
        assert_eq!(checkpoint.undo, entries);
        assert_eq!(cell.borrow().undo.len(), entries);
        assert_eq!(cell.borrow().frames[0].sub_module_names.name_to_node::<&ModNameNode>(&[], "a0").unwrap() as *const _, node);
        state.add_module_to_name_tree(&runner, root, "top:p08-wide:one-more", ModId::TOP).unwrap();
        let insides = cell.borrow();
        assert_eq!(insides.undo.len(), entries + 1);
        drop(insides);
        state.restore_checkpoint(Some(checkpoint));
        assert_eq!(cell.borrow().undo.len(), entries);
        assert!(state.get_module_by_name(&runner, "top:p08-wide:one-more").is_err());
    }

    fn p08_journal_len(state: &ModuleInitState) -> usize {
        match state {
            ModuleInitState::Root(cell) | ModuleInitState::Child(cell) => cell.borrow().undo.len(),
            ModuleInitState::None => 0,
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    fn p08_journal_descriptor() -> ModuleDescriptor {
        ModuleDescriptor::new("p08-journal".to_string(), None, Some(809))
    }

    #[derive(Debug)]
    struct P08JournalMutationLoader(bool);

    impl ModuleLoader for P08JournalMutationLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            // Exercise the insertion primitive's overwrite contract, including
            // descendants. Public aliases normally reject an existing name.
            let root = ModId::new_relative(0);
            context.init_state.add_module_to_name_tree(context.metta, root,
                "top:p08-journal:branch", context.mod_id)?;
            context.init_state.add_module_to_name_tree(context.metta, root,
                "top:p08-journal:branch:new-leaf", context.mod_id)?;
            #[cfg(feature = "pkg_mgmt")]
            context.init_state.add_module_descriptor(context.metta, p08_journal_descriptor(), context.mod_id);
            if self.0 { Err(P08_ORIGINAL_ERROR.to_string()) } else { Ok(()) }
        }
    }

    #[derive(Debug)]
    struct P08JournalAncestorLoader;

    impl ModuleLoader for P08JournalAncestorLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let child = context.load_module_direct(Box::new(P08JournalMutationLoader(false)), "self:success")?;
            let entries = p08_journal_len(context.init_state);
            for _ in 0..2 {
                assert_eq!(context.load_module_direct(Box::new(P08JournalMutationLoader(true)), "self:retry"),
                    Err(P08_ORIGINAL_ERROR.to_string()));
                assert_eq!(p08_journal_len(context.init_state), entries);
                assert_eq!(context.get_module_by_name("top:p08-journal:branch"), Ok(child));
                assert_eq!(context.get_module_by_name("top:p08-journal:branch:new-leaf"), Ok(child));
                #[cfg(feature = "pkg_mgmt")]
                assert_eq!(context.init_state.get_module_with_descriptor(context.metta, &p08_journal_descriptor()), Some(child));
            }
            Err(P08_ORIGINAL_ERROR.to_string())
        }
    }

    #[derive(Debug)]
    struct P08JournalParentLoader;

    impl ModuleLoader for P08JournalParentLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            context.load_module_alias("self:branch", ModId::TOP)?;
            context.load_module_alias("self:branch:old-leaf", ModId::TOP)?;
            #[cfg(feature = "pkg_mgmt")]
            context.init_state.add_module_descriptor(context.metta, p08_journal_descriptor(), ModId::TOP);
            let entries = p08_journal_len(context.init_state);
            for _ in 0..2 {
                assert_eq!(context.load_module_direct(Box::new(P08JournalAncestorLoader), "self:attempt"),
                    Err(P08_ORIGINAL_ERROR.to_string()));
                assert_eq!(p08_journal_len(context.init_state), entries);
                assert_eq!(context.get_module_by_name("self:branch"), Ok(ModId::TOP));
                assert_eq!(context.get_module_by_name("self:branch:old-leaf"), Ok(ModId::TOP));
                assert!(context.get_module_by_name("self:branch:new-leaf").is_err());
                assert!(context.get_module_by_name("self:attempt").is_err());
                #[cfg(feature = "pkg_mgmt")]
                assert_eq!(context.init_state.get_module_with_descriptor(context.metta, &p08_journal_descriptor()), Some(ModId::TOP));
            }
            Ok(())
        }
    }

    #[test]
    fn p08_journal_restores_overwrites_across_nested_success_failure_and_retry() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        runner.load_module_direct(Box::new(P08JournalParentLoader), "p08-journal").unwrap();
        assert_eq!(runner.get_module_by_name("p08-journal:branch:old-leaf"), Ok(ModId::TOP));
        assert!(runner.get_module_by_name("p08-journal:branch:new-leaf").is_err());
        #[cfg(feature = "pkg_mgmt")]
        assert_eq!(runner.get_module_with_descriptor(&p08_journal_descriptor()), Some(ModId::TOP));
    }

    fn p08_assert_module_value(runner: &Metta, mod_name: &str, value_name: &str, expected: i64) {
        let import = format!("!(import! &self {mod_name})");
        assert_eq!(runner.run(SExprParser::new(&import)), Ok(vec![vec![expr!()]]));
        let query = format!("!(match &self ({value_name} $x) $x)");
        assert_eq!(runner.run(SExprParser::new(&query)),
            Ok(vec![vec![Atom::gnd(Number::Integer(expected))]]));
    }

    const P08_ORIGINAL_ERROR: &str = "p08 original failure";

    #[derive(Debug)]
    struct P08ErrorLoader;

    impl ModuleLoader for P08ErrorLoader {
        fn load(&self, _context: &mut RunContext) -> Result<(), String> {
            Err(P08_ORIGINAL_ERROR.to_string())
        }
    }

    #[derive(Debug)]
    struct P08RetryParentLoader;

    impl ModuleLoader for P08RetryParentLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let keep_id = context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-keep-value 11)")), "self:keep")?;

            let error = context.load_module_direct(Box::new(P08ErrorLoader), "self:retry")
                .unwrap_err();
            assert_eq!(error, P08_ORIGINAL_ERROR);
            assert_eq!(context.get_module_by_name("self:keep"), Ok(keep_id));

            let retry_id = context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-retry-value 22)")), "self:retry")?;
            let last_id = context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-last-value 33)")), "self:last")?;
            assert_eq!(context.get_module_by_name("self:keep"), Ok(keep_id));
            assert_eq!(context.get_module_by_name("self:retry"), Ok(retry_id));
            assert_eq!(context.get_module_by_name("self:last"), Ok(last_id));
            Ok(())
        }
    }

    #[test]
    fn p08_failure_before_init_preserves_siblings_and_allows_retry() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        runner.load_module_direct(Box::new(P08RetryParentLoader), "p08-parent").unwrap();

        assert!(runner.get_module_by_name("p08-parent:retry").is_ok());
        p08_assert_module_value(&runner, "p08-parent:keep", "p08-keep-value", 11);
        p08_assert_module_value(&runner, "p08-parent:retry", "p08-retry-value", 22);
        p08_assert_module_value(&runner, "p08-parent:last", "p08-last-value", 33);
    }

    #[derive(Debug)]
    struct P08NestedLoader {
        fail: bool,
    }

    impl ModuleLoader for P08NestedLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let child_source = if self.fail {
                "(p08-generation 1)"
            } else {
                "(p08-generation 2)"
            };
            context.load_module_direct(Box::new(P08ModuleLoader(child_source)), "self:grandchild")?;
            if self.fail {
                Err(P08_ORIGINAL_ERROR.to_string())
            } else {
                Ok(())
            }
        }
    }

    #[derive(Debug)]
    struct P08NestedRetryParentLoader;

    impl ModuleLoader for P08NestedRetryParentLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let error = context.load_module_direct(
                Box::new(P08NestedLoader { fail: true }), "self:retry").unwrap_err();
            assert_eq!(error, P08_ORIGINAL_ERROR);
            context.load_module_direct(
                Box::new(P08NestedLoader { fail: false }), "self:retry")?;
            Ok(())
        }
    }

    #[test]
    fn p08_initialized_ancestor_failure_discards_child_and_allows_retry() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        runner.load_module_direct(Box::new(P08NestedRetryParentLoader), "p08-nested-parent").unwrap();

        p08_assert_module_value(&runner, "p08-nested-parent:retry:grandchild", "p08-generation", 2);
    }

    #[derive(Clone, Copy, Debug)]
    enum P08LateFailure {
        Parser,
        Function,
        Finalize,
        ErrorAtom,
    }

    impl P08LateFailure {
        fn error(self) -> &'static str {
            match self {
                Self::Parser => "p08 parser failure",
                Self::Function => "p08 function failure",
                Self::Finalize => "Module loader finished without running RunContext::init_self_module",
                Self::ErrorAtom => "p08 error atom failure",
            }
        }
    }

    struct P08ErrorAtomParser(u8);

    impl Parser for P08ErrorAtomParser {
        fn next_atom(&mut self, _tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
            let atom = match self.0 {
                0 => EXEC_SYMBOL,
                1 => error_atom(None, None, P08LateFailure::ErrorAtom.error().to_string()),
                _ => return Ok(None),
            };
            self.0 += 1;
            Ok(Some(atom))
        }
    }

    struct P08ErrorParser;

    impl Parser for P08ErrorParser {
        fn next_atom(&mut self, _tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
            Err(P08LateFailure::Parser.error().to_string())
        }
    }

    #[derive(Debug)]
    struct P08LateFailureLoader(P08LateFailure);

    impl ModuleLoader for P08LateFailureLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            match self.0 {
                P08LateFailure::Parser => {
                    context.init_self_module(GroundingSpace::new().into(), None);
                    context.push_parser(Box::new(P08ErrorParser));
                },
                P08LateFailure::Function => {
                    context.init_self_module(GroundingSpace::new().into(), None);
                    context.push_func(|_context| Err(P08LateFailure::Function.error().to_string()));
                },
                P08LateFailure::Finalize => {},
                P08LateFailure::ErrorAtom => {
                    context.init_self_module(GroundingSpace::new().into(), None);
                    context.push_parser(Box::new(P08ErrorAtomParser(0)));
                },
            }
            Ok(())
        }
    }

    #[derive(Debug)]
    struct P08LateFailureParentLoader(P08LateFailure);

    impl ModuleLoader for P08LateFailureParentLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let error = context.load_module_direct(
                Box::new(P08LateFailureLoader(self.0)), "self:retry").unwrap_err();
            assert_eq!(error, self.0.error());
            context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-late-value 44)")), "self:retry")?;
            Ok(())
        }
    }

    fn p08_test_late_failure(failure: P08LateFailure, mod_name: &str) {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        runner.load_module_direct(Box::new(P08LateFailureParentLoader(failure)), mod_name).unwrap();
        p08_assert_module_value(&runner, &format!("{mod_name}:retry"), "p08-late-value", 44);
    }

    #[test]
    fn p08_parser_step_failure_preserves_error_and_allows_retry() {
        p08_test_late_failure(P08LateFailure::Parser, "p08-parser-parent");
    }

    #[test]
    fn p08_function_step_failure_preserves_error_and_allows_retry() {
        p08_test_late_failure(P08LateFailure::Function, "p08-function-parent");
    }

    #[test]
    fn p08_finalization_failure_preserves_error_and_allows_retry() {
        p08_test_late_failure(P08LateFailure::Finalize, "p08-finalize-parent");
    }

    #[test]
    fn p08_error_atom_finalization_failure_preserves_error_and_allows_retry() {
        p08_test_late_failure(P08LateFailure::ErrorAtom, "p08-error-atom-parent");
    }

    #[cfg(feature = "pkg_mgmt")]
    #[derive(Debug)]
    struct P08PrepareFailureLoader;

    #[cfg(feature = "pkg_mgmt")]
    impl ModuleLoader for P08PrepareFailureLoader {
        fn load(&self, _context: &mut RunContext) -> Result<(), String> {
            Err("p08 loader called after failed preparation".to_string())
        }

        fn prepare(&self, _local_dir: Option<&Path>, _update_mode: UpdateMode) -> Result<Option<Box<dyn ModuleLoader>>, String> {
            Err(P08_ORIGINAL_ERROR.to_string())
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    #[derive(Debug)]
    struct P08PrepareFailureParentLoader;

    #[cfg(feature = "pkg_mgmt")]
    impl ModuleLoader for P08PrepareFailureParentLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let error = context.load_module_direct(
                Box::new(P08PrepareFailureLoader), "self:retry").unwrap_err();
            assert_eq!(error, P08_ORIGINAL_ERROR);
            context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-prepare-value 55)")), "self:retry")?;
            Ok(())
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    #[test]
    fn p08_prepare_failure_preserves_error_and_existing_state() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        runner.load_module_direct(Box::new(P08PrepareFailureParentLoader), "p08-prepare-parent").unwrap();
        p08_assert_module_value(&runner, "p08-prepare-parent:retry", "p08-prepare-value", 55);
    }

    #[cfg(feature = "pkg_mgmt")]
    #[derive(Debug)]
    struct P08DescriptorLoader {
        child_descriptor: ModuleDescriptor,
        fail: bool,
    }

    #[cfg(feature = "pkg_mgmt")]
    impl ModuleLoader for P08DescriptorLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let child_name = context.normalize_module_name("self:child")?;
            let child_source = if self.fail {
                "(p08-descriptor-generation 1)"
            } else {
                "(p08-descriptor-generation 2)"
            };
            context.get_or_init_module_with_descriptor(&child_name, self.child_descriptor.clone(),
                Box::new(P08ModuleLoader(child_source)))?;
            if self.fail {
                Err(P08_ORIGINAL_ERROR.to_string())
            } else {
                Ok(())
            }
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    #[derive(Debug)]
    struct P08DescriptorParentLoader {
        keep: ModuleDescriptor,
        retry: ModuleDescriptor,
        child: ModuleDescriptor,
    }

    #[cfg(feature = "pkg_mgmt")]
    impl P08DescriptorParentLoader {
        fn assert_descriptor_count(&self, state: &ModuleInitState, expected: usize) {
            match state {
                ModuleInitState::Root(cell) | ModuleInitState::Child(cell) => {
                    assert_eq!(cell.borrow().module_descriptors.len(), expected);
                },
                ModuleInitState::None => panic!("parent initialization state is missing"),
            }
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    impl ModuleLoader for P08DescriptorParentLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let keep_name = context.normalize_module_name("self:keep")?;
            let keep_id = context.get_or_init_module_with_descriptor(&keep_name, self.keep.clone(),
                Box::new(P08ModuleLoader("(p08-descriptor-keep 66)")))?;
            let retry_name = context.normalize_module_name("self:retry")?;

            for _ in 0..2 {
                let error = context.get_or_init_module_with_descriptor(&retry_name, self.retry.clone(),
                    Box::new(P08DescriptorLoader { child_descriptor: self.child.clone(), fail: true }))
                    .unwrap_err();
                assert_eq!(error, P08_ORIGINAL_ERROR);
                assert_eq!(context.init_state.get_module_with_descriptor(context.metta, &self.keep),
                    Some(keep_id));
                assert_eq!(context.init_state.get_module_with_descriptor(context.metta, &self.retry), None);
                assert_eq!(context.init_state.get_module_with_descriptor(context.metta, &self.child), None);
                self.assert_descriptor_count(context.init_state, 1);
            }

            context.get_or_init_module_with_descriptor(&retry_name, self.retry.clone(),
                Box::new(P08DescriptorLoader { child_descriptor: self.child.clone(), fail: false }))?;
            assert_eq!(context.init_state.get_module_with_descriptor(context.metta, &self.keep),
                Some(keep_id));
            Ok(())
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    #[test]
    fn p08_repeated_descriptor_failure_discards_only_failed_mappings() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        let keep = ModuleDescriptor::new("p08-keep".to_string(), None, Some(801));
        let retry = ModuleDescriptor::new("p08-retry".to_string(), None, Some(802));
        let child = ModuleDescriptor::new("p08-child".to_string(), None, Some(803));
        runner.load_module_direct(Box::new(P08DescriptorParentLoader {
            keep: keep.clone(),
            retry: retry.clone(),
            child: child.clone(),
        }), "p08-descriptor-parent").unwrap();

        let keep_id = runner.get_module_by_name("p08-descriptor-parent:keep").unwrap();
        let retry_id = runner.get_module_by_name("p08-descriptor-parent:retry").unwrap();
        let child_id = runner.get_module_by_name("p08-descriptor-parent:retry:child").unwrap();
        assert_eq!(runner.get_module_with_descriptor(&keep), Some(keep_id));
        assert_eq!(runner.get_module_with_descriptor(&retry), Some(retry_id));
        assert_eq!(runner.get_module_with_descriptor(&child), Some(child_id));
        p08_assert_module_value(&runner, "p08-descriptor-parent:keep", "p08-descriptor-keep", 66);
        p08_assert_module_value(&runner, "p08-descriptor-parent:retry:child", "p08-descriptor-generation", 2);
    }

    #[derive(Debug)]
    struct P08SuccessfulParentLoader;

    impl ModuleLoader for P08SuccessfulParentLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            let first = context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-first-value 77)")), "self:first")?;
            let second = context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-second-value 88)")), "self:second")?;
            assert_eq!(first.get_idx_from_relative(), 1);
            assert_eq!(second.get_idx_from_relative(), 2);
            assert_eq!(context.get_module_by_name("self:first"), Ok(first));
            assert_eq!(context.get_module_by_name("self:second"), Ok(second));
            Ok(())
        }
    }

    #[test]
    fn p08_successful_nested_loading_preserves_order_identity_and_results() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        let parent = runner.load_module_direct(Box::new(P08SuccessfulParentLoader), "p08-success-parent").unwrap();
        let first = runner.get_module_by_name("p08-success-parent:first").unwrap();
        let second = runner.get_module_by_name("p08-success-parent:second").unwrap();
        assert_eq!(first, ModId(parent.0 + 1));
        assert_eq!(second, ModId(parent.0 + 2));
        p08_assert_module_value(&runner, "p08-success-parent:first", "p08-first-value", 77);
        p08_assert_module_value(&runner, "p08-success-parent:second", "p08-second-value", 88);
    }

    #[test]
    fn p08_top_level_failure_restores_none_for_same_runner_retry() {
        let runner = Metta::new(Some(EnvBuilder::test_env()));
        let mut state = RunnerState::new(&runner);
        state.run_in_context(|context| {
            let error = context.load_module_direct(Box::new(P08ErrorLoader), "p08-root-retry")
                .unwrap_err();
            assert_eq!(error, P08_ORIGINAL_ERROR);
            context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-root-value 99)")), "p08-root-retry")
        }).unwrap();
        p08_assert_module_value(&runner, "p08-root-retry", "p08-root-value", 99);
    }

    #[cfg(feature = "pkg_mgmt")]
    #[derive(Debug)]
    struct P08AliasCatalog;

    #[cfg(feature = "pkg_mgmt")]
    impl P08AliasCatalog {
        fn descriptor() -> ModuleDescriptor {
            ModuleDescriptor::new("shared".to_string(), None, Some(808))
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    impl ModuleCatalog for P08AliasCatalog {
        fn lookup(&self, name: &str) -> Vec<ModuleDescriptor> {
            if name == "shared" {
                vec![Self::descriptor()]
            } else {
                vec![]
            }
        }

        fn get_loader(&self, descriptor: &ModuleDescriptor) -> Result<Box<dyn ModuleLoader>, String> {
            assert_eq!(descriptor, &Self::descriptor());
            Ok(Box::new(P08ModuleLoader("(p08-catalog-shared 100)")))
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    #[derive(Clone, Copy, Debug)]
    enum P08AliasCase {
        Dangling,
        Remapped,
        Existing,
    }

    #[cfg(feature = "pkg_mgmt")]
    #[derive(Debug)]
    struct P08AliasAttemptLoader {
        parent_name: &'static str,
        load_shared: bool,
    }

    #[cfg(feature = "pkg_mgmt")]
    impl ModuleLoader for P08AliasAttemptLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            if self.load_shared {
                context.load_module("self:shared")?;
            }
            let alias = format!("top:{}:keep:shared", self.parent_name);
            context.load_module(&alias)?;
            Err(P08_ORIGINAL_ERROR.to_string())
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    #[derive(Debug)]
    struct P08AliasParentLoader {
        name: &'static str,
        case: P08AliasCase,
    }

    #[cfg(feature = "pkg_mgmt")]
    impl ModuleLoader for P08AliasParentLoader {
        fn load(&self, context: &mut RunContext) -> Result<(), String> {
            context.init_self_module(GroundingSpace::new().into(), None);
            if matches!(self.case, P08AliasCase::Existing) {
                context.load_module("self:shared")?;
            }
            context.load_module_direct(
                Box::new(P08ModuleLoader("(p08-alias-keep 101)")), "self:keep")?;
            let error = context.load_module_direct(Box::new(P08AliasAttemptLoader {
                parent_name: self.name,
                load_shared: !matches!(self.case, P08AliasCase::Existing),
            }), "self:attempt").unwrap_err();
            assert_eq!(error, P08_ORIGINAL_ERROR);

            if matches!(self.case, P08AliasCase::Remapped) {
                context.load_module_direct(
                    Box::new(P08ModuleLoader("(p08-sib1-value 102)")), "self:sib1")?;
                context.load_module_direct(
                    Box::new(P08ModuleLoader("(p08-sib2-value 103)")), "self:sib2")?;
            }
            Ok(())
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    fn p08_alias_runner(parent_name: &'static str, case: P08AliasCase) -> Metta {
        let runner = Metta::new(Some(EnvBuilder::test_env().push_module_catalog(P08AliasCatalog)));
        runner.load_module_direct(Box::new(P08AliasParentLoader {
            name: parent_name,
            case,
        }), parent_name).unwrap();
        runner
    }

    #[cfg(feature = "pkg_mgmt")]
    #[test]
    fn p08_failed_attempt_does_not_leave_dangling_sibling_alias() {
        let runner = p08_alias_runner("p08-alias-dangling", P08AliasCase::Dangling);
        assert!(runner.get_module_by_name("p08-alias-dangling:keep:shared").is_err());
        p08_assert_module_value(&runner, "p08-alias-dangling:keep:shared", "p08-catalog-shared", 100);
        p08_assert_module_value(&runner, "p08-alias-dangling:keep", "p08-alias-keep", 101);
    }

    #[cfg(feature = "pkg_mgmt")]
    #[test]
    fn p08_failed_attempt_alias_cannot_be_remapped_to_later_sibling() {
        let runner = p08_alias_runner("p08-alias-remapped", P08AliasCase::Remapped);
        let sibling = runner.get_module_by_name("p08-alias-remapped:sib2").unwrap();
        if let Ok(alias) = runner.get_module_by_name("p08-alias-remapped:keep:shared") {
            assert_ne!(alias, sibling, "discarded relative ID was remapped to an unrelated sibling");
            panic!("failed-attempt alias survived cleanup");
        }
        p08_assert_module_value(&runner, "p08-alias-remapped:keep", "p08-alias-keep", 101);
        p08_assert_module_value(&runner, "p08-alias-remapped:sib2", "p08-sib2-value", 103);
    }

    #[cfg(feature = "pkg_mgmt")]
    #[test]
    fn p08_failed_attempt_removes_alias_to_existing_module() {
        let runner = p08_alias_runner("p08-alias-existing", P08AliasCase::Existing);
        let existing = runner.get_module_by_name("p08-alias-existing:shared").unwrap();
        if let Ok(alias) = runner.get_module_by_name("p08-alias-existing:keep:shared") {
            assert_ne!(alias, existing, "failed attempt retained an alias to a pre-existing module");
            panic!("failed-attempt alias survived cleanup");
        }
        p08_assert_module_value(&runner, "p08-alias-existing:shared", "p08-catalog-shared", 100);
        p08_assert_module_value(&runner, "p08-alias-existing:keep", "p08-alias-keep", 101);
    }


    //LP-TODO-NEXT,  Make a test for an inner-loader that throws an error, blocking the outer-loader from loading sucessfully,
    // and make sure neither module is loaded into the named index
    //
    //Also test the case where the inner loader is sucessul, but then the outer loader throws an error.  Also make sure neither
    // module is loaded into the namespace
    //
}
