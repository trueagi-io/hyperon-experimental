//!
//! # MeTTa Runner Implementation
//!
//! This documentation addresses the different objects involved with the MeTTa runner, and how they fit together.
//!
//! ## [Environment]
//! [Environment] is the gateway to the outside world.  It creates a platform-abstraction layer for MeTTa, and
//! is responsible for managing configuration and implementing a security model with permissions.  In a typical
//! situation, there will only be one [Environment] needed.
//!
//! ## [Metta]
//! [Metta] is the runner object.  It is owned by the caller and hosts all state associated with MeTTa execution,
//! including loaded [MettaMod] modules.  A [Metta] runner has one top-level module, (named "top") and
//! all other modules are loaded as dependents (or transitive dependents) of the top-level module. [Metta] is a
//! long-lived object, and it may be sufficient to create one [Metta] runner that lasts for the duration of the
//! host program.
//!
//! ## [RunnerState]
//! A [RunnerState] object encapsulates one conceptual "thread" of MeTTa execution (although it may be
//! parallelized in its implementation)  A [RunnerState] is short-lived; it is created to evaluate some
//! MeTTa code, and can be run until it finishes or encounters an error.  Multiple [RunnerState] objects may
//! be executing within the same [Metta] at the same time.
//! UPDATE: I think I will be removing the [RunnerState] shortly, in favor of a delegate interface that allows
//! a function to interact with the runner in specific ways for the implementation of a debugger.
//!
//! ## [ModuleDescriptor]
//! A self-contained data-structure that uniquely identifies a specific version of a specific module.  Two
//! modules that have the same ModuleDescriptor are considered to be the same module from the perspective of
//! the implementation.
//!
//! ## [MettaMod]
//! A [MettaMod] contains a loaded module.  A module is fundamentally a [Space] of atoms, although it also
//! contains an associated [Tokenizer] to help with the conversion from text to [Atom]s and sometimes a
//! resources directory.  Modules are loaded via loader functions, and they can originate from code
//! in pure MeTTa as well as through extensions in other host languages, namely Rust, C, and Python.
//!
//! ## [RunContext]
//! A [RunContext] objects encapsulates the interface accessible to code running inside a [RunnerState].  It
//! provides access to the currently loaded module and any other shared state required for the atoms executing
//! within the MeTTa interpreter.  A [RunContext] is created inside the runner, and it is not possible for
//! code outside the MeTTa core library to own a [RunContext].
//!
//!  Metta (Runner)
//!  ┌─────────────────────────────────────────────────────────────────┐
//!  │ MettaMods (Modules)                                             │
//!  │ ┌────────────────────────────────────────────────────┐          │
//!  │ │ Space                  Tokenizer                   ├─┐        │
//!  │ │ ┌─────────────────┐    ┌─────────────────────┐     │ ├─┐      │
//!  │ │ │                 │    │                     │     │ │ │      │
//!  │ │ └─────────────────┘    └─────────────────────┘     │ │ │      │
//!  │ └─┬──────────────────────────────────────────────────┘ │ │      │
//!  │   └─┬──────────────────────────────────────────────────┘ │      │
//!  │     └────────────────────────────────────────────────────┘      │
//!  └─────────────────────────────────────────────────────────────────┘
//!

//LP-TODO-NEXT: This description above is correct, but it's not complete.  Update with latest design.

use hyperon_atom::*;
use hyperon_common::shared::Shared;

use super::*;
use hyperon_space::*;
use super::text::{Tokenizer, Parser, SExprParser};
use super::types::{AtomType, get_atom_types, get_atom_types_v2, validate_atom};

pub mod modules;
use modules::{MettaMod, ModId, ModuleInitState, ModNameNode, ModuleLoader, ResourceKey, Resource, TOP_MOD_NAME, ModNameNodeDisplayWrapper, normalize_relative_module_name};
#[cfg(feature = "pkg_mgmt")]
use modules::{decompose_name_path, compose_name_path};

#[cfg(feature = "pkg_mgmt")]
pub mod pkg_mgmt;
#[cfg(feature = "pkg_mgmt")]
use pkg_mgmt::*;

#[cfg(not(feature = "pkg_mgmt"))]
pub(crate) type ModuleDescriptor = ();

use std::rc::Rc;
use std::path::PathBuf;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

mod environment;
pub use environment::{Environment, EnvBuilder};

use super::interpreter::{interpret, interpret_init, interpret_step, InterpreterState};

#[macro_use]
pub mod stdlib;
use stdlib::CoreLibLoader;

mod builtin_mods;
use builtin_mods::*;

pub mod bool;
pub mod number;
pub mod str;

const EXEC_SYMBOL : Atom = sym!("!");

// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*
// Metta & related objects
// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*

/// A Metta object encapsulates everything needed to execute MeTTa code
#[derive(Clone, Debug)]
pub struct Metta(Rc<MettaContents>);

impl PartialEq for Metta {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug)]
pub struct PragmaSettings(Shared<HashMap<String, Atom>>);

impl PragmaSettings {
    pub fn new() -> Self {
        Self(Shared::new(HashMap::new()))
    }

    pub fn set(&self, key: String, value: Atom) {
        self.0.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Atom> {
        self.0.borrow().get(key).cloned()
    }

    pub fn get_string(&self, key: &str) -> Option<String> {
        self.0.borrow().get(key).map(|a| a.to_string())
    }
}

#[derive(Debug)]
pub(crate) struct MettaContents {
    /// All the runner's loaded modules
    modules: Mutex<Vec<Rc<MettaMod>>>,
    /// A tree to locate loaded mods by name
    module_names: Mutex<ModNameNode>,
    #[cfg(feature = "pkg_mgmt")]
    /// An index, to find a loaded module from a ModuleDescriptor
    module_descriptors: Mutex<HashMap<ModuleDescriptor, ModId>>,
    /// A clone of the top module's Space, so we don't need to do any locking to access it,
    /// to support the metta.space() public function. Actual module space is an instance
    /// of the [module::ModuleSpace]. This instance contains dependencies of the
    /// top module. This space is an original space passed to the Metta constructor
    /// thus it doesn't contain any dependencies.
    top_mod_space: DynSpace,
    /// A clone of the top module's Tokenizer
    top_mod_tokenizer: Shared<Tokenizer>,
    /// The ModId of the extended corelib to import into some modules loaded into the runner
    corelib_mod: OnceLock<ModId>,
    /// The ModId of the extended stdlib to import into some modules loaded into the runner
    stdlib_mod: OnceLock<ModId>,
    /// The runner's pragmas, affecting runner-wide behavior
    settings: PragmaSettings,
    /// The runner's Environment
    environment: Arc<Environment>,
    //TODO-HACK: This is a terrible horrible ugly hack that should not be merged.  Delete this field
    // The real context is an interface to the state in a run, and should not live across runs
    // This hack will fail badly if we end up running code from two different modules in parallel
    context: Arc<Mutex<Vec<Arc<Mutex<&'static mut RunContext<'static, 'static>>>>>>,
}

impl Metta {

    /// A 1-line method to create a fully initialized MeTTa runner
    ///
    /// NOTE: pass `None` for `env_builder` to use the common environment
    pub fn new(env_builder: Option<EnvBuilder>) -> Metta {
        Self::new_with_stdlib_loader(None, None, env_builder)
    }

    /// Create and initialize a MeTTa runner with a custom stdlib, for example a language-specific stdlib
    ///
    /// NOTE: The custom stdlib loader may import the corelib if desired, but it won't be imported automatically.
    ///
    /// NOTE: Is `None` is passed as the `loader` parameter, `stdlib` will be an alias to `corelib`
    /// pass `None` for space to create a new [GroundingSpace]
    /// pass `None` for `env_builder` to use the common environment
    pub fn new_with_stdlib_loader(loader: Option<Box<dyn ModuleLoader>>, space: Option<DynSpace>, env_builder: Option<EnvBuilder>) -> Metta {

        //Create the raw MeTTa runner
        let metta = Metta::new_core(space, env_builder);

        //Load the "corelib" module into the runner
        let corelib_mod_id = metta.load_module_direct(Box::new(CoreLibLoader), "corelib").expect("Failed to load corelib");
        metta.0.corelib_mod.set(corelib_mod_id).unwrap();

        //Load the stdlib if we have one, and otherwise make an alias to corelib
        match loader {
            Some(loader) => {
                let stdlib_mod_id = metta.load_module_direct(loader, "stdlib").expect("Failed to load stdlib");
                metta.0.stdlib_mod.set(stdlib_mod_id).unwrap();
            },
            None => {
                metta.load_module_alias("stdlib", corelib_mod_id).expect("Failed to create stdlib alias for corelib");
                ()
            }
        };

        //Load the rest of the builtin mods, but don't `import` (aka "use") them
        load_builtin_mods(&metta).unwrap();

        //Import the corelib and stdlib into the top module, now that it is loaded
        let mut runner_state = RunnerState::new(&metta);
       
        if let Some(corelib_mod_id) = metta.0.corelib_mod.get() {
            runner_state.run_in_context(|context| {
                context.import_all_from_dependency(*corelib_mod_id).unwrap();
                Ok(())
            }).expect("Failed to import corelib");
        }

        if let Some(stdlib_mod_id) = metta.0.stdlib_mod.get() {
            runner_state.run_in_context(|context| {
                context.import_all_from_dependency(*stdlib_mod_id).unwrap();
                Ok(())
            }).expect("Failed to import stdlib");
        }
        drop(runner_state);

        //Run the `init.metta` file
        if let Some(init_meta_file_path) = metta.0.environment.initialization_metta_file_path() {
            let metta_file = match std::fs::File::open(init_meta_file_path).map(std::io::BufReader::new)
            {
                Ok(metta_file) => metta_file,
                Err(err) => panic!("Could not read file, path: {}, error: {}", init_meta_file_path.display(), err)
            };
            metta.run(SExprParser::new(metta_file)).unwrap();
        }
        metta
    }

    /// Returns a new core MeTTa interpreter without any loaded corelib, stdlib, or initialization
    ///
    /// NOTE: If `space` is `None`, a [GroundingSpace] will be created
    /// NOTE: If `env_builder` is `None`, the common environment will be used
    /// NOTE: This function does not load any modules, nor run the [Environment]'s 'init.metta'
    pub fn new_core(space: Option<DynSpace>, env_builder: Option<EnvBuilder>) -> Self {
        let space = match space {
            Some(space) => space,
            None => GroundingSpace::new().into(),
        };
        let settings = PragmaSettings::new();
        let environment = match env_builder {
            Some(env_builder) => Arc::new(env_builder.build()),
            None => Environment::common_env_arc()
        };
        let top_mod_resource_dir = environment.working_dir().map(|path| path.into());
        let top_mod_tokenizer = Shared::new(Tokenizer::new());
        let contents = MettaContents{
            modules: Mutex::new(vec![]),
            module_names: Mutex::new(ModNameNode::top()),
            #[cfg(feature = "pkg_mgmt")]
            module_descriptors: Mutex::new(HashMap::new()),
            top_mod_space: space.clone(),
            top_mod_tokenizer: top_mod_tokenizer.clone(),
            corelib_mod: OnceLock::new(),
            stdlib_mod: OnceLock::new(),
            settings,
            environment,
            context: std::sync::Arc::new(std::sync::Mutex::new(vec![])),
        };
        let metta = Self(Rc::new(contents));

        let top_mod = MettaMod::new_with_tokenizer(&metta, TOP_MOD_NAME.to_string(), space, top_mod_tokenizer, top_mod_resource_dir, false);
        assert_eq!(metta.add_module(top_mod).unwrap(), ModId::TOP);

        metta
    }

    /// Loads a module into a Runner, directly from a [ModuleLoader]
    ///
    /// NOTE: `mod_name` may be a module name path if this module is being loaded as a sub-module of
    /// another loaded module.  Relative paths may not be used with this API, however.  Use
    /// [RunContext::load_module_direct] if you are loading a sub-module from within a running module.
    pub fn load_module_direct(&self, loader: Box<dyn ModuleLoader>, mod_name: &str) -> Result<ModId, String> {
        let mut state = RunnerState::new_with_module(self, ModId::TOP);
        state.run_in_context(|context| {
            context.load_module_direct(loader, mod_name)
        })
    }

    /// Loads a module into a runner from a resource at the specified path
    ///
    /// This method will try each [FsModuleFormat] in order until one can sucessfully load the module
    ///
    /// NOTE: `mod_name` may be a module name path if the module is being loaded as a sub-module of
    /// another loaded module.  Relative paths may not be used with this API, however.  Use
    /// [RunContext::load_module_at_path] if you are loading a sub-module from within a running module.
    ///
    /// Requires the `pkg_mgmt` feature
    #[cfg(feature = "pkg_mgmt")]
    pub fn load_module_at_path<P: AsRef<std::path::Path>>(&self, path: P, mod_name: Option<&str>) -> Result<ModId, String> {
        let mut state = RunnerState::new_with_module(self, ModId::TOP);
        state.run_in_context(|context| {
            context.load_module_at_path(&path, mod_name)
        })
    }

    /// Internal method to look up a module from a ModId
    pub(crate) fn get_mod_ptr(&self, mod_id: ModId) -> Rc<MettaMod> {
        let mod_ref = self.0.modules.lock().unwrap();
        mod_ref.get(mod_id.0).unwrap().clone()
    }

    /// Locates and retrieves a loaded module based on its name, relative to the top of the runner
    ///
    /// NOTE: this function will not find any modules in the process of being loaded; use
    /// [RunContext::get_module_by_name] if you require that
    fn get_module_by_name(&self, mod_name: &str) -> Result<ModId, String> {
        let module_names = self.0.module_names.lock().unwrap();
        module_names.resolve(mod_name).ok_or_else(|| format!("Unable to locate module: {mod_name}"))
    }

    /// Adds a ModId to the named module tree with the specified name, relative to the top of the runer
    fn add_module_to_name_tree(&self, mod_name: &str, mod_id: ModId) -> Result<(), String>  {
        assert!(!mod_id.is_relative());
        let mut module_names = self.0.module_names.lock().unwrap();
        module_names.add(mod_name, mod_id)
    }

    /// Makes a public alias for a loaded module inside the runner
    ///
    /// NOTE: `mod_name` may be a module name path if this alias is being loaded as a sub-module of
    /// another loaded module.  Relative paths may not be used with this API, however.  Use
    /// [RunContext::load_module_alias] if you are creating an alias from within a running module.
    pub fn load_module_alias(&self, mod_name: &str, mod_id: ModId) -> Result<ModId, String> {
        let mut state = RunnerState::new_with_module(self, ModId::TOP);
        state.run_in_context(|context| {
            context.load_module_alias(mod_name, mod_id)
        })
    }

    /// Writes a textual description of the loaded modules to stdout
    pub fn display_loaded_modules(&self) {
        let module_names = self.0.module_names.lock().unwrap();
        let wrapper = ModNameNodeDisplayWrapper::new(TOP_MOD_NAME, &*module_names, |mod_id: ModId, f: &mut std::fmt::Formatter| write!(f, "{}", mod_id.0));
        println!("{wrapper}");
    }

    #[cfg(feature = "pkg_mgmt")]
    /// Returns the ModId of a loaded module, based on its descriptor, or None if it isn't loaded
    pub fn get_module_with_descriptor(&self, descriptor: &ModuleDescriptor) -> Option<ModId> {
        let descriptors = self.0.module_descriptors.lock().unwrap();
        descriptors.get(descriptor).cloned()
    }

    #[cfg(feature = "pkg_mgmt")]
    /// Internal method to add a ModuleDescriptor, ModId pair to the runner's lookup table
    fn add_module_descriptor(&self, descriptor: ModuleDescriptor, mod_id: ModId) {
        let mut descriptors = self.0.module_descriptors.lock().unwrap();
        descriptors.insert(descriptor, mod_id);
    }

    /// Merges all modules in a [ModuleInitState] into the runner
    fn merge_init_state(&self, init_state: ModuleInitState) -> Result<ModId, String> {
        let mut main_mod_id = ModId::INVALID;
        let (frames, descriptors) = init_state.decompose();

        // Unpack each frame and ,erge the modules from the ModuleInitState into the
        // runner, and build the mapping table for ModIds
        let mut mod_name_subtrees: Vec<(String, ModNameNode)> = Vec::with_capacity(frames.len());
        let mut mod_id_mapping = HashMap::with_capacity(frames.len());
        for (frame_idx, frame) in frames.into_iter().enumerate() {
            let old_mod_id = ModId::new_relative(frame_idx);
            let mod_name = frame.new_mod_name.unwrap();
            let module = frame.the_mod.unwrap();

            mod_name_subtrees.push((mod_name, frame.sub_module_names));

            let new_mod_id = self.add_module(Rc::into_inner(module).unwrap())?;
            mod_id_mapping.insert(old_mod_id, new_mod_id);

            if frame_idx == 0 {
                main_mod_id = new_mod_id;
            }
        }

        // Merge the name trees into the runner
        let mut module_names = self.0.module_names.lock().unwrap();
        for (mod_name, mut subtree) in mod_name_subtrees.into_iter() {
            subtree.visit_mut("", |_name, node: &mut ModNameNode| {
                if let Some(new_mod_id) = mod_id_mapping.get(&node.mod_id) {
                    node.mod_id = *new_mod_id;
                }
            });
            module_names.merge_subtree_into(&mod_name, subtree)?;
        }

        // Merge the [ModuleDescriptor]s into the runner's table
        #[cfg(feature = "pkg_mgmt")]
        for (descriptor, mod_id) in descriptors.into_iter() {
            let mod_id = match mod_id_mapping.get(&mod_id) {
                Some(mapped_id) => *mapped_id,
                None => mod_id,
            };
            self.add_module_descriptor(descriptor, mod_id);
        }
        #[cfg(not(feature = "pkg_mgmt"))]
        let _ = descriptors;

        // Finally, re-map the module's "deps" ModIds
        for added_mod_id in mod_id_mapping.values() {
            let mod_ptr = self.get_mod_ptr(*added_mod_id);
            mod_ptr.remap_imported_deps(&mod_id_mapping);
        }

        Ok(main_mod_id)
    }

    /// Internal function to add a loaded module to the runner, assigning it a ModId
    fn add_module(&self, module: MettaMod) -> Result<ModId, String> {
        let mut vec_ref = self.0.modules.lock().unwrap();
        let new_id = ModId(vec_ref.len());
        vec_ref.push(Rc::new(module));
        Ok(new_id)
    }

    /// Returns a reference to the Environment used by the runner
    pub fn environment(&self) -> &Environment {
        &self.0.environment
    }

    /// Returns a reference to the Space associated with the runner's top module
    pub fn space(&self) -> &DynSpace {
        &self.0.top_mod_space
    }

    /// Returns the [DynSpace] handle associated with any loaded module's Space
    pub fn module_space(&self, mod_id: ModId) -> DynSpace {
        let modules = self.0.modules.lock().unwrap();
        modules.get(mod_id.0).unwrap().space().clone()
    }

    /// Returns a buffer containing the specified resource, if it is available from a loaded module
    pub fn get_module_resource(&self, mod_id: ModId, res_key: ResourceKey) -> Result<Resource, String> {
        let modules = self.0.modules.lock().unwrap();
        modules.get(mod_id.0).unwrap().get_resource(res_key)
    }

    /// Returns a reference to the Tokenizer associated with the runner's top module
    pub fn tokenizer(&self) -> &Shared<Tokenizer> {
        &self.0.top_mod_tokenizer
    }

    pub fn settings(&self) -> &PragmaSettings {
        &self.0.settings
    }

    pub fn get_setting_string(&self, key: &str) -> Option<String> {
        self.0.settings.get(key).map(|a| a.to_string())
    }

    pub fn run(&self, parser: impl Parser) -> Result<Vec<Vec<Atom>>, String> {
        let state = RunnerState::new_with_parser(self, Box::new(parser));
        state.run_to_completion()
    }

    pub fn run_in_module(&self, mod_id: ModId, parser: impl Parser) -> Result<Vec<Vec<Atom>>, String> {
        let mut state = RunnerState::new_with_module(self, mod_id);
        state.i_wrapper.input_src.push_parser(Box::new(parser));
        state.run_to_completion()
    }

    pub fn evaluate_atom(&self, atom: Atom) -> Result<Vec<Atom>, String> {
        let atom = if is_bare_minimal_interpreter(self) {
            atom
        } else {
            wrap_atom_by_metta_interpreter(self.module_space(ModId::TOP), atom)
        };
        if self.type_check_is_enabled()  {
            let types = get_atom_types(&self.module_space(ModId::TOP), &atom);
            if types.iter().all(AtomType::is_error) {
                return Ok(types.into_iter().map(|t| t.into_err_message()).collect());
            }
        }
        interpret(self.space().clone(), &atom)
    }

    fn type_check_is_enabled(&self) -> bool {
        self.settings().get_string("type-check").map_or(false, |val| val == "auto")
    }

}

// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*
// RunnerState & related objects
// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*

//TODO: Proposed API change to eliminate RunnerState from public API
// After a lot of experimentation with a design that is capable of moving execution work across threads,
// I think it makes sense to reverse course on the idea to expose "RunnerState" public API object.
//
//In essence, I am running into all exactly the same set of challenges that async Rust went through,
// but their solution is very involved.  For example: https://rust-lang.github.io/async-book/04_pinning/01_chapter.html 
// We could end up using the same approaches (and possibly utilizing the same mechanims (like pinning)), but
// I feel like that is overkill for what we require from Rust-language interoperability.
//
//Instead, I would like to simplify the Runner API to include a delegate interface that the runner will call
// with periodic events and status updates.  This delegate interface would then be used to implement a
// debugger and any other code that needs to influence execution from outside the MeTTa runner.
//
//Multi-threading inside a single Runner is tricky no matter which design we choose, but my thinking is that
// the delegate would be an FnMut closure that is called by the receiver on a mpsc channel, so delegate
// callbacks will always happen on the same thread, and the callback won't need to be Send nor Sync.
//
//So, the API change will be:
// - `run_step` goes away, and is replaced by a delegate that is called periodically, and has the ability to:
//  * receive incremental new results
//  * interrupt (terminate) execution early
//  * access additional debug info (specifics TBD)
// - New runner APIs including: `Metta::run_from_parser`, `Metta::run_atoms`, etc. will replace existing
//    RunnerState APIs like `RunnerState::new_with_parser`, etc.
//

/// A RunnerState encapsulates a single in-flight process, executing code within a [Metta] runner
pub struct RunnerState<'m, 'i> {
    metta: &'m Metta,
    mod_id: ModId,
    mod_ptr: Option<Rc<MettaMod>>,
    init_state: ModuleInitState,
    i_wrapper: InterpreterWrapper<'i>,
}

impl std::fmt::Debug for RunnerState<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RunnerState")
            .field("mode", &self.i_wrapper.mode)
            .field("interpreter_state", &self.i_wrapper.interpreter_state)
            .finish()
    }
}

impl<'m, 'input> RunnerState<'m, 'input> {

    fn new_internal(metta: &'m Metta, mod_id: ModId, init_state: ModuleInitState) -> Self {
        Self {
            metta,
            mod_id,
            mod_ptr: None,
            init_state: init_state,
            i_wrapper: InterpreterWrapper::default(),
        }
    }

    /// Returns a new RunnerState to execute code in the context of a MeTTa runner's top module
    pub fn new(metta: &'m Metta) -> Self {
        Self::new_with_module(metta, ModId::TOP)
    }

    /// Creates a new RunnerState to be used in the process of loading a new module
    pub(crate) fn new_for_loading(metta: &'m Metta, new_mod_name: &str, init_state: &mut ModuleInitState) -> Self {
        let normalized_name = normalize_relative_module_name("top", &new_mod_name).unwrap();
        let mod_id = init_state.push(normalized_name);
        Self::new_internal(metta, mod_id, init_state.new_child())
    }

    /// Creates a new RunnerState to be used in the process of loading a new module
    pub(crate) fn new_with_module_and_init_state(metta: &'m Metta, mod_id: ModId, init_state: ModuleInitState) -> Self {
        let mut state = Self::new_internal(metta, mod_id, init_state);
        let mod_ptr = state.init_state.get_mod_ptr(metta, mod_id).unwrap();
        state.mod_ptr = Some(mod_ptr);
        state
    }

    /// Returns a new RunnerState to execute code in the context a module in the runner
    pub(crate) fn new_with_module(metta: &'m Metta, mod_id: ModId) -> Self {
        Self::new_with_module_and_init_state(metta, mod_id, ModuleInitState::empty())
    }

    /// Returns a new RunnerState, for running code from the [SExprParser] with the specified [Metta] runner
    pub fn new_with_parser(metta: &'m Metta, parser: Box<dyn Parser + 'input>) -> Self {
        let mut state = Self::new(metta);
        state.i_wrapper.input_src.push_parser(parser);
        state
    }

    /// Returns a new RunnerState, for running code encoded as a slice of [Atom]s with the specified [Metta] runner
    pub fn new_with_atoms(metta: &'m Metta, atoms: &'input[Atom]) -> Self {
        let mut state = Self::new(metta);
        state.i_wrapper.input_src.push_parser(Box::new(atoms));
        state
    }

    /// Repeatedly steps a RunnerState until it is complete, and then returns the results
    pub fn run_to_completion(mut self) -> Result<Vec<Vec<Atom>>, String> {
        while !self.is_complete() {
            self.run_step()?;
        }
        Ok(self.into_results())
    }

    /// Runs one step of the interpreter
    pub fn run_step(&mut self) -> Result<(), String> {
        self.run_in_context(|context| context.step())
    }

    /// Returns `true` if the RunnerState has completed execution of all input or has encountered a
    ///    fatal error, otherwise returns `false`
    pub fn is_complete(&self) -> bool {
        self.i_wrapper.mode == MettaRunnerMode::TERMINATE
    }

    /// Returns a reference to the current in-progress results within the RunnerState
    pub fn current_results(&self) -> &Vec<Vec<Atom>> {
        &self.i_wrapper.results
    }

    /// Consumes the RunnerState and returns the final results
    pub fn into_results(self) -> Vec<Vec<Atom>> {
        self.i_wrapper.results
    }

    /// Private method.  Creates the Runner's context, and executes an arbitrary function within that context
    //TODO: When we eliminate the RunnerState, this method should become a private method of Metta,
    // and an argument of type `Option<ModId>` should be added.  When this function is used to initialize
    // modules, the module type can be returned from this function
    fn run_in_context<T, F: FnOnce(&mut RunContext<'_, 'input>) -> Result<T, String>>(&mut self, f: F) -> Result<T, String> {

        // Construct the RunContext
        let mut context = RunContext {
            metta: &self.metta,
            mod_id: self.mod_id,
            mod_ptr: &mut self.mod_ptr,
            init_state: &mut self.init_state,
            i_wrapper: &mut self.i_wrapper,
        };

        //TODO-HACK: This is a terrible horrible ugly hack that should be cleaned up ASAP.  It will cause
        // UB when we have multiple runner threads that execute concurrently.
        //Push the RunContext so the MeTTa Ops can access it.  The context ought to be passed as an argument
        // to the execute functions, in the absence of the hack
        self.metta.0.context.lock().unwrap().push(Arc::new(Mutex::new( unsafe{ std::mem::transmute(&mut context) } )));
        //END HORRIBLE HACK

        // Call our function
        let result = f(&mut context);

        //TODO-HACK: This is a terrible horrible ugly hack that should be cleaned up ASAP.
        //pop the context in the runner
        self.metta.0.context.lock().unwrap().pop();
        //END HORRIBLE HACK

        result
    }

    /// Internal method to unpack a RunnerState that just initialized a module (and its children)
    pub(crate) fn finalize_loading(self) -> Result<ModId, String> {
        for result_vec in self.i_wrapper.results {
            for result in result_vec {
                if atom_is_error(&result) {
                    return Err(atom_error_message(&result).to_owned())
                }
            }
        }
        let mod_ptr = match self.mod_ptr {
            Some(mod_ptr) => mod_ptr,
            None => return Err(format!("Module loader finished without running RunContext::init_self_module"))
        };

        self.init_state.in_frame(self.mod_id, |frame| frame.the_mod = Some(mod_ptr));
        Ok(self.mod_id)
    }
}

// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*
// RunContext & related objects
// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*

/// Runtime data that is available to Grounded Atom execution
// TODO: I think we may be able to remove the `'interpreter`` lifetime after the minimal MeTTa migration
//  because the lifetime is separated on account of the inability of the compiler to shorten a lifetime
//  used as a generic parameter on a trait.  In this case, the `Plan` trait.
pub struct RunContext<'a, 'input> {
    metta: &'a Metta,
    mod_id: ModId,
    mod_ptr: &'a mut Option<Rc<MettaMod>>,
    init_state: &'a mut ModuleInitState,
    i_wrapper: &'a mut InterpreterWrapper<'input>
}

impl std::fmt::Debug for RunContext<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RunContext")
         .finish()
    }
}

impl<'input> RunContext<'_, 'input> {
    /// Returns access to the Metta runner that is hosting the context 
    pub fn metta(&self) -> &Metta {
        &self.metta
    }

    /// Returns access to the context's current module
    pub fn module(&self) -> &MettaMod {
        self.mod_ptr.as_ref().unwrap_or_else(|| panic!("No module available"))
    }

    /// Returns mutable access the context's current module, if possible
    pub fn module_mut(&mut self) -> Option<&mut MettaMod> {
        Rc::get_mut(self.mod_ptr.as_mut().unwrap_or_else(|| panic!("No module available")))
    }

    /// Pushes the parser as a source of operations to subsequently execute
    pub fn push_parser(&mut self, parser: Box<dyn Parser + 'input>) {
        self.i_wrapper.input_src.push_parser(parser);
    }

    /// Pushes the atoms as a source of operations to subsequently execute
    pub fn push_atoms(&mut self, atoms: &'input[Atom]) {
        self.i_wrapper.input_src.push_parser(Box::new(atoms));
    }

    /// Pushes an executable function as an operation to be executed
    pub fn push_func<F: FnOnce(&mut RunContext) -> Result<(), String> + 'input>(&mut self, f: F) {
        self.i_wrapper.input_src.push_func(f);
    }

    /// Creates a child RunContext within the current context, for immediate inline execution
    //
    //Internal Note: There were two implementation options here: 1. for the RunContext / RunnerState
    // to contain a stack of contexts / interpreter states or 2. to resolve the child context inline.
    // This function implements option 2.
    //
    // If we intended to stay with the RunnerState design where the caller has direct control over
    // the step-loop, Option 1 would be a superior design because it keeps an outer step correlated
    // to a unit of inner interpreter-work, regardless of whether that work is happening at a context
    // or a nested sub-context.  However I want to move toward removing the step-loop from the public
    // API, so I chose Option 2 for now.  See the comment beginning with:
    // "Proposed API change to eliminate RunnerState from public API"
    //
    pub fn run_inline<F: FnOnce(&mut RunContext) -> Result<(), String>>(&mut self, f: F) -> Result<Vec<Vec<Atom>>, String> {
        let mut new_interpreter = InterpreterWrapper::default();
        let mut new_context = RunContext {
            metta: &self.metta,
            i_wrapper: &mut new_interpreter,
            mod_id: self.mod_id,
            mod_ptr: self.mod_ptr,
            init_state: self.init_state,
        };

        let mut err = None;
        match f(&mut new_context) {
            Ok(_) => {
                while new_context.i_wrapper.mode != MettaRunnerMode::TERMINATE {
                    if new_context.step().is_err() {
                        break;
                    }
                }
            },
            Err(e) => err = Some(e)
        }

        match err {
            None => Ok(new_interpreter.results),
            Some(e) => Err(e)
        }
    }

    /// Runs the function in the context of the mod_id
    #[allow(dead_code)] //Some clients are behind feature gates
    fn in_mod_context<T, F: FnOnce(&mut RunContext) -> Result<T, String>>(&mut self, mod_id: ModId, f: F) -> Result<T, String> {
        if mod_id == self.mod_id {
            f(self)
        } else {
            let mut state = RunnerState::new_with_module_and_init_state(&self.metta, mod_id, self.init_state.new_child());
            state.run_in_context(f)
        }
    }

    /// Locates and retrieves a loaded module based on its name
    pub fn get_module_by_name(&self, mod_name: &str) -> Result<ModId, String> {
        let normalized_mod_name = normalize_relative_module_name(self.module().path(), mod_name)?;
        self.init_state.get_module_by_name(&self.metta, &normalized_mod_name)
    }

    /// Adds a ModId to the named module tree with the specified name
    ///
    /// NOTE: If this method is called during module load, and the module load fails, the
    /// added name will not become part of the runner's module namespace
    fn add_module_to_name_tree(&mut self, mod_name: &str, mod_id: ModId) -> Result<(), String>  {
        let normalized_mod_name = normalize_relative_module_name(self.module().path(), mod_name)?;
        self.init_state.add_module_to_name_tree(&self.metta, self.mod_id, &normalized_mod_name, mod_id)
    }

    /// Normalize a module name into a canonical name-path form, and expanding a relative module-path
    pub fn normalize_module_name(&self, mod_name: &str) -> Result<String, String> {
        let self_mod_path = self.mod_ptr.as_ref()
            .map(|mod_ptr| mod_ptr.path()).ok_or_else(|| "RunContext::init_self_module must be called prior to this operation".to_string())?;
        normalize_relative_module_name(self_mod_path, mod_name)
    }

    /// Initiates the loading of a module from a runner thread.  Useful for loading sub-modules 
    pub fn load_module_direct(&mut self, loader: Box<dyn ModuleLoader>, mod_name: &str) -> Result<ModId, String> {

        //Make sure we don't have a conflicting mod, before attempting to load this one
        if self.get_module_by_name(mod_name).is_ok() {
            return Err(format!("Attempt to load module with name that conflicts with existing module: {mod_name}"));
        }

        let absolute_mod_name = self.normalize_module_name(mod_name)?;
        self.init_module(&absolute_mod_name, loader)
    }

    /// A version of [Metta::load_module_at_path] Useful for loading sub-modules 
    #[cfg(feature = "pkg_mgmt")]
    pub fn load_module_at_path<P: AsRef<std::path::Path>>(&mut self, path: P, mod_name: Option<&str>) -> Result<ModId, String> {

        let absolute_mod_name = match mod_name {
            Some(mod_name) => {
                if self.get_module_by_name(mod_name).is_ok() {
                    return Err(format!("Attempt to load module with name that conflicts with existing module: {mod_name}"));
                }
                Some(self.normalize_module_name(mod_name)?)
            },
            None => None
        };

        // Get the loader and descriptor by trying the module formats
        let (loader, descriptor) = match loader_for_module_at_path(self.metta.environment().fs_mod_formats(), &path, absolute_mod_name.as_deref(), self.module().resource_dir())? {
            Some((loader, descriptor)) => (loader, descriptor),
            None => return Err(format!("Failed to resolve module at path: {}", path.as_ref().display()))
        };

        let mod_name = match absolute_mod_name {
            Some(mod_name) => mod_name,
            None => descriptor.name().to_string()
        };

        // Load the module from the loader
        self.get_or_init_module_with_descriptor(&mod_name, descriptor, loader)
    }

    /// A version of [Metta::load_module_alias] Useful for defining sub-module aliases
    pub fn load_module_alias(&mut self, mod_name: &str, mod_id: ModId) -> Result<ModId, String> {

        if self.get_module_by_name(&mod_name).is_ok() {
            return Err(format!("Attempt to create module alias with name that conflicts with existing module: {mod_name}"));
        }
        self.add_module_to_name_tree(&mod_name, mod_id)?;
        Ok(mod_id)
    }

    /// Initializes the context's module.  Used in the implementation of a module loader function
    ///
    /// Prior to calling this function, any attempt to access the active module in the RunContext will
    /// lead to a panic.
    pub fn init_self_module(&mut self, space: DynSpace, resource_dir: Option<PathBuf>) {
        if self.mod_ptr.is_some() {
            panic!("Module already initialized")
        }
        *self.mod_ptr = Some(self.init_state.in_frame(self.mod_id, |frame| {
            frame.init_self_module(self.mod_id, &self.metta, space, resource_dir)
        }));
    }

    /// Resolves a dependency module from a name, according to the [PkgInfo] of the current module,
    /// and loads it into the runner, if it's not already loaded
    pub fn load_module(&mut self, mod_name: &str) -> Result<ModId, String> {
        let absolute_mod_path = self.normalize_module_name(mod_name)?;

        // See if we already have the module loaded
        if let Ok(mod_id) = self.get_module_by_name(&absolute_mod_path) {
            return Ok(mod_id);
        }

        #[cfg(not(feature = "pkg_mgmt"))]
        return Err(format!("Failed to resolve module {absolute_mod_path}"));

        // Resolve the module name into a loader object using the resolution logic in the pkg_info
        #[cfg(feature = "pkg_mgmt")]
        {
            let parent_mod_id = self.load_module_parents(&absolute_mod_path)?;
            self.load_module_internal(&absolute_mod_path, parent_mod_id)
        }
    }

    /// Internal function used for recursive loading of parent modules by [Self::load_module]
    /// Returns the ModId of the loaded parent module
    #[cfg(feature = "pkg_mgmt")]
    fn load_module_parents(&mut self, mod_name: &str) -> Result<ModId, String> {

        //Make sure the parent module is loaded, and descend recursively until we find a loaded parent
        let mod_name_components = decompose_name_path(mod_name)?;
        let parent_mod_id = if mod_name_components.len() > 1 {
            let parent_name = compose_name_path(&mod_name_components[..mod_name_components.len()-1])?;
            if let Ok(parent_mod_id) = self.get_module_by_name(&parent_name) {
                parent_mod_id
            } else {
                let parent_of_parent = self.load_module_parents(&parent_name)?;
                self.load_module_internal(&parent_name, parent_of_parent)?
            }
        } else {
            ModId::TOP
        };
        Ok(parent_mod_id)
    }

    /// Internal method to retrieve the mod_ptr to a module that's either loading in the
    /// InitFrame, or loaded into the runner
    fn get_mod_ptr(&self, mod_id: ModId) -> Result<Rc<MettaMod>, String> {
        self.init_state.get_mod_ptr(&self.metta, mod_id)
    }

    /// Internal function to load a module in the context of a parent module, assuming the path is normalized
    #[cfg(feature = "pkg_mgmt")]
    fn load_module_internal(&mut self, mod_path: &str, parent_mod_id: ModId) -> Result<ModId, String> {
        self.in_mod_context(parent_mod_id, |context| {
            match resolve_module(context.module().pkg_info(), context, mod_path)? {
                Some((loader, descriptor)) => {
                    context.get_or_init_module_with_descriptor(mod_path, descriptor, loader)
                },
                None => {return Err(format!("Failed to resolve module {mod_path}"))}
            }
        })
    }

    /// Resolves a dependency module from a name, according to the [PkgInfo] of the current module,
    /// and loads the specified resource from the module, without loading the module itself
    ///
    /// NOTE: Although this method won't load the module itself, it will load parent modules if necessary
    pub fn load_resource_from_module(&mut self, mod_name: &str, res_key: ResourceKey) -> Result<Resource, String> {

        // Resolve the module name and see if the module is already loaded into the runner
        if let Ok(mod_id) = self.get_module_by_name(mod_name) {
            self.metta().get_module_resource(mod_id, res_key)
        } else {
            #[cfg(not(feature = "pkg_mgmt"))]
            return Err(format!("Failed to resolve module {mod_name}"));

            // Ensure the module's parents are loaded if a module path was provided
            #[cfg(feature = "pkg_mgmt")]
            {
                let parent_mod_id = self.load_module_parents(mod_name)?;
                let normalized_mod_path = self.normalize_module_name(mod_name)?;
                self.in_mod_context(parent_mod_id, |context| {
                    match resolve_module(context.module().pkg_info(), context, &normalized_mod_path)? {
                        Some((loader, _descriptor)) => {
                            loader.get_resource(res_key)
                        },
                        None => {return Err(format!("Failed to resolve module {mod_name}"))}
                    }
                })
            }
        }
    }

    #[cfg(feature = "pkg_mgmt")]
    /// Checks the loaded [ModuleDescriptor]s to see if a given module has already been loaded, and returns
    /// that if it has.  Otherwise loads the module
    ///
    /// ## Explanation of behavior
    /// * `mod_name` should not speicify an existing loaded module; If it does the caller should not have
    ///   called this method
    /// * If `descriptor` matches an existing loaded module, alias in the module name-space will be created,
    ///   and the module's ModId will be returned, otherwise,
    /// * The `loader` will be used to initialize a new module, and the new ModId will be returned
    pub(crate) fn get_or_init_module_with_descriptor(&mut self, mod_name: &str, descriptor: ModuleDescriptor, loader: Box<dyn ModuleLoader>) -> Result<ModId, String> {
        match self.init_state.get_module_with_descriptor(&self.metta, &descriptor) {
            Some(mod_id) => {
                self.load_module_alias(mod_name, mod_id)
            },
            None => {
                let new_id = self.init_module(mod_name, loader)?;
                self.init_state.add_module_descriptor(&self.metta, descriptor, new_id);
                Ok(new_id)
            }
        }
    }

    /// Internal method, Returns the ModId of a module initialized with the provided loader
    ///
    /// The init function will then call `context.init_self_module()` along with any other initialization code
    fn init_module(&mut self, mod_name: &str, loader: Box<dyn ModuleLoader>) -> Result<ModId, String> {
        let new_mod_id = self.init_state.init_module(&self.metta, mod_name, loader)?;

        if self.init_state.is_root() {
            let mut init_state = ModuleInitState::empty();
            core::mem::swap(&mut init_state, self.init_state);
            self.metta.merge_init_state(init_state)
        } else {
            Ok(new_mod_id)
        }
    }

    /// Adds a loaded module as a dependency of the `&self` [MettaMod], and adds a [Tokenizer] entry to access
    /// the dependent module's Space.
    pub fn import_dependency_as(&self, mod_id: ModId, name: Option<String>) -> Result<(), String> {
        self.module().import_dependency_as(self.get_mod_ptr(mod_id)?, name)
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
    // More discussion on the topic of tokenizer entry names is here https://github.com/trueagi-io/hyperon-experimental/issues/510
    pub fn import_item_from_dependency_as(&self, from_name: &str, mod_id: ModId, name: Option<&str>) -> Result<(), String> {
        self.module().import_item_from_dependency_as(from_name, self.get_mod_ptr(mod_id)?, name)
    }

    /// Effectively adds all atoms in a dependency module to the &self module, by adding the dependency
    /// module's space as an atom inside the &self module
    ///
    /// WARNING: Module import behavior is still WIP, specifically around "import *" behavior, and
    /// especially around transitive imports
    pub fn import_all_from_dependency(&self, mod_id: ModId) -> Result<(), String> {
        self.module().import_all_from_dependency(mod_id, self.get_mod_ptr(mod_id)?, self.metta)
    }

    /// Private method to advance the context forward one step
    fn step(&mut self) -> Result<(), String> {

        // If we're in the middle of interpreting an atom...
        if let Some(interpreter_state) = core::mem::take(&mut self.i_wrapper.interpreter_state) {

            if interpreter_state.has_next() {

                //Take a step with the interpreter, and put it back for next time
                self.i_wrapper.interpreter_state = Some(interpret_step(interpreter_state))
            } else {

                //This interpreter is finished, process the results
                let result = interpreter_state.into_result().unwrap();
                let error = result.iter().any(|atom| atom_is_error(atom));
                self.i_wrapper.results.push(result);
                if error {
                    self.i_wrapper.mode = MettaRunnerMode::TERMINATE;
                    return Ok(());
                }
            }

            Ok(())
        } else {

            // Get the next operation
            let tokenizer_option = self.mod_ptr.as_ref().map(|module| module.tokenizer().borrow());
            let tokenizer = tokenizer_option.as_ref().map(|tok| &**tok as &Tokenizer);
            let next_op = match self.i_wrapper.input_src.next_op(tokenizer) {
                Ok(atom) => atom,
                Err(err) => {
                    self.i_wrapper.mode = MettaRunnerMode::TERMINATE;
                    return Err(err);
                }
            };
            drop(tokenizer_option);

            // Start execution of the operation
            match next_op {
                Some(Executable::Func(func)) => {
                    func(self)
                },
                // If the next operation is an atom, start a new intperpreter
                Some(Executable::Atom(atom)) => {
                    if atom == EXEC_SYMBOL {
                        self.i_wrapper.mode = MettaRunnerMode::INTERPRET;
                        return Ok(());
                    }
                    match self.i_wrapper.mode {
                        MettaRunnerMode::ADD => {
                            if let Err(atom) = self.module().add_atom(atom, self.metta.type_check_is_enabled()) {
                                self.i_wrapper.results.push(vec![atom]);
                                self.i_wrapper.mode = MettaRunnerMode::TERMINATE;
                                return Ok(());
                            }
                        },
                        MettaRunnerMode::INTERPRET => {

                            if self.metta.type_check_is_enabled() && !validate_atom(&self.module().space(), &atom) {
                                let typ = get_atom_types_v2(&self.module().space(), &atom);
                                let type_err_exp = typ.iter().nth(0).unwrap().get_err_message().clone();
                                self.i_wrapper.interpreter_state = Some(InterpreterState::new_finished(self.module().space().clone(), vec![type_err_exp]));
                            } else {
                                let atom = if is_bare_minimal_interpreter(self.metta) {
                                    atom
                                } else {
                                    wrap_atom_by_metta_interpreter(self.module().space().clone(), atom)
                                };
                                self.i_wrapper.interpreter_state = Some(interpret_init(self.module().space().clone(), &atom));
                                if let Some(depth) = self.metta.settings().get_string("max-stack-depth") {
                                    let depth = depth.parse::<usize>().unwrap();
                                    self.i_wrapper.interpreter_state.as_mut().map(|state| state.set_max_stack_depth(depth));
                                }
                            }
                        },
                        MettaRunnerMode::TERMINATE => {
                            return Ok(());
                        },
                    }
                    self.i_wrapper.mode = MettaRunnerMode::ADD;
                    Ok(())
                },
                None => {
                    self.i_wrapper.mode = MettaRunnerMode::TERMINATE;
                    Ok(())
                }
            }
        }
    }

}

fn is_bare_minimal_interpreter(metta: &Metta) -> bool {
    metta.settings().get_string("interpreter") == Some("bare-minimal".into())
}

// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*
// InterpreterWrapper & related objects
// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*

/// Private structure to contain everything associated with an InterpreterState.
/// This is basically the part of RunContext that lasts across calls to run_step
#[derive(Default)]
struct InterpreterWrapper<'i> {
    mode: MettaRunnerMode,
    input_src: InputStream<'i>,
    interpreter_state: Option<InterpreterState>,
    results: Vec<Vec<Atom>>,
}

#[derive(Debug, Default, PartialEq, Eq)]
enum MettaRunnerMode {
    #[default]
    ADD,
    INTERPRET,
    TERMINATE,
}

/// Private type representing a source for operations for the runner
enum InputSource<'i> {
    Parser(Box<dyn Parser + 'i>),
    Func(Box<dyn FnOnce(&mut RunContext) -> Result<(), String> + 'i>)
}

/// Private type representing an input operation for the runner
/// FUTURE-CLEANUP-TODO: I would like to be able to delete this `Executable` type and simplify this code
/// by making the runner's only instructions be a stream of atoms.  However, it an important aspect of
/// the runner's abstactions (and necessary functionality for module loading, etc.) is the ability to
/// dispatch one-off functions to execute inside the runner.  Therefore, the most sensible design would
/// be to allow those functions to be embedded within grounded atoms.  Right now, there are two things
/// that stand in the way of that design:
/// 1.  Atoms have a 'static lifetime, but a lot of the value of dispatching special functions is to
///   interact with the caller, and this requiring a 'static lifetime bound on the function drastically
///   limits the usefullness of the feature.
///     More specifically, the module loader functions are borrowed from the Environment, and the
///   environment may not be 'static.  So we would need to move loader functions out of the Environment,
///   Which is doable.
///     However, if we implement "inside-out atoms" (atoms with a lifetime bound) we may be able to solve
///   this more elegantly.
/// 2.  The runner's RunContext is not available to execution of atoms in the current API.  Although
///   hopefully this will be addressed shortly
enum Executable<'i> {
    Atom(Atom),
    Func(Box<dyn FnOnce(&mut RunContext) -> Result<(), String> + 'i>)
}

/// A private structure representing a heterogeneous source of operations for the runner to execute
#[derive(Default)]
struct InputStream<'a>(Vec<InputSource<'a>>);

impl<'i> InputStream<'i> {
    fn push_parser(&mut self, parser: Box<(dyn Parser + 'i)>) {
        self.0.push(InputSource::Parser(parser))
    }
    fn push_func<F: FnOnce(&mut RunContext) -> Result<(), String> + 'i>(&mut self, f: F) {
        self.0.push(InputSource::Func(Box::new(f)))
    }
    /// Returns the next operation in the InputStream, and removes it from the stream.  Returns None if the
    /// InputStream is empty.
    fn next_op(&mut self, tokenizer: Option<&Tokenizer>) -> Result<Option<Executable<'i>>, String> {
        match self.0.get_mut(0) {
            None => Ok(None),
            Some(src) => {
                match src {
                    InputSource::Func(_) => match self.0.remove(0) {
                        InputSource::Func(f) => Ok(Some(Executable::Func(f))),
                        _ => unreachable!()
                    },
                    InputSource::Parser(parser) => {
                        match parser.next_atom(tokenizer.as_ref()
                            .unwrap_or_else(|| panic!("Module must be initialized to parse MeTTa code")))? {
                            Some(atom) => Ok(Some(Executable::Atom(atom))),
                            None => {
                                self.0.remove(0);
                                self.next_op(tokenizer)
                            }
                        }
                    }
                }
            }
        }
    }
}

fn wrap_atom_by_metta_interpreter(space: DynSpace, atom: Atom) -> Atom {
    let space = Atom::gnd(space);
    let interpret = Atom::expr([METTA_SYMBOL, atom, ATOM_TYPE_UNDEFINED, space]);
    interpret
}

// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*
// Tests
// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*

#[cfg(test)]
pub fn run_program(program: &str) -> Result<Vec<Vec<Atom>>, String> {
    let metta = Metta::new(Some(EnvBuilder::test_env()));
    metta.run(SExprParser::new(program))
}

#[cfg(test)]
mod tests {
    use crate::metta::runner::number::Number;
    use super::*;
    use super::bool::Bool;

    #[test]
    fn test_space() {
        let program = "
            (= (And T T) T)
            (= (frog $x)
                (And (croaks $x)
                     (eat_flies $x)))
            (= (croaks Fritz) T)
            (= (eat_flies Fritz) T)
            (= (green $x) (frog $x))
            !(green Fritz)
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![Atom::sym("T")]]));
    }

    #[test]
    fn metta_add_type_check() {
        let program = "
            (: foo (-> A B))
            (: b B)
            (foo b)
        ";

        let metta = Metta::new_core(None, Some(EnvBuilder::test_env()));
        metta.settings().set("type-check".into(), sym!("auto"));
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") ("BadType" {Number::Integer(0)} ("B") ("A")))]]));
    }

    #[test]
    fn metta_interpret_type_check() {
        let program = "
            (: foo (-> A B))
            (: b B)
            !(foo b)
        ";

        let metta = Metta::new_core(None, Some(EnvBuilder::test_env()));
        metta.settings().set("type-check".into(), sym!("auto"));
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") ("BadType" {Number::Integer(0)} ("B") ("A")))]]));
    }

    #[derive(Clone, Debug)]
    struct ErrorOp{}

    grounded_op!(ErrorOp, "error");

    impl Grounded for ErrorOp {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED])
        }
        fn as_execute(&self) -> Option<&dyn CustomExecute> {
            Some(self)
        }
    }

    impl CustomExecute for ErrorOp {
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Err("TestError".into())
        }
    }

    #[test]
    fn metta_stop_run_after_error() {
        let program = "
            (= (foo) ok)
            !(error)
            !(foo)
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token_with_regex_str("error",
            |_| Atom::gnd(ErrorOp{}));
        let result = metta.run(SExprParser::new(program));

        assert_eq!(result, Ok(vec![vec![expr!("Error" ({ErrorOp{}}) "TestError")]]));
    }

    #[test]
    fn metta_stop_after_type_check_fails_on_add() {
        let program = "
            (: foo (-> A B))
            (: a A)
            (: b B)
            (foo b)
            !(foo a)
        ";

        let metta = Metta::new_core(None, Some(EnvBuilder::test_env()));
        metta.settings().set("type-check".into(), sym!("auto"));
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") ("BadType" {Number::Integer(0)} ("B") ("A")))]]));
    }

    #[test]
    fn metta_stop_after_error_happens_inside_tuple() {
        let metta = Metta::new_core(None, Some(EnvBuilder::test_env()));
        let program = "
            !(a b c (Error e SomeError))
        ";
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" "e" "SomeError")]]));

        let metta = Metta::new_core(None, Some(EnvBuilder::test_env()));
        let program = "
            !((Error e SomeError) a b c)
        ";
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" "e" "SomeError")]]));

        let metta = Metta::new_core(None, Some(EnvBuilder::test_env()));
        let program = "
            (: foo (-> A B))
            (: b B)
            !(s (foo b))
        ";
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") ("BadType" {Number::Integer(1)} "A" "B"))]]));
    }

    #[test]
    fn metta_first_call_in_the_tuple_has_incorrect_typing() {
        let metta = Metta::new_core(None, Some(EnvBuilder::test_env()));
        let program = "
            (: foo (-> A B))
            (: b B)
            !((foo b) s)
        ";
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") ("BadType" {Number::Integer(0)} ("B") ("A")))]]));
    }


    #[derive(Clone, PartialEq, Debug)]
    struct ReturnAtomOp(Atom);

    impl std::fmt::Display for ReturnAtomOp {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "return-atom {}", self.0)
        }
    }

    impl Grounded for ReturnAtomOp {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED])
        }
        fn as_execute(&self) -> Option<&dyn CustomExecute> {
            Some(self)
        }
    }

    impl CustomExecute for ReturnAtomOp {
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![self.0.clone()])
        }
    }

    #[test]
    fn metta_no_crash_on_empty_expression_returned() {
        let program = "
            !(empty)
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token_with_regex_str("empty",
            |_| Atom::gnd(ReturnAtomOp(expr!())));
        let result = metta.run(SExprParser::new(program));

        assert_eq!(result, Ok(vec![vec![expr!()]]));
    }

    #[test]
    fn metta_empty_results_issue_481() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));

        let program = "
            !(== () (collapse
              (let* (($L ()) ($x (superpose $L))) $x) ))
        ";
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![Atom::gnd(Bool(true))]]));

        let program = "!(let $x (empty) OK)";
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![]]));

        let program = "!(let* (($x (empty))) OK)";
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn metta_no_config_dir_by_default() {
        let metta = Metta::new(None);
        assert_eq!(metta.environment().config_dir(), None);
    }
}
