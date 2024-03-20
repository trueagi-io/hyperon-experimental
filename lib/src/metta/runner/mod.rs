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

use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::space::*;
use super::text::{Tokenizer, Parser, SExprParser};
use super::types::validate_atom;

pub mod modules;
use modules::{MettaMod, ModNameNode, ModuleLoader, TOP_MOD_NAME, ModNameNodeDisplayWrapper};
#[cfg(feature = "pkg_mgmt")]
use modules::catalog::{ModuleDescriptor, loader_for_module_at_path};

use std::rc::Rc;
use std::path::PathBuf;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

mod environment;
pub use environment::{Environment, EnvBuilder};

pub mod stdlib;
#[cfg(not(feature = "minimal"))]
use super::interpreter::{interpret, interpret_init, interpret_step, InterpreterState};

#[cfg(feature = "minimal")]
pub mod stdlib2;
#[cfg(feature = "minimal")]
use super::interpreter2::{interpret, interpret_init, interpret_step, InterpreterState};
#[cfg(feature = "minimal")]
use stdlib2::*;

use stdlib::CoreLibLoader;

pub mod arithmetics;

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

#[derive(Debug)]
pub struct MettaContents {
    /// All the runner's loaded modules
    modules: Mutex<Vec<Rc<MettaMod>>>,
    /// A tree to locate loaded mods by name
    module_names: Mutex<ModNameNode>,
    #[cfg(feature = "pkg_mgmt")]
    /// An index, to find a loaded module from a ModuleDescriptor
    module_descriptors: Mutex<HashMap<ModuleDescriptor, ModId>>,
    /// A clone of the top module's Space, so we don't need to do any locking to access it,
    /// to support the metta.space() public function
    top_mod_space: DynSpace,
    /// A clone of the top module's Tokenizer
    top_mod_tokenizer: Shared<Tokenizer>,
    /// The ModId of the extended stdlib to import into some modules loaded into the runner
    stdlib_mod: OnceLock<ModId>,
    /// The runner's pragmas, affecting runner-wide behavior
    settings: Shared<HashMap<String, Atom>>,
    /// The runner's Environment
    environment: Arc<Environment>,
    //TODO-HACK: This is a terrible horrible ugly hack that should not be merged.  Delete this field
    // The real context is an interface to the state in a run, and should not live across runs
    // This hack will fail badly if we end up running code from two different modules in parallel
    context: Arc<Mutex<Vec<Arc<Mutex<&'static mut RunContext<'static, 'static, 'static>>>>>>,
}

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
    pub fn new_with_stdlib_loader(loader: Option<&dyn ModuleLoader>, space: Option<DynSpace>, env_builder: Option<EnvBuilder>) -> Metta {

        //Create the raw MeTTa runner
        let metta = Metta::new_core(space, env_builder);

        //Load the "corelib" module into the runner
        let corelib_mod_id = metta.load_module_direct(&CoreLibLoader::default(), "corelib").expect("Failed to load corelib");

        //Load the stdlib if we have one, and otherwise make an alias to corelib
        let stdlib_mod_id = match loader {
            Some(loader) => metta.load_module_direct(loader, "stdlib").expect("Failed to load stdlib"),
            None => metta.load_module_alias("stdlib", corelib_mod_id).expect("Failed to create stdlib alias for corelib")
        };

        //Set the runner's stdlib mod_id
        metta.0.stdlib_mod.set(stdlib_mod_id).unwrap();

        //Import the stdlib into the top module, now that it is loaded
        let mut runner_state = RunnerState::new(&metta);
        runner_state.run_in_context(|context| {
            context.module().import_all_from_dependency(&metta, stdlib_mod_id).unwrap();
            Ok(())
        }).expect("Failed to import stdlib");
        drop(runner_state);

        //Run the `init.metta` file
        if let Some(init_meta_file_path) = metta.0.environment.initialization_metta_file_path() {
            let program = match std::fs::read_to_string(init_meta_file_path)
            {
                Ok(program) => program,
                Err(err) => panic!("Could not read file, path: {}, error: {}", init_meta_file_path.display(), err)
            };
            metta.run(SExprParser::new(program.as_str())).unwrap();
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
            None => DynSpace::new(GroundingSpace::new())
        };
        let settings = Shared::new(HashMap::new());
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
    pub fn load_module_direct(&self, loader: &dyn ModuleLoader, mod_name: &str) -> Result<ModId, String> {
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

    /// Locates and retrieves a loaded module based on its name, relative to the top of the runner
    fn get_module_by_name(&self, mod_name: &str) -> Result<ModId, String> {
        let module_names = self.0.module_names.lock().unwrap();
        module_names.resolve(mod_name).ok_or_else(|| format!("Unable to locate module: {mod_name}"))
    }

    /// Adds a ModId to the named module tree with the specified name, relative to the top of the runer
    fn add_module_to_name_tree(&self, mod_name: &str, mod_id: ModId) -> Result<(), String>  {
        let mut module_names = self.0.module_names.lock().unwrap();
        module_names.add(mod_name, mod_id)
    }

    //LP-QUESTION: I am not sure if this should be deleted as unnecessary, or exposed as part of the public API.
    // On the one hand, [RunContext::normalize_name_path] handles relative paths, and this function doesn't.
    // On the other hand, you don't always have a RunContext available.
    // /// Internal function to normalize a module name into a canonical name-path form
    // fn normalize_module_name(&self, mod_name: &str) -> Result<String, String> {
    //     let mod_name = match mod_name_relative_path(mod_name) {
    //         Some(_) => {return Err(format!("Relative module-path not allowed when loading modules through runner API: {mod_name}"))},
    //         None => mod_name,
    //     };
    //     ModNameNode::normalize_name_path(mod_name)
    // }

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
    /// Checks the runner's descriptors to see if a given module has already been loaded, and returns
    /// that if it has.  Otherwise loads the module
    fn get_or_init_module_with_descriptor<F: FnOnce(&mut RunContext) -> Result<(), String>>(&self, mod_name: &str, descriptor: ModuleDescriptor, f: F) -> Result<ModId, String> {
        match self.get_module_with_descriptor(&descriptor) {
            Some(mod_id) => return Ok(mod_id),
            None => {
                let new_id = self.init_module(mod_name, f)?;
                let mut descriptors = self.0.module_descriptors.lock().unwrap();
                descriptors.insert(descriptor, new_id);
                Ok(new_id)
            }
        }
    }

    /// Returns the ModId of a module, initializing it with the provided function if it isn't already loaded
    ///
    /// The init function will then call `context.init_self_module()` along with any other initialization code
    fn init_module<F: FnOnce(&mut RunContext) -> Result<(), String>>(&self, mod_name: &str, f: F) -> Result<ModId, String> {

        //Create a new RunnerState in order to initialize the new module, and push the init function
        // to run within the new RunnerState.  The init function will then call `context.init_self_module()`
        let mut runner_state = RunnerState::new_internal(&self, Some(mod_name.to_string()));
        runner_state.run_in_context(|context| {
            context.push_func(|context| f(context));
            Ok(())
        })?;

        //Finish the execution
        while !runner_state.is_complete() {
            runner_state.run_step()?;
        }

        //Add the newly initialized module to the Runner
        match runner_state.into_module() {
            Ok(mut module) => {
                if let Some(sub_module_names) = module.take_sub_module_names() {
                    self.merge_sub_module_names(module.path(), sub_module_names)?;
                }
                self.add_module(module)
            },
            Err(err_atom) => Err(atom_error_message(&err_atom).to_owned())
        }
    }

    /// Internal function to add a loaded module to the runner, assigning it a ModId
    fn add_module(&self, module: MettaMod) -> Result<ModId, String> {
        let mut vec_ref = self.0.modules.lock().unwrap();
        let new_id = ModId(vec_ref.len());
        vec_ref.push(Rc::new(module));
        Ok(new_id)
    }

    fn merge_sub_module_names(&self, root_name: &str, subtree: ModNameNode) -> Result<(), String> {
        //LP-TODO-NEXT: This call only takes a single level of hierarchy into account,
        // but modules are loaded from the inside-out, meaning the parent won't be available when
        // the children are loaded for hierarchical loading.  This fix requires changing the way
        // modules are stored when they are in the process of being loaded.
        let mut module_names = self.0.module_names.lock().unwrap();
        module_names.merge_subtree_into(root_name, subtree)
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

    /// Returns a reference to the Tokenizer associated with the runner's top module
    pub fn tokenizer(&self) -> &Shared<Tokenizer> {
        &self.0.top_mod_tokenizer
    }

    pub fn settings(&self) -> &Shared<HashMap<String, Atom>> {
        &self.0.settings
    }

    pub fn set_setting(&self, key: String, value: Atom) {
        self.0.settings.borrow_mut().insert(key, value);
    }

    pub fn get_setting(&self, key: &str) -> Option<Atom> {
        self.0.settings.borrow().get(key.into()).cloned()
    }

    pub fn get_setting_string(&self, key: &str) -> Option<String> {
        self.0.settings.borrow().get(key.into()).map(|a| a.to_string())
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

    // TODO: this method is deprecated and should be removed after switching
    // to the minimal MeTTa
    pub fn evaluate_atom(&self, atom: Atom) -> Result<Vec<Atom>, String> {
        #[cfg(feature = "minimal")]
        let atom = if is_bare_minimal_interpreter(self) {
            atom
        } else {
            wrap_atom_by_metta_interpreter(self.0.top_mod_space.clone(), atom)
        };
        if self.type_check_is_enabled() && !validate_atom(self.0.top_mod_space.borrow().as_space(), &atom) {
            Ok(vec![Atom::expr([ERROR_SYMBOL, atom, BAD_TYPE_SYMBOL])])
        } else {
            interpret(self.space(), &atom)
        }
    }

    fn type_check_is_enabled(&self) -> bool {
        self.get_setting_string("type-check").map_or(false, |val| val == "auto")
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
    module: StateMod,
    i_wrapper: InterpreterWrapper<'m, 'i>,
}

impl std::fmt::Debug for RunnerState<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RunnerState")
            .field("mode", &self.i_wrapper.mode)
            .field("interpreter_state", &self.i_wrapper.interpreter_state)
            .finish()
    }
}

/// Internal, refers to the MeTTa module used by a RunnerState
#[derive(Debug)]
enum StateMod {
    None(Option<String>), //This means there is no module initialized, but a new module will get this name
    UseLoaded(ModId),
    Initializing(MettaMod),
}

impl<'m, 'input> RunnerState<'m, 'input> {

    fn new_internal(metta: &'m Metta, new_mod_name: Option<String>) -> Self {
        Self {
            metta,
            module: StateMod::None(new_mod_name),
            i_wrapper: InterpreterWrapper::default()
        }
    }

    /// Returns a new RunnerState to execute code in the context of a MeTTa runner's top module
    pub fn new(metta: &'m Metta) -> Self {
        Self::new_with_module(metta, ModId::TOP)
    }

    /// Returns a new RunnerState to execute code in the context of any loaded module
    pub(crate) fn new_with_module(metta: &'m Metta, mod_id: ModId) -> Self {
        let mut state = Self::new_internal(metta, None);
        state.module = StateMod::UseLoaded(mod_id);
        state
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
    fn run_in_context<T, F: FnOnce(&mut RunContext<'_, 'm, 'input>) -> Result<T, String>>(&mut self, f: F) -> Result<T, String> {

        // Construct the RunContext
        let module = match &mut self.module {
            StateMod::UseLoaded(mod_id) => {
                let mod_ref = self.metta.0.modules.lock().unwrap();
                ModRef::Borrowed(mod_ref.get(mod_id.0).unwrap().clone())
            },
            StateMod::Initializing(_) |
            StateMod::None(_) =>  ModRef::Local(&mut self.module)
        };
        let mut context = RunContext {
            metta: &self.metta,
            i_wrapper: &mut self.i_wrapper,
            module,
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

    /// Internal method to return the MettaMod for a RunnerState that just initialized the module
    pub(crate) fn into_module(self) -> Result<MettaMod, Atom> {

        for result_vec in self.i_wrapper.results {
            for result in result_vec {
                if atom_is_error(&result) {
                    return Err(result)
                }
            }
        }

        let module = match self.module {
            StateMod::Initializing(module) => module,
            _ => panic!("Fatal Error: Module loader function exited without calling RunContext::init_self_module")
        };

        Ok(module)
    }
}

// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*
// RunContext & related objects
// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*

/// Runtime data that is available to Grounded Atom execution
// TODO: I think we may be able to remove the `'interpreter`` lifetime after the minimal MeTTa migration
//  because the lifetime is separated on account of the inability of the compiler to shorten a lifetime
//  used as a generic parameter on a trait.  In this case, the `Plan` trait.
pub struct RunContext<'a, 'interpreter, 'input> {
    metta: &'a Metta,
    module: ModRef<'a>,
    i_wrapper: &'a mut InterpreterWrapper<'interpreter, 'input>
}

impl std::fmt::Debug for RunContext<'_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RunContext")
         .finish()
    }
}

enum ModRef<'a> {
    Local(&'a mut StateMod),
    Borrowed(Rc<MettaMod>)
}

impl ModRef<'_> {
    fn try_borrow(&self) -> Option<&MettaMod> {
        match &self {
            ModRef::Borrowed(module) => Some(&*module),
            ModRef::Local(state_mod) => {
                match state_mod {
                    StateMod::Initializing(module) => Some(module),
                    _ => None
                }
            }
        }
    }
    pub fn try_borrow_mut(&mut self) -> Option<&mut MettaMod> {
        match self {
            ModRef::Borrowed(_) => None,
            ModRef::Local(state_mod) => {
                match state_mod {
                    StateMod::Initializing(module) => Some(module),
                    _ => None
                }
            }
        }
    }
}

impl<'input> RunContext<'_, '_, 'input> {
    /// Returns access to the Metta runner that is hosting the context 
    pub fn metta(&self) -> &Metta {
        &self.metta
    }

    /// Returns access to the context's current module
    pub fn module(&self) -> &MettaMod {
        self.module.try_borrow().unwrap_or_else(|| panic!("No module available"))
    }

    /// Returns mutable access the context's current module, if possible
    pub fn module_mut(&mut self) -> Option<&mut MettaMod> {
        self.module.try_borrow_mut()
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

    /// Locates and retrieves a loaded module based on its name
    pub fn get_module_by_name(&self, mod_name: &str) -> Result<ModId, String> {
        self.module().get_module_by_name(&self.metta, mod_name)
    }

    /// Adds a ModId to the named module tree with the specified name
    pub fn add_module_to_name_tree(&mut self, mod_name: &str, mod_id: ModId) -> Result<(), String>  {
        match self.module.try_borrow_mut() {
            Some(module) => module.add_module_to_name_tree(&self.metta, mod_name, mod_id),
            None => self.metta.add_module_to_name_tree(mod_name, mod_id)
        }
    }

    /// Normalize a module name into a canonical name-path form, and expanding a relative module-path
    pub fn normalize_module_name(&self, mod_name: &str) -> Result<String, String> {
        self.module().normalize_module_name(mod_name)
    }

    /// Initiates the loading of a module from a runner thread.  Useful for loading sub-modules 
    pub fn load_module_direct(&mut self, loader: &dyn ModuleLoader, mod_name: &str) -> Result<ModId, String> {

        //Make sure we don't have a conflicting mod, before attempting to load this one
        if self.get_module_by_name(mod_name).is_ok() {
            return Err(format!("Attempt to load module with name that conflicts with existing module: {mod_name}"));
        }

        let absolute_mod_name = self.normalize_module_name(mod_name)?;
        let mod_id = self.metta.init_module(&absolute_mod_name, |context| loader.load(context))?;

        self.add_module_to_name_tree(&mod_name, mod_id)?;
        Ok(mod_id)
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
        let (loader, descriptor) = match loader_for_module_at_path(&self.metta, &path, absolute_mod_name.as_deref(), self.module().resource_dir())? {
            Some((loader, descriptor)) => (loader, descriptor),
            None => return Err(format!("Failed to resolve module at path: {}", path.as_ref().display()))
        };

        let mod_name = match absolute_mod_name {
            Some(mod_name) => mod_name,
            None => descriptor.name().to_string()
        };

        // Load the module from the loader
        let mod_id = self.metta.get_or_init_module_with_descriptor(&mod_name, descriptor, |context| loader.load(context))?;
        self.add_module_to_name_tree(&mod_name, mod_id)?;
        Ok(mod_id)
    }

    /// A version of [Metta::load_module_alias] Useful for defining sub-module aliases
    pub fn load_module_alias(&mut self, mod_name: &str, mod_id: ModId) -> Result<ModId, String> {

        if self.get_module_by_name(&mod_name).is_ok() {
            return Err(format!("Attempt to create module alias with name that conflicts with existing module: {mod_name}"));
        }
        let mod_name = self.normalize_module_name(mod_name)?;
        self.add_module_to_name_tree(&mod_name, mod_id)?;
        Ok(mod_id)
    }

    /// Initializes the context's module.  Used in the implementation of a module loader function
    ///
    /// Prior to calling this function, any attempt to access the active module in the RunContext will
    /// lead to a panic.
    pub fn init_self_module(&mut self, space: DynSpace, resource_dir: Option<PathBuf>) {
        match &mut self.module {
            ModRef::Borrowed(_) => panic!("Module already initialized"),
            ModRef::Local(ref mut state_mod_ref) => {
                let mod_name = match state_mod_ref {
                    StateMod::None(mod_name) => mod_name.clone().unwrap().to_string(),
                    _ => panic!("Module already initialized"),
                };
                let tokenizer = Shared::new(Tokenizer::new());
                **state_mod_ref = StateMod::Initializing(MettaMod::new_with_tokenizer(self.metta, mod_name, space, tokenizer, resource_dir, false));
            }
        }
    }

    /// Resolves a dependency module from a name, according to the [PkgInfo] of the current module,
    /// and loads it into the runner, if it's not already loaded
    pub fn load_module(&mut self, mod_name: &str) -> Result<ModId, String> {

        // LP-TODO-NOW!, We should assume mod_name is relative by default, not absolute

        // See if we already have the module loaded
        if let Ok(mod_id) = self.get_module_by_name(mod_name) {
            return Ok(mod_id);
        }

        #[cfg(not(feature = "pkg_mgmt"))]
        return Err(format!("Failed to resolve module {mod_name}"));

        // Resolve the module name into a loader object using the resolution logic in the pkg_info
        #[cfg(feature = "pkg_mgmt")]
        self.load_module_recursive(mod_name)
    }

    /// Internal function used for recursive loading of parent modules by [Self::load_module]
    #[cfg(feature = "pkg_mgmt")]
    fn load_module_recursive(&mut self, mod_name: &str) -> Result<ModId, String> {

        //Normalize the path in the context of this running module
        let normalized_mod_path = self.normalize_module_name(mod_name)?;

        //Make sure the parent module is loaded, and descend recursively until we find a loaded parent
        let mod_name_components = ModNameNode::decompose_name_path(&normalized_mod_path)?;
        let parent_mod_id = if mod_name_components.len() > 1 {
            let parent_name = ModNameNode::compose_name_path(&mod_name_components[..mod_name_components.len()-1])?;
            if let Ok(parent_mod_id) = self.get_module_by_name(&parent_name) {
                parent_mod_id
            } else {
                self.load_module_recursive(&parent_name)?
            }
        } else {
            ModId::TOP
        };

        //Perform the loading in the context of the parent module
        let mut state = RunnerState::new_with_module(&self.metta, parent_mod_id);
        state.run_in_context(|context| {
            let new_mod_id = match context.module().pkg_info().resolve_module(context, &normalized_mod_path)? {
                Some((loader, descriptor)) => {
                    self.metta.get_or_init_module_with_descriptor(&normalized_mod_path, descriptor, |context| loader.load(context))?
                },
                None => {return Err(format!("Failed to resolve module {mod_name}"))}
            };
            self.add_module_to_name_tree(&normalized_mod_path, new_mod_id)?;
            Ok(new_mod_id)
        })
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
            let tokenizer_option = self.module.try_borrow().map(|module| module.tokenizer().borrow());
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

                            if self.metta.type_check_is_enabled() && !validate_atom(self.module().space().borrow().as_space(), &atom) {
                                let type_err_exp = Atom::expr([ERROR_SYMBOL, atom, BAD_TYPE_SYMBOL]);
                                self.i_wrapper.interpreter_state = Some(InterpreterState::new_finished(self.module().space().clone(), vec![type_err_exp]));
                            } else {
                                #[cfg(feature = "minimal")]
                                let atom = if is_bare_minimal_interpreter(self.metta) {
                                    atom
                                } else {
                                    wrap_atom_by_metta_interpreter(self.module().space().clone(), atom)
                                };
                                self.i_wrapper.interpreter_state = Some(interpret_init(self.module().space().clone(), &atom));
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

#[cfg(feature = "minimal")]
fn is_bare_minimal_interpreter(metta: &Metta) -> bool {
    metta.get_setting_string("interpreter") == Some("bare-minimal".into())
}

// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*
// InterpreterWrapper & related objects
// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*

/// Private structure to contain everything associated with an InterpreterState.
/// This is basically the part of RunContext that lasts across calls to run_step
#[derive(Default)]
struct InterpreterWrapper<'interpreter, 'i> {
    mode: MettaRunnerMode,
    input_src: InputStream<'i>,
    interpreter_state: Option<InterpreterState<'interpreter, DynSpace>>,
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

#[cfg(feature = "minimal")]
fn wrap_atom_by_metta_interpreter(space: DynSpace, atom: Atom) -> Atom {
    let space = Atom::gnd(space);
    let interpret = Atom::expr([Atom::sym("interpret"), atom, ATOM_TYPE_UNDEFINED, space]);
    let eval = Atom::expr([EVAL_SYMBOL, interpret]);
    eval
}

// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*
// Tests
// *-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*-=-*

#[cfg(test)]
mod tests {
    use super::*;

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
        metta.set_setting("type-check".into(), sym!("auto"));
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") "BadType")]]));
    }

    #[test]
    fn metta_interpret_type_check() {
        let program = "
            (: foo (-> A B))
            (: b B)
            !(foo b)
        ";

        let metta = Metta::new_core(None, Some(EnvBuilder::test_env()));
        metta.set_setting("type-check".into(), sym!("auto"));
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") "BadType")]]));
    }

    #[derive(Clone, PartialEq, Debug)]
    struct ErrorOp{}

    impl std::fmt::Display for ErrorOp {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "error")
        }
    }

    impl Grounded for ErrorOp {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED])
        }
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            // TODO: why next two lines led to not equal results?
            Ok(vec![expr!("Error" ("error") "TestError")])
            //Err("TestError".into())
        }
        fn match_(&self, other: &Atom) -> crate::matcher::MatchResultIter {
            match_by_equality(self, other)
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

        assert_eq!(result, Ok(vec![vec![expr!("Error" ("error") "TestError")]]));
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
        metta.set_setting("type-check".into(), sym!("auto"));
        let result = metta.run(SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") "BadType")]]));
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
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![self.0.clone()])
        }
        fn match_(&self, other: &Atom) -> crate::matcher::MatchResultIter {
            match_by_equality(self, other)
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

}
