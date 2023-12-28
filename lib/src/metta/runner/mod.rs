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
//! including loaded [MettaMod] modules.  A [Metta] has one top-level module, (known as the runner's `&self`) and
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
//! A [MettaMod] represents a loaded module.  A module is fundamentally a [Space] of atoms, although it also
//! contains an associated [Tokenizer] to help with the conversion from text to [Atom]s and a set of sub-modules
//! upon which it depends.  Modules can be implemented in pure MeTTa as well as through extensions in other host
//! languages, namely Rust, C, and Python.
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

use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::space::*;
use super::text::{Tokenizer, Parser, SExprParser};
use super::types::validate_atom;

pub mod modules;
use modules::*;

use std::rc::Rc;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

mod environment;
pub use environment::{Environment, EnvBuilder};

pub mod stdlib;
#[cfg(not(feature = "minimal"))]
use super::interpreter::{interpret, interpret_init, interpret_step, InterpreterState};
#[cfg(not(feature = "minimal"))]
use stdlib::*;

#[cfg(feature = "minimal")]
pub mod stdlib2;
#[cfg(feature = "minimal")]
use super::interpreter2::{interpret, interpret_init, interpret_step, InterpreterState};
#[cfg(feature = "minimal")]
use stdlib2::*;

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
    /// An index, to find a loaded module from a ModuleDescriptor
    module_descriptors: Mutex<HashMap<ModuleDescriptor, ModId>>,
    /// A table of public mods loaded by name
    public_mods: Mutex<HashMap<String, ModId>>,
    /// A clone of the top module's Space, so we don't need to do any locking to access it,
    /// to support the metta.space() public function
    top_mod_space: DynSpace,
    /// A clone of the top module's Tokenizer
    top_mod_tokenizer: Shared<Tokenizer>,
    /// The runner's pragmas, affecting runner-wide behavior
    settings: Shared<HashMap<String, Atom>>,
    /// The runner's Environment
    environment: Arc<Environment>,
    //TODO-HACK: This is a terrible horrible ugly hack that should not be merged.  Delete this field
    // The real context is an interface to the state in a run, and should not live across runs
    // This hack will fail badly if we end up running code from two different modules in parallel
    context: Arc<Mutex<Vec<Arc<&'static mut RunContext<'static, 'static, 'static>>>>>,
}

/// A reference to a [MettaMod] that is loaded into a [Metta] runner
//
//NOTE: I don't love exposing the internals of ModId, but because the C bindings are in a separate crate
// it was a choice between that and using an unnecessary box
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ModId(pub usize);

impl ModId {
    pub const INVALID: ModId = ModId(usize::MAX);
    pub const TOP_MOD: ModId = ModId(0);
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
    /// If the custom stdlib wishes to use the core stdlib, the custom loader may load the core stdlib
    /// using [CoreStdlibLoader]
    ///
    /// NOTE: pass `None` for `loader` to use the core `stdlib`
    /// pass `None` for space to create a new [GroundingSpace]
    /// pass `None` for `env_builder` to use the common environment
    pub fn new_with_stdlib_loader(loader: Option<&dyn ModuleLoader>, space: Option<DynSpace>, env_builder: Option<EnvBuilder>) -> Metta {
        let space = match space {
            Some(space) => space,
            None => DynSpace::new(GroundingSpace::new())
        };

        //Create the raw MeTTa runner
        let metta = Metta::new_core(space, env_builder);

        //Load the stdlib
        let core_loader;
        let stdlib_loader = match loader {
            Some(loader) => loader,
            None => {core_loader = CoreStdlibLoader::default(); &core_loader}
        };
        let stdlib_mod_id = metta.load_module_direct(stdlib_loader, None).expect("Failed to load stdlib");

        //Import the stdlib into &self
        let mut runner_state = RunnerState::new(&metta);
        runner_state.run_in_context(|context| {
            context.module().import_dependency_legacy(&metta, stdlib_mod_id).unwrap();
            Ok(())
        }).expect("Failed to load stdlib");
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

    /// Returns a new core MeTTa interpreter without any stdlib or initialization
    ///
    /// NOTE: If `env_builder` is `None`, the common environment will be used
    /// NOTE: This function does not load any stdlib atoms, nor run the [Environment]'s 'init.metta'
    pub fn new_core(space: DynSpace, env_builder: Option<EnvBuilder>) -> Self {
        let settings = Shared::new(HashMap::new());
        let environment = match env_builder {
            Some(env_builder) => Arc::new(env_builder.build()),
            None => Environment::common_env_arc()
        };
        let top_mod_working_dir = environment.working_dir().map(|path| path.into());
        let top_mod_tokenizer = Shared::new(Tokenizer::new());
        let contents = MettaContents{
            modules: Mutex::new(vec![]),
            module_descriptors: Mutex::new(HashMap::new()),
            public_mods: Mutex::new(HashMap::new()),
            top_mod_space: space.clone(),
            top_mod_tokenizer: top_mod_tokenizer.clone(),
            settings,
            environment,
            context: std::sync::Arc::new(std::sync::Mutex::new(vec![])),
        };
        let metta = Self(Rc::new(contents));

        let top_mod = MettaMod::new_with_tokenizer(&metta, ModuleDescriptor::top(), space, top_mod_tokenizer, top_mod_working_dir);
        assert_eq!(metta.add_module(top_mod), ModId::TOP_MOD);

        metta
    }

    /// Loads a module into a Runner, directly from a [ModuleLoader]
    ///
    /// `private_to` should be the [ModuleDescriptor] of the parent module, if the module is being loaded
    /// as an internal private dependency.  If `private_to` is None, then the module will be available for
    /// any module to find it by name
    pub fn load_module_direct(&self, loader: &dyn ModuleLoader, private_to: Option<&ModuleDescriptor>) -> Result<ModId, String> {

        //Make an appropriate ModuleDescriptor, and load the module
        let descriptor = match private_to {
            Some(parent_desc) => ModuleDescriptor::new_with_uid(loader.name()?, parent_desc.hash()),
            None => ModuleDescriptor::new(loader.name()?)
        };
        self.load_module_internal(loader, descriptor, private_to.is_none())
    }

    /// Loads a module into a runner from a resource at the specified path
    ///
    /// This method will try each [FsModuleFormat] in order until one can sucessfully load the module
    ///
    /// If `mod_name` is [None], the module name will be loaded privately, and won't be accessible for
    /// import by name, elsewhere within the runner
    pub fn load_module_at_path<P: AsRef<Path>>(&self, path: P, mod_name: Option<&str>) -> Result<ModId, String> {

        // Resolve the module name into a loader object using the resolution logic in the bom
        let (loader, descriptor) = match loader_for_module_at_path(self, &path, mod_name, self.environment().working_dir(), mod_name.is_some())? {
            Some((loader, descriptor)) => (loader, descriptor),
            None => return Err(format!("Failed to resolve module at path: {}", path.as_ref().display()))
        };

        // Load the module from the loader
        self.load_module_internal(&*loader, descriptor, mod_name.is_some())
    }

    /// Internal function to load a module into the runner
    //TODO: I think we can infer "public" from the descriptor, but it's not official yet
    fn load_module_internal(&self, loader: &dyn ModuleLoader, descriptor: ModuleDescriptor, public: bool) -> Result<ModId, String> {
        let mod_name = loader.name()?;

        //Make sure we don't already have a conflicting mod, before attempting to load this one
        if public {
            let public_mods = self.0.public_mods.lock().unwrap();
            if public_mods.get(&mod_name).is_some() {
                return Err(format!("Attempt to load public module with name that conflicts with existing module: {mod_name}"));
            }
        }

        let mod_id = self.get_or_init_module(descriptor, |context, descriptor| {
            loader.load(context, descriptor)
        })?;

        //If the `private_to` argument is None, the module should be added to the runner's `public_mods`
        // table, so it can be imported by name
        if public {
            let mut public_mods = self.0.public_mods.lock().unwrap();
            public_mods.insert(mod_name, mod_id);
        }

        Ok(mod_id)
    }

    /// Returns the ModId of a module, or None if it isn't loaded
    pub fn get_module(&self, descriptor: &ModuleDescriptor) -> Option<ModId> {
        let descriptors = self.0.module_descriptors.lock().unwrap();
        descriptors.get(descriptor).cloned()
    }

    /// Returns the ModId of a module, initializing it with the provided function if it isn't already loaded
    ///
    /// The init function will then call `context.init_self_module()` along with any other initialization code
    pub(crate) fn get_or_init_module<F: FnOnce(&mut RunContext, ModuleDescriptor) -> Result<(), String>>(&self, descriptor: ModuleDescriptor, f: F) -> Result<ModId, String> {

        //If we already have the module loaded, then return it
        if let Some(mod_id) = self.get_module(&descriptor) {
            return Ok(mod_id);
        }

        //Create a new RunnerState in order to initialize the new module, and push the init function
        // to run within the new RunnerState.  The init function will then call `context.init_self_module()`
        let mut runner_state = RunnerState::new_internal(&self);
        runner_state.run_in_context(|context| {
            context.push_func(|context| f(context, descriptor));
            Ok(())
        })?;

        //Finish the execution, and add the newly initialized module to the Runner
        while !runner_state.is_complete() {
            runner_state.run_step()?;
        }
        match runner_state.into_module() {
            Ok(module) => Ok(self.add_module(module)),
            Err(err_atom) => Err(atom_error_message(&err_atom).to_owned())
        }
    }

    /// Internal function to add a loaded module to the runner
    pub(crate) fn add_module(&self, module: MettaMod) -> ModId {
        let mut vec_ref = self.0.modules.lock().unwrap();
        let new_id = ModId(vec_ref.len());
        let mut descriptors = self.0.module_descriptors.lock().unwrap();
        if descriptors.insert(module.descriptor().clone(), new_id).is_some() {
            panic!(); //We should never try and add the same module twice.
        }
        vec_ref.push(Rc::new(module));
        new_id
    }

    /// Returns a reference to the Environment used by the runner
    pub fn environment(&self) -> &Environment {
        &self.0.environment
    }

    /// Returns a reference to the Space associated with the runner's top module
    pub fn space(&self) -> &DynSpace {
        &self.0.top_mod_space
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
        let atom = wrap_atom_by_metta_interpreter(self.0.top_mod_space.clone(), atom);
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
    None,
    UseLoaded(ModId),
    Initializing(MettaMod),
}

impl<'m, 'input> RunnerState<'m, 'input> {

    fn new_internal(metta: &'m Metta) -> Self {
        Self {
            metta,
            module: StateMod::None,
            i_wrapper: InterpreterWrapper::default()
        }
    }

    /// Returns a new RunnerState to execute code in the context of a MeTTa runner's top module
    pub fn new(metta: &'m Metta) -> Self {
        Self::new_with_module(metta, ModId::TOP_MOD)
    }

    /// Returns a new RunnerState to execute code in the context of any loaded module
    pub(crate) fn new_with_module(metta: &'m Metta, mod_id: ModId) -> Self {
        let mut state = Self::new_internal(metta);
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
            StateMod::None =>  ModRef::Local(&mut self.module)
        };
        let mut context = RunContext {
            metta: &self.metta,
            i_wrapper: &mut self.i_wrapper,
            module,
        };

        //TODO-HACK: This is a terrible horrible ugly hack that should not be merged
        //Push the RunContext so the MeTTa Ops can access it.  The context ought to be passed as an argument
        // to the execute functions, in the absence of the hack
        self.metta.0.context.lock().unwrap().push(Arc::new( unsafe{ std::mem::transmute(&mut context) } ));
        //END HORRIBLE HACK

        // Call our function
        let result = f(&mut context);

        //TODO-HACK: This is a terrible horrible ugly hack that should not be merged
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

        match self.module {
            StateMod::Initializing(module) => Ok(module),
            _ => panic!("Fatal Error: Module loader function exited without calling RunContext::init_self_module")
        }
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

    /// Initializes the context's module.  Used in the implementation of a module loader function
    ///
    /// Prior to calling this function, any attempt to access the active module in the RunContext will
    /// lead to a panic.
    pub fn init_self_module(&mut self, descriptor: ModuleDescriptor, space: DynSpace, working_dir: Option<PathBuf>) {
        match &mut self.module {
            ModRef::Borrowed(_) => panic!("Module already initialized"),
            ModRef::Local(ref mut state_mod_ref) => {
                if matches!(state_mod_ref, StateMod::Initializing(_)) {
                    panic!("Module already initialized");
                }
                let tokenizer = Shared::new(Tokenizer::new());
                **state_mod_ref = StateMod::Initializing(MettaMod::new_with_tokenizer(self.metta, descriptor, space, tokenizer, working_dir));
            }
        }
    }

    /// Resolves a dependency module from a name, according to the [ModuleBom] of the current module,
    /// and loads it into the runner, if it's not already loaded
    pub fn load_module(&self, name: &str) -> Result<ModId, String> {

        // If the module is already loaded into the `public_mods` table, that takes precedence
        {
            let public_mods = self.metta.0.public_mods.lock().unwrap();
            if let Some(mod_id) = public_mods.get(name) {
                return Ok(*mod_id);
            }
        }

        // Resolve the module name into a loader object using the resolution logic in the bom
        let (loader, descriptor) = match self.module().bom().resolve_module(self, name)? {
            Some((loader, descriptor)) => (loader, descriptor),
            None => return Err(format!("Failed to resolve module {name}"))
        };

        // Load the module from the loader
        self.metta.get_or_init_module(descriptor, |context, descriptor| {
            loader.load(context, descriptor)
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
                                let atom = wrap_atom_by_metta_interpreter(self.module().space().clone(), atom);
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

        let metta = Metta::new_core(DynSpace::new(GroundingSpace::new()), Some(EnvBuilder::test_env()));
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

        let metta = Metta::new_core(DynSpace::new(GroundingSpace::new()), Some(EnvBuilder::test_env()));
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

        let metta = Metta::new_core(DynSpace::new(GroundingSpace::new()), Some(EnvBuilder::test_env()));
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
