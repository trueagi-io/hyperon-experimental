use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::environment::EnvBuilder;
use super::space::*;
use super::text::{Tokenizer, SExprParser};
use super::types::validate_atom;

use std::rc::Rc;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::Arc;

use metta::environment::Environment;

#[cfg(not(feature = "minimal"))]
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

mod arithmetics;

const EXEC_SYMBOL : Atom = sym!("!");

pub fn atom_is_error(atom: &Atom) -> bool {
    match atom {
        Atom::Expression(expr) => {
            expr.children().len() > 0 && expr.children()[0] == ERROR_SYMBOL
        },
        _ => false,
    }
}

#[derive(Clone, Debug)]
pub struct Metta(Rc<MettaContents>);

#[derive(Debug)]
pub struct MettaContents {
    space: DynSpace,
    tokenizer: Shared<Tokenizer>,
    settings: Shared<HashMap<String, Atom>>,
    modules: Shared<HashMap<PathBuf, DynSpace>>,
    working_dir: Option<PathBuf>,
    environment: Arc<Environment>,
}

#[derive(Debug, PartialEq, Eq)]
enum MettaRunnerMode {
    ADD,
    INTERPRET,
    TERMINATE,
}

pub struct RunnerState<'a> {
    mode: MettaRunnerMode,
    interpreter_state: Option<InterpreterState<'a, DynSpace>>,
    results: Vec<Vec<Atom>>,
}

impl Metta {

    /// A 1-line method to create a fully initialized MeTTa interpreter
    ///
    /// NOTE: pass `None` for `env_builder` to use the platform environment
    /// NOTE: This function is appropriate for Rust or C clients, but if other language-specific
    ///   stdlibs are involved then see the documentation for [Metta::init]
    pub fn new_rust(env_builder: Option<EnvBuilder>) -> Metta {
        let metta = Metta::new_with_space(DynSpace::new(GroundingSpace::new()),
            Shared::new(Tokenizer::new()), env_builder);
        metta.load_module(PathBuf::from("stdlib")).expect("Could not load stdlib");
        metta.init();
        metta
    }

    /// Performs initialization of a MeTTa interpreter.  Presently this involves running the `init.metta`
    /// file from the associated environment.
    ///
    /// DISCUSSION: Creating a fully-initialized MeTTa runner should usually be done with with
    /// a top-level initialization function, such as [Metta::new_rust] or `MeTTa()` in Python.
    ///
    /// Doing it manually involves several steps:
    /// 1. Create the MeTTa runner, using [new_with_space].  This provides a working interpreter, but
    ///     doesn't load any stdlibs
    /// 2. Load each language-specific stdlib (Currently only Python has an extended stdlib)
    /// 3. Load the Rust `stdlib` (TODO: Conceptually I'd like to load Rust's stdlib first, so other
    ///      stdlibs can utilize the Rust stdlib's atoms, but that depends on value bridging)
    /// 4. Run the `init.metta` file by calling this function
    ///
    /// TODO: When we are able to load the Rust stdlib before the Python stdlib, which requires value-bridging,
    ///     we can refactor this function to load the appropriate stdlib(s) and simplify the init process
    pub fn init(&self) {
        if let Some(init_meta_file) = self.0.environment.initialization_metta_file_path() {
            self.load_module(init_meta_file.into()).unwrap();
        }
    }

    /// Returns a new MeTTa interpreter, using the provided Space, Tokenizer
    ///
    /// NOTE: If `env_builder` is `None`, the platform environment will be used
    /// NOTE: This function does not load any stdlib atoms, nor run the [Environment]'s 'init.metta'
    pub fn new_with_space(space: DynSpace, tokenizer: Shared<Tokenizer>, env_builder: Option<EnvBuilder>) -> Self {
        let settings = Shared::new(HashMap::new());
        let modules = Shared::new(HashMap::new());
        let environment = match env_builder {
            Some(env_builder) => Arc::new(env_builder.build()),
            None => Environment::platform_env_arc()
        };
        let contents = MettaContents{
            space,
            tokenizer,
            settings,
            modules,
            working_dir: environment.working_dir().map(|path| path.into()),
            environment,
        };
        let metta = Self(Rc::new(contents));
        register_runner_tokens(&metta);
        register_common_tokens(&metta);
        metta
    }

    /// Returns a new MeTTa interpreter intended for use loading MeTTa modules during import
    fn new_loading_runner(metta: &Metta, path: &Path) -> Self {
        let space = DynSpace::new(GroundingSpace::new());
        let tokenizer = metta.tokenizer().clone_inner();
        let environment = metta.0.environment.clone();
        let settings = metta.0.settings.clone();
        let modules = metta.0.modules.clone();

        //Start search for sub-modules in the parent directory of the module we're loading
        let working_dir = path.parent().map(|path| path.into());

        let metta = Self(Rc::new(MettaContents { space, tokenizer, settings, modules, environment, working_dir }));
        register_runner_tokens(&metta);
        metta
    }

    pub fn load_module_space(&self, path: PathBuf) -> Result<DynSpace, String> {
        log::debug!("Metta::load_module_space: load module space {}", path.display());
        let loaded_module = self.0.modules.borrow().get(&path).cloned();

        // Loading the module only once
        // TODO: force_reload?
        match loaded_module {
            Some(module_space) => {
                log::debug!("Metta::load_module_space: module is already loaded {}", path.display());
                Ok(module_space)
            },
            None => {
                // Load the module to the new space
                let runner = Metta::new_loading_runner(self, &path);
                let program = match path.to_str() {
                    Some("stdlib") => METTA_CODE.to_string(),
                    _ => std::fs::read_to_string(&path).map_err(
                        |err| format!("Could not read file, path: {}, error: {}", path.display(), err))?,
                };
                // Make the imported module be immediately available to itself
                // to mitigate circular imports
                self.0.modules.borrow_mut().insert(path.clone(), runner.space().clone());
                runner.run(&mut SExprParser::new(program.as_str()))
                    .map_err(|err| format!("Cannot import module, path: {}, error: {}", path.display(), err))?;

                // TODO: This is a hack. We need a way to register tokens at module-load-time, for any module
                if path.to_str().unwrap() == "stdlib" {
                    register_rust_tokens(self);
                }

                Ok(runner.space().clone())
            }
        }
    }

    pub fn load_module(&self, path: PathBuf) -> Result<(), String> {
        // Load module to &self
        // TODO: Should we register the module name?
        // self.tokenizer.borrow_mut().register_token(stdlib::regex(name), move |_| { space_atom.clone() });
        // TODO: check if it is already there (if the module is newly loaded)
        let module_space = self.load_module_space(path)?;
        let space_atom = Atom::gnd(module_space);
        self.0.space.borrow_mut().add(space_atom);
        Ok(())
    }

    pub fn space(&self) -> &DynSpace {
        &self.0.space
    }

    pub fn tokenizer(&self) -> &Shared<Tokenizer> {
        &self.0.tokenizer
    }

    /// Returns the search paths to explore for MeTTa modules, in search priority order
    ///
    /// The runner's working_dir is always returned first
    pub fn search_paths<'a>(&'a self) -> impl Iterator<Item=&Path> + 'a {
        [&self.0.working_dir].into_iter().filter_map(|opt| opt.as_deref())
            .chain(self.0.environment.extra_include_paths())
    }

    pub fn modules(&self) -> &Shared<HashMap<PathBuf, DynSpace>> {
        &self.0.modules
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

    pub fn run(&self, parser: &mut SExprParser) -> Result<Vec<Vec<Atom>>, String> {
        let mut state = self.start_run();

        while !state.is_complete() {
            self.run_step(parser, &mut state)?;
        }
        Ok(state.into_results())
    }

    pub fn start_run(&self) -> RunnerState {
        RunnerState::new()
    }

    pub fn run_step(&self, parser: &mut SExprParser, state: &mut RunnerState) -> Result<(), String> {

        // If we're in the middle of interpreting an atom...
        if let Some(interpreter_state) = core::mem::replace(&mut state.interpreter_state, None) {

            if interpreter_state.has_next() {

                //Take a step with the interpreter, and put it back for next time
                state.interpreter_state = Some(interpret_step(interpreter_state))
            } else {

                //This interpreter is finished, process the results
                match interpreter_state.into_result() {
                    Err(msg) => return Err(msg),
                    Ok(result) => {
                        let error = result.iter().any(|atom| atom_is_error(atom));
                        state.results.push(result);
                        if error {
                            state.mode = MettaRunnerMode::TERMINATE;
                            return Ok(());
                        }
                    }
                }
            }

        } else {

            // We'll parse the next atom, and start a new intperpreter
            if let Some(atom) = parser.parse(&self.0.tokenizer.borrow())? {
                if atom == EXEC_SYMBOL {
                    state.mode = MettaRunnerMode::INTERPRET;
                    return Ok(());
                }
                match state.mode {
                    MettaRunnerMode::ADD => {
                        if let Err(atom) = self.add_atom(atom) {
                            state.results.push(vec![atom]);
                            state.mode = MettaRunnerMode::TERMINATE;
                            return Ok(());
                        }
                    },
                    MettaRunnerMode::INTERPRET => {

                        state.interpreter_state = Some(match self.type_check(atom) {
                            Err(atom) => {
                                InterpreterState::new_finished(self.space().clone(), vec![atom])
                            },
                            Ok(atom) => {
                                #[cfg(feature = "minimal")]
                                let atom = wrap_atom_by_metta_interpreter(self, atom);
                                interpret_init(self.space().clone(), &atom)
                            },
                        });
                    },
                    MettaRunnerMode::TERMINATE => {
                        return Ok(());
                    },
                }
                state.mode = MettaRunnerMode::ADD;
            }  else {
                state.mode = MettaRunnerMode::TERMINATE;
            }
        }

        Ok(())
    }

    // TODO: this method is deprecated and should be removed after switching
    // to the minimal MeTTa
    pub fn evaluate_atom(&self, atom: Atom) -> Result<Vec<Atom>, String> {
        #[cfg(feature = "minimal")]
        let atom = wrap_atom_by_metta_interpreter(self, atom);
        match self.type_check(atom) {
            Err(atom) => Ok(vec![atom]),
            Ok(atom) => interpret(self.space(), &atom),
        }
    }

    fn add_atom(&self, atom: Atom) -> Result<(), Atom>{
        let atom = self.type_check(atom)?;
        self.0.space.borrow_mut().add(atom);
        Ok(())
    }

    fn type_check(&self, atom: Atom) -> Result<Atom, Atom> {
        let is_type_check_enabled = self.get_setting_string("type-check").map_or(false, |val| val == "auto");
        if  is_type_check_enabled && !validate_atom(self.0.space.borrow().as_space(), &atom) {
            Err(Atom::expr([ERROR_SYMBOL, atom, BAD_TYPE_SYMBOL]))
        } else {
            Ok(atom)
        }
    }

}

#[cfg(feature = "minimal")]
fn wrap_atom_by_metta_interpreter(runner: &Metta, atom: Atom) -> Atom {
    let space = Atom::gnd(runner.space().clone());
    let interpret = Atom::expr([Atom::sym("interpret"), atom, ATOM_TYPE_UNDEFINED, space]);
    let eval = Atom::expr([EVAL_SYMBOL, interpret]);
    eval
}

impl<'a> RunnerState<'a> {
    fn new() -> Self {
        Self {
            mode: MettaRunnerMode::ADD,
            interpreter_state: None,
            results: vec![],
        }
    }
    pub fn is_complete(&self) -> bool {
        self.mode == MettaRunnerMode::TERMINATE
    }
    /// Returns a reference to the current in-progress results within the RunnerState
    pub fn current_results(&self) -> &Vec<Vec<Atom>> {
        &self.results
    }
    /// Consumes the RunnerState and returns the final results
    pub fn into_results(self) -> Vec<Vec<Atom>> {
        self.results
    }
}

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

        let metta = Metta::new_rust(Some(EnvBuilder::test_env()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![Atom::sym("T")]]));
    }

    #[test]
    fn metta_add_type_check() {
        let program = "
            (: foo (-> A B))
            (: b B)
            (foo b)
        ";

        let metta = Metta::new_with_space(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()), Some(EnvBuilder::test_env()));
        metta.set_setting("type-check".into(), sym!("auto"));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") "BadType")]]));
    }

    #[test]
    fn metta_interpret_type_check() {
        let program = "
            (: foo (-> A B))
            (: b B)
            !(foo b)
        ";

        let metta = Metta::new_with_space(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()), Some(EnvBuilder::test_env()));
        metta.set_setting("type-check".into(), sym!("auto"));
        let result = metta.run(&mut SExprParser::new(program));
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

        let metta = Metta::new_rust(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("error").unwrap(),
            |_| Atom::gnd(ErrorOp{}));
        let result = metta.run(&mut SExprParser::new(program));

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

        let metta = Metta::new_with_space(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()), Some(EnvBuilder::test_env()));
        metta.set_setting("type-check".into(), sym!("auto"));
        let result = metta.run(&mut SExprParser::new(program));
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

        let metta = Metta::new_rust(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("empty").unwrap(),
            |_| Atom::gnd(ReturnAtomOp(expr!())));
        let result = metta.run(&mut SExprParser::new(program));

        assert_eq!(result, Ok(vec![vec![expr!()]]));
    }

}
