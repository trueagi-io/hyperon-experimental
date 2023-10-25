use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::space::*;
use super::text::{Tokenizer, SExprParser};
use super::types::validate_atom;

use std::rc::Rc;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::sync::Arc;

mod environment;
pub use environment::{Environment, EnvBuilder};

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

#[derive(Clone, Debug)]
pub struct Metta(Rc<MettaContents>);

impl PartialEq for Metta {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

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
    metta: &'a Metta,
    parser: Option<SExprParser<'a>>,
    atoms: Option<&'a [Atom]>,
    interpreter_state: Option<InterpreterState<'a, DynSpace>>,
    results: Vec<Vec<Atom>>,
}

impl std::fmt::Debug for RunnerState<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RunnerState")
            .field("mode", &self.mode)
            .field("interpreter_state", &self.interpreter_state)
            .finish()
    }
}

impl Metta {

    /// A 1-line method to create a fully initialized MeTTa interpreter
    ///
    /// NOTE: pass `None` for `env_builder` to use the common environment
    pub fn new(env_builder: Option<EnvBuilder>) -> Metta {
        Self::new_with_stdlib_loader(|_| {}, None, env_builder)
    }

    /// Create and initialize a MeTTa interpreter with a language-specific stdlib
    ///
    /// NOTE: pass `None` for space to create a new [GroundingSpace]
    /// pass `None` for `env_builder` to use the common environment
    pub fn new_with_stdlib_loader<F>(loader: F, space: Option<DynSpace>, env_builder: Option<EnvBuilder>) -> Metta
        where F: FnOnce(&Self)
    {
        let space = match space {
            Some(space) => space,
            None => DynSpace::new(GroundingSpace::new())
        };

        //Create the raw MeTTa runner
        let metta = Metta::new_core(space, Shared::new(Tokenizer::new()), env_builder);

        // TODO: Reverse the loading order between the Rust stdlib and user-supplied stdlib loader,
        // because user-supplied stdlibs might need to build on top of the Rust stdlib.
        // Currently this is problematic because https://github.com/trueagi-io/hyperon-experimental/issues/408,
        // and the right fix is value-bridging (https://github.com/trueagi-io/hyperon-experimental/issues/351)

        //Load the custom stdlib
        loader(&metta);

        //Load the Rust stdlib
        metta.load_module(PathBuf::from("stdlib")).expect("Could not load stdlib");

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
    pub fn new_core(space: DynSpace, tokenizer: Shared<Tokenizer>, env_builder: Option<EnvBuilder>) -> Self {
        let settings = Shared::new(HashMap::new());
        let modules = Shared::new(HashMap::new());
        let environment = match env_builder {
            Some(env_builder) => Arc::new(env_builder.build()),
            None => Environment::common_env_arc()
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
                // TODO: This is a hack. We need a way to register tokens at module-load-time, for any module
                if path.to_str().unwrap() == "stdlib" {
                    register_rust_tokens(self);
                }

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
                runner.run(SExprParser::new(program.as_str()))
                    .map_err(|err| format!("Cannot import module, path: {}, error: {}", path.display(), err))?;

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

    pub fn run<'p, 'a: 'p>(&'a self, parser: SExprParser<'p>) -> Result<Vec<Vec<Atom>>, String> {
        let state = RunnerState::new_with_parser(self, parser);
        state.run_to_completion()
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
    fn new(metta: &'a Metta) -> Self {
        Self {
            metta,
            mode: MettaRunnerMode::ADD,
            interpreter_state: None,
            parser: None,
            atoms: None,
            results: vec![],
        }
    }
    /// Returns a new RunnerState, for running code from the [SExprParser] with the specified [Metta] runner
    pub fn new_with_parser(metta: &'a Metta, parser: SExprParser<'a>) -> Self {
        let mut state = Self::new(metta);
        state.parser = Some(parser);
        state
    }

    /// Returns a new RunnerState, for running code encoded as a slice of [Atom]s with the specified [Metta] runner
    pub fn new_with_atoms(metta: &'a Metta, atoms: &'a[Atom]) -> Self {
        let mut state = Self::new(metta);
        state.atoms = Some(atoms);
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

        // If we're in the middle of interpreting an atom...
        if let Some(interpreter_state) = core::mem::take(&mut self.interpreter_state) {

            if interpreter_state.has_next() {

                //Take a step with the interpreter, and put it back for next time
                self.interpreter_state = Some(interpret_step(interpreter_state))
            } else {

                //This interpreter is finished, process the results
                match interpreter_state.into_result() {
                    Err(msg) => return Err(msg),
                    Ok(result) => {
                        let error = result.iter().any(|atom| atom_is_error(atom));
                        self.results.push(result);
                        if error {
                            self.mode = MettaRunnerMode::TERMINATE;
                            return Ok(());
                        }
                    }
                }
            }

        } else {

            // Get the next atom, and start a new intperpreter
            let next_atom = if let Some(parser) = self.parser.as_mut() {
                parser.parse(&self.metta.0.tokenizer.borrow())?
            } else {
                if let Some(atoms) = self.atoms.as_mut() {
                    if let Some((atom, rest)) = atoms.split_first() {
                        *atoms = rest;
                        Some(atom.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            };

            if let Some(atom) = next_atom {
                if atom == EXEC_SYMBOL {
                    self.mode = MettaRunnerMode::INTERPRET;
                    return Ok(());
                }
                match self.mode {
                    MettaRunnerMode::ADD => {
                        if let Err(atom) = self.metta.add_atom(atom) {
                            self.results.push(vec![atom]);
                            self.mode = MettaRunnerMode::TERMINATE;
                            return Ok(());
                        }
                    },
                    MettaRunnerMode::INTERPRET => {

                        self.interpreter_state = Some(match self.metta.type_check(atom) {
                            Err(atom) => {
                                InterpreterState::new_finished(self.metta.space().clone(), vec![atom])
                            },
                            Ok(atom) => {
                                #[cfg(feature = "minimal")]
                                let atom = wrap_atom_by_metta_interpreter(&self.metta, atom);
                                interpret_init(self.metta.space().clone(), &atom)
                            },
                        });
                    },
                    MettaRunnerMode::TERMINATE => {
                        return Ok(());
                    },
                }
                self.mode = MettaRunnerMode::ADD;
            }  else {
                self.mode = MettaRunnerMode::TERMINATE;
            }
        }

        Ok(())
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

        let metta = Metta::new_core(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()), Some(EnvBuilder::test_env()));
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

        let metta = Metta::new_core(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()), Some(EnvBuilder::test_env()));
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
        metta.tokenizer().borrow_mut().register_token(Regex::new("error").unwrap(),
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

        let metta = Metta::new_core(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()), Some(EnvBuilder::test_env()));
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
        metta.tokenizer().borrow_mut().register_token(Regex::new("empty").unwrap(),
            |_| Atom::gnd(ReturnAtomOp(expr!())));
        let result = metta.run(SExprParser::new(program));

        assert_eq!(result, Ok(vec![vec![expr!()]]));
    }

}
