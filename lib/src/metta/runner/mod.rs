use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::space::*;
use super::text::{Tokenizer, SExprParser};
use super::types::validate_atom;

use std::rc::Rc;
use std::path::PathBuf;
use std::collections::HashMap;


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

#[derive(Clone, Debug, PartialEq)]
pub struct Metta(Rc<MettaContents>);

#[derive(Debug, PartialEq)]
pub struct MettaContents {
    space: DynSpace,
    tokenizer: Shared<Tokenizer>,
    settings: Shared<HashMap<String, String>>,
    modules: Shared<HashMap<PathBuf, DynSpace>>,
    search_paths: Vec<PathBuf>,
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
    pub fn new(space: DynSpace, tokenizer: Shared<Tokenizer>) -> Self {
        Metta::from_space(space, tokenizer, vec![PathBuf::from(".")])
    }

    pub fn from_space(space: DynSpace, tokenizer: Shared<Tokenizer>, search_paths: Vec<PathBuf>) -> Self {
        let settings = Shared::new(HashMap::new());
        let modules = Shared::new(HashMap::new());
        let contents = MettaContents{ space, tokenizer, settings, modules, search_paths };
        let metta = Self(Rc::new(contents));
        register_runner_tokens(&metta);
        register_common_tokens(&metta);
        metta
    }

    fn new_loading_runner(metta: &Metta, path: PathBuf) -> Self {
        let space = DynSpace::new(GroundingSpace::new());
        let tokenizer = metta.tokenizer().clone_inner();
        let settings = metta.0.settings.clone();
        let modules = metta.0.modules.clone();

        //Search only the parent directory of the module we're loading
        let mut path = path;
        path.pop();
        let search_paths = vec![path];

        let metta = Self(Rc::new(MettaContents { space, tokenizer, settings, modules, search_paths }));
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
                let runner = Metta::new_loading_runner(self, path.clone());
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
        self.0.space.borrow_mut().add(space_atom); // self.add_atom(space_atom)
        Ok(())
    }

    pub fn space(&self) -> &DynSpace {
        &self.0.space
    }

    pub fn tokenizer(&self) -> &Shared<Tokenizer> {
        &self.0.tokenizer
    }

    pub fn search_paths(&self) -> &Vec<PathBuf> {
        &self.0.search_paths
    }

    pub fn modules(&self) -> &Shared<HashMap<PathBuf, DynSpace>> {
        &self.0.modules
    }

    pub(crate) fn settings(&self) -> &Shared<HashMap<String, String>> {
        &self.0.settings
    }

    #[cfg(test)]
    fn set_setting(&self, key: String, value: String) {
        self.0.settings.borrow_mut().insert(key, value);
    }

    fn get_setting(&self, key: &str) -> Option<String> {
        self.0.settings.borrow().get(key.into()).cloned()
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
                        fn is_error(atom: &Atom) -> bool {
                            match atom {
                                Atom::Expression(expr) => {
                                    expr.children().len() > 0 && expr.children()[0] == ERROR_SYMBOL
                                },
                                _ => false,
                            }
                        }

                        let error = result.iter().any(|atom| is_error(atom));
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
        let is_type_check_enabled = self.get_setting("type-check").map_or(false, |val| val == "auto");
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
    pub fn intermediate_results(&self) -> &Vec<Vec<Atom>> {
        &self.results
    }
    pub fn into_results(self) -> Vec<Vec<Atom>> {
        self.results
    }
}

pub fn new_metta_rust() -> Metta {
    let metta = Metta::new(DynSpace::new(GroundingSpace::new()),
        Shared::new(Tokenizer::new()));
    register_rust_tokens(&metta);
    metta.load_module(PathBuf::from("stdlib")).expect("Could not load stdlib");
    metta
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

        let metta = new_metta_rust();
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

        let metta = Metta::new(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()));
        metta.set_setting("type-check".into(), "auto".into());
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

        let metta = Metta::new(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()));
        metta.set_setting("type-check".into(), "auto".into());
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

        let metta = new_metta_rust();
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

        let metta = Metta::new(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()));
        metta.set_setting("type-check".into(), "auto".into());
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

        let metta = new_metta_rust();
        metta.tokenizer().borrow_mut().register_token(Regex::new("empty").unwrap(),
            |_| Atom::gnd(ReturnAtomOp(expr!())));
        let result = metta.run(&mut SExprParser::new(program));

        assert_eq!(result, Ok(vec![vec![expr!()]]));
    }

}
