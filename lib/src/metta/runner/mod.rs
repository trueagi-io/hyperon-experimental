use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::space::*;
use super::text::{Tokenizer, SExprParser};
use super::types::validate_atom;

use std::path::PathBuf;
use std::collections::HashMap;

pub mod stdlib;
use super::interpreter::interpret;
use stdlib::*;

// Uncomment three lines below and comment three lines above to
// switch to the minimal MeTTa version
//pub mod stdlib2;
//use super::interpreter2::interpret;
//use stdlib2::*;

mod arithmetics;

const EXEC_SYMBOL : Atom = sym!("!");

#[derive(Debug, Clone)]
pub struct Metta {
    space: DynSpace,
    tokenizer: Shared<Tokenizer>,
    settings: Shared<HashMap<String, String>>,
    modules: Shared<HashMap<PathBuf, DynSpace>>,
}

enum Mode {
    ADD,
    INTERPRET,
}

impl Metta {
    pub fn new(space: DynSpace, tokenizer: Shared<Tokenizer>) -> Self {
        Metta::from_space_cwd(space, tokenizer, PathBuf::from("."))
    }

    pub fn from_space_cwd(space: DynSpace, tokenizer: Shared<Tokenizer>, cwd: PathBuf) -> Self {
        let settings = Shared::new(HashMap::new());
        let modules = Shared::new(HashMap::new());
        let metta = Self{ space, tokenizer, settings, modules };
        register_runner_tokens(&metta, cwd);
        register_common_tokens(&metta);
        metta
    }

    fn new_loading_runner(metta: &Metta, path: PathBuf) -> Self {
        let space = DynSpace::new(GroundingSpace::new());
        let tokenizer = metta.tokenizer.cloned();
        let mut next_cwd = path;
        next_cwd.pop();
        let settings = metta.settings.clone();
        let modules = metta.modules.clone();
        let metta = Metta{ space, tokenizer, settings, modules };
        register_runner_tokens(&metta, next_cwd);
        metta
    }

    pub fn load_module_space(&self, path: PathBuf) -> Result<DynSpace, String> {
        log::debug!("Metta::load_module_space: load module space {}", path.display());
        let loaded_module = self.modules.borrow().get(&path).cloned();

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
                self.modules.borrow_mut().insert(path.clone(), runner.space());
                runner.run(&mut SExprParser::new(program.as_str()))
                    .map_err(|err| format!("Cannot import module, path: {}, error: {}", path.display(), err))?;
                Ok(runner.space())
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
        self.space.borrow_mut().add(space_atom); // self.add_atom(space_atom)
        Ok(())
    }

    pub fn space(&self) -> DynSpace {
        self.space.clone()
    }

    pub fn tokenizer(&self) -> Shared<Tokenizer> {
        self.tokenizer.clone()
    }

    #[cfg(test)]
    fn set_setting(&self, key: String, value: String) {
        self.settings.borrow_mut().insert(key, value);
    }

    fn get_setting(&self, key: &str) -> Option<String> {
        self.settings.borrow().get(key.into()).cloned()
    }

    pub fn run(&self, parser: &mut SExprParser) -> Result<Vec<Vec<Atom>>, String> {
        let mut mode = Mode::ADD;
        let mut results: Vec<Vec<Atom>> = Vec::new();

        loop {
            let atom = parser.parse(&self.tokenizer.borrow())?;

            if let Some(atom) = atom {
                if atom == EXEC_SYMBOL {
                    mode = Mode::INTERPRET;
                    continue;
                }
                match mode {
                    Mode::ADD => {
                        if let Err(atom) = self.add_atom(atom) {
                            results.push(vec![atom]);
                            break;
                        }
                    },
                    Mode::INTERPRET => {
                        match self.evaluate_atom(atom) {
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
                                results.push(result);
                                if error {
                                    break;
                                }
                            }
                        }
                    },
                }
                mode = Mode::ADD;
            } else {
                break;
            }
        }
        Ok(results)
    }



    pub fn evaluate_atom(&self, atom: Atom) -> Result<Vec<Atom>, String> {
        match self.type_check(atom) {
            Err(atom) => Ok(vec![atom]),
            Ok(atom) => interpret(self.space.clone(), &atom),
        }
    }

    fn add_atom(&self, atom: Atom) -> Result<(), Atom>{
        let atom = self.type_check(atom)?;
        self.space.borrow_mut().add(atom);
        Ok(())
    }

    fn type_check(&self, atom: Atom) -> Result<Atom, Atom> {
        let is_type_check_enabled = self.get_setting("type-check").map_or(false, |val| val == "auto");
        if  is_type_check_enabled && !validate_atom(self.space.borrow().as_space(), &atom) {
            Err(Atom::expr([ERROR_SYMBOL, atom, BAD_TYPE_SYMBOL]))
        } else {
            Ok(atom)
        }
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

        let metta = Metta::new(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()));
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

        let metta = Metta::new(DynSpace::new(GroundingSpace::new()), Shared::new(Tokenizer::new()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("empty").unwrap(),
            |_| Atom::gnd(ReturnAtomOp(expr!())));
        let result = metta.run(&mut SExprParser::new(program));

        assert_eq!(result, Ok(vec![vec![expr!()]]));
    }

}
