use crate::*;
use crate::common::shared::Shared;

use super::space::grounding::GroundingSpace;
use super::text::{Tokenizer, SExprParser};
use super::interpreter::interpret;
use super::ATOM_TYPE_UNDEFINED;

use regex::Regex;
use std::path::PathBuf;

const EXEC_SYMBOL : Atom = sym!("!");

pub struct Metta {
    space: Shared<GroundingSpace>,
    tokenizer: Shared<Tokenizer>,
}

impl Metta {
    pub fn new(space: Shared<GroundingSpace>) -> Self {
        Metta::from_space_cwd(space, PathBuf::from("."))
    }

    pub fn from_space_cwd(space: Shared<GroundingSpace>, cwd: PathBuf) -> Self {
        let tokenizer = Shared::new(Tokenizer::new());
        let tokenizer_closure = tokenizer.clone();
        let space_closure = space.clone();
        tokenizer.borrow_mut().register_token(Regex::new(r"import!").unwrap(),
            move |_| { Atom::gnd(ImportOp::new(cwd.clone(), space_closure.clone(), tokenizer_closure.clone())) });
        Self{ space, tokenizer }
    }

    pub fn space(&self) -> Shared<GroundingSpace> {
        self.space.clone()
    }

    pub fn tokenizer(&self) -> Shared<Tokenizer> {
        self.tokenizer.clone()
    }

    pub fn run(&self, parser: &mut SExprParser) -> Result<Vec<Vec<Atom>>, String> {
        enum Mode {
            ADD,
            EXEC,
        }
        let mut mode = Mode::ADD;
        let mut results: Vec<Vec<Atom>> = Vec::new();

        loop {
            let atom = parser.parse(&self.tokenizer.borrow());
            match atom {
                Some(atom) => {
                    if atom == EXEC_SYMBOL {
                        mode = Mode::EXEC;
                    } else {
                        match mode {
                            Mode::ADD => {
                                log::trace!("Metta::run: adding atom: {}", atom);
                                self.space.borrow_mut().add(atom)
                            },
                            Mode::EXEC => {
                                log::trace!("Metta::run: executing atom: {}", atom);
                                let result = interpret(self.space.clone(), &atom);
                                log::trace!("Metta::run: execution result {:?}", result);
                                match result {
                                    Ok(result) => results.push(result),
                                    Err(message) => return Err(format!("Error: {}", message)),
                                }
                            },
                        }
                        mode = Mode::ADD;
                    }
                },
                None => break,
            }
        }
        Ok(results)
    }
}

use crate::matcher::MatchResultIter;
use std::fmt::Display;

#[derive(Clone)]
struct ImportOp {
    cwd: PathBuf,
    space: Shared<GroundingSpace>,
    tokenizer: Shared<Tokenizer>,
}

impl ImportOp {
    fn new(cwd: PathBuf, space: Shared<GroundingSpace>, tokenizer: Shared<Tokenizer>) -> Self {
        Self{ cwd, space, tokenizer }
    }
}

impl std::fmt::Debug for ImportOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ImportOp{{ cwd={:?} }}", self.cwd)
    }
}

impl PartialEq for ImportOp {
    fn eq(&self, other: &Self) -> bool {
        self.cwd == other.cwd
            && self.space == other.space
            && self.tokenizer == other.tokenizer
    }
}

fn remove_quotes(text: &str) -> String {
    let mut text = String::from(text).split_off(1);
    text.pop();
    text
}

impl Grounded for ImportOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("import! expects two arguments: space and file path");
        let space = args.get(0).ok_or_else(arg_error)?;
        let file = args.get(1).ok_or_else(arg_error)?;

        let mut path = self.cwd.clone();
        // TODO: replace Symbol by grounded String?
        if let Atom::Symbol(file) = file {
            path.push(remove_quotes(file.name()));
            log::trace!("import! load file, full path: {}", path.display());
        } else {
            return Err("import! expects a file path as a second argument".into())
        }

        match space {
            Atom::Symbol(space) => {
                let name = space.name();
                let space = Shared::new(GroundingSpace::new());
                let space_atom = Atom::value(space.clone());
                let regex = Regex::new(name)
                    .map_err(|err| format!("Could convert space name {} into regex: {}", name, err))?;
                self.tokenizer.borrow_mut()
                    .register_token(regex, move |_| { space_atom.clone() });
                let mut next_cwd = path.clone();
                next_cwd.pop();
                let metta = Metta::from_space_cwd(space.clone(), next_cwd);
                let program = std::fs::read_to_string(&path)
                    .map_err(|err| format!("Could not read file {}: {}", path.display(), err))?;
                let _result = metta.run(&mut SExprParser::new(program.as_str()))?;
                Ok(vec![])
            },
            Atom::Grounded(_) => Err("import! for existing space is not implemented!".into()),
            _ => Err("import! expects space as a first argument".into()),
        }
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

impl Display for ImportOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "import!")
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

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![Atom::sym("T")]]));
    }
}
