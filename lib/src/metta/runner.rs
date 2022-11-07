use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::space::grounding::GroundingSpace;
use super::text::{Tokenizer, SExprParser};
use super::interpreter::interpret;

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
        {
            fn regex(regex: &str) -> Regex {
                Regex::new(regex).unwrap()
            }

            let mut tref = tokenizer.borrow_mut();
            let match_op = Atom::gnd(MatchOp{});
            tref.register_token(regex(r"match"), move |_| { match_op.clone() });
            let space_val = Atom::value(space.clone());
            tref.register_token(regex(r"&self"), move |_| { space_val.clone() });
            let import_op = Atom::gnd(ImportOp::new(cwd.clone(), space.clone(), tokenizer.clone()));
            tref.register_token(regex(r"import!"), move |_| { import_op.clone() });
            let bind_op = Atom::gnd(BindOp::new(tokenizer.clone()));
            tref.register_token(regex(r"bind!"), move |_| { bind_op.clone() });
            let new_space_op = Atom::gnd(NewSpaceOp{});
            tref.register_token(regex(r"new-space"), move |_| { new_space_op.clone() });
        }
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
                                log::trace!("Metta::run: adding atom: {} into space: {:?}", atom, self.space);
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

#[derive(Clone, PartialEq, Debug)]
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

impl Display for ImportOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "import!")
    }
}

fn remove_quotes(text: &str) -> String {
    text.chars().skip(1).take(text.len() - 2).collect()
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

#[derive(Clone, PartialEq, Debug)]
struct MatchOp {}

impl Display for MatchOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "match")
    }
}

impl Grounded for MatchOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<Shared<GroundingSpace>>(), ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("match expects three arguments: space, pattern and template");
        let space = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let template = args.get(2).ok_or_else(arg_error)?;
        log::trace!("match_op: space: {:?}, pattern: {:?}, template: {:?}", space, pattern, template);
        let space = Atom::as_gnd::<Shared<GroundingSpace>>(space).ok_or("match expects a space as a second argument")?;
        Ok(space.borrow().subst(&pattern, &template))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
struct BindOp {
    tokenizer: Shared<Tokenizer>,
}

impl BindOp {
    fn new(tokenizer: Shared<Tokenizer>) -> Self {
        Self{ tokenizer }
    }
}

impl Display for BindOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bind!")
    }
}

fn atom_as_sym(atom: &Atom) -> Option<&SymbolAtom> {
    match atom {
        Atom::Symbol(sym) => Some(sym),
        _ => None,
    }
}

impl Grounded for BindOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, ATOM_TYPE_UNDEFINED, ATOM_TYPE_UNDEFINED])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("bind! expects two arguments: token and atom");
        let token = atom_as_sym(args.get(0).ok_or_else(arg_error)?).ok_or("bind! expects symbol atom as a token")?.name();
        let atom = args.get(1).ok_or_else(arg_error)?.clone();

        let token_regex = Regex::new(token).map_err(|err| format!("Could convert token {} into regex: {}", token, err))?;
        self.tokenizer.borrow_mut().register_token(token_regex, move |_| { atom.clone() });
        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
struct NewSpaceOp {}

impl Display for NewSpaceOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "new-space")
    }
}

impl Grounded for NewSpaceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<Shared<GroundingSpace>>()])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        if args.len() == 0 {
            let space = Atom::value(Shared::new(GroundingSpace::new()));
            Ok(vec![space])
        } else {
            Err("new-space doesn't expect arguments".into())
        }
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
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

    #[test]
    fn test_match() {
        let program = "
            (A B)
            !(match &self (A B) (B A))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
    }

    #[test]
    fn new_space() {
        let program = "
            (A B)
            !(match (new-space) (A B) (B A))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn bind_new_space() {
        let program = "
            (A B)
            !(bind! &my (new-space))
            !(match &my (A B) (B A))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![], vec![]]));
    }
}
