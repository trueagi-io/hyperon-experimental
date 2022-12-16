use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::space::grounding::GroundingSpace;
use super::text::{Tokenizer, SExprParser};
use super::types::validate_atom;
use super::interpreter::interpret;

use std::path::PathBuf;
use std::collections::HashMap;

mod stdlib;

mod arithmetics;

const EXEC_SYMBOL : Atom = sym!("!");

pub struct Metta {
    space: Shared<GroundingSpace>,
    tokenizer: Shared<Tokenizer>,
    settings: Shared<HashMap<String, String>>,
}

enum Mode {
    ADD,
    INTERPRET,
}

impl Metta {
    pub fn new(space: Shared<GroundingSpace>, tokenizer: Shared<Tokenizer>) -> Self {
        Metta::from_space_cwd(space, tokenizer, PathBuf::from("."))
    }

    pub fn from_space_cwd(space: Shared<GroundingSpace>, tokenizer: Shared<Tokenizer>, cwd: PathBuf) -> Self {
        let settings = Shared::new(HashMap::new());
        let metta = Self{ space, tokenizer, settings };
        stdlib::register_tokens(&metta, cwd);
        metta
    }

    pub fn space(&self) -> Shared<GroundingSpace> {
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
            let atom = parser.parse(&self.tokenizer.borrow());
            match atom {
                Some(atom) => {
                    if atom == EXEC_SYMBOL {
                        mode = Mode::INTERPRET;
                        continue;
                    }
                    match mode {
                        Mode::ADD => match self.add_atom(atom) {
                            Err(atom) => results.push(vec![atom]),
                            Ok(()) => {},
                        }
                        Mode::INTERPRET => match self.evaluate_atom(atom) {
                            Err(msg) => return Err(msg),
                            Ok(result) => results.push(result),
                        },
                    }
                    mode = Mode::ADD;
                },
                None => break,
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
        if  is_type_check_enabled && !validate_atom(&self.space.borrow(), &atom) {
            Err(Atom::expr([ERROR_SYMBOL, atom, BAD_TYPE_SYMBOL]))
        } else {
            Ok(atom)
        }
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

        let metta = Metta::new(Shared::new(GroundingSpace::new()), Shared::new(Tokenizer::new()));
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

        let metta = Metta::new(Shared::new(GroundingSpace::new()), Shared::new(Tokenizer::new()));
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

        let metta = Metta::new(Shared::new(GroundingSpace::new()), Shared::new(Tokenizer::new()));
        metta.set_setting("type-check".into(), "auto".into());
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("foo" "b") "BadType")]]));
    }
}
