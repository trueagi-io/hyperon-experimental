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
                    match self.interp_atom(mode, atom) {
                        Err(msg) => return Err(msg),
                        Ok(Some(result)) => results.push(result),
                        _ => {},
                    }
                    mode = Mode::ADD;
                },
                None => break,
            }
        }
        Ok(results)
    }

    pub fn evaluate_atom(&self, atom: Atom) -> Result<Vec<Atom>, String> {
        // FIXME: some code repitiion with `run`;
        //        can a better decomposition be done?
        match self.interp_atom(Mode::INTERPRET, atom) {
            Err(msg) => Err(msg),
            Ok(Some(result)) => Ok(result),
            _ => Ok(Vec::new()),
        }
    }

    fn interp_atom(&self, mode: Mode, atom: Atom) -> Result<Option<Vec<Atom>>, String> {
        if self.get_setting("type-check").map_or(false, |val| val == "auto") {
            if !validate_atom(&self.space.borrow(), &atom) {
                return Ok(Some(vec![Atom::expr([ERROR_SYMBOL, atom, BAD_TYPE_SYMBOL])]))
            }
        }
        match mode {
            Mode::ADD => {
                log::trace!("Metta::run: adding atom: {} into space: {:?}", atom, self.space);
                self.space.borrow_mut().add(atom);
                Ok(None) 
            },
            Mode::INTERPRET => {
                log::trace!("Metta::run: interpreting atom: {}", atom);
                let result = interpret(self.space.clone(), &atom);
                log::trace!("Metta::run: interpretation result {:?}", result);
                match result {
                    Ok(result) => Ok(Some(result)),
                    Err(message) => Err(format!("Error: {}", message)),
                }
            },
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
}
