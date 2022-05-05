pub mod text;
pub mod interpreter;
pub mod types;

mod examples;

use crate::Atom;
use crate::space::grounding::GroundingSpace;
use text::{SExprParser, Tokenizer, SExprSpace};

pub fn metta_space(text: &str) -> GroundingSpace {
    let mut parser = SExprSpace::new();
    parser.add_str(text).unwrap();
    GroundingSpace::from(&parser)
}

pub fn metta_atom(atom: &str) -> Atom {
    let tokenizer = Tokenizer::new();
    let mut parser = SExprParser::new(atom);
    let atom = parser.parse(&tokenizer);
    if let Some(atom) = atom {
        atom
    } else {
        panic!("Single atom is expected");
    }
}


