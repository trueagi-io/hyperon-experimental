
use crate::*;
use crate::metta::text::{Tokenizer, SExprParser};
use crate::space::DynSpace;
use crate::space::grounding::GroundingSpace;

pub(crate) fn metta_space(text: &str) -> DynSpace {
    let mut space = GroundingSpace::new();
    let mut parser = SExprParser::new(text);
    while let Some(atom) = parser.parse(&Tokenizer::new()).unwrap() {
        space.add(atom);
    }
    space.into()
}

pub(crate) fn metta_atom(atom_str: &str) -> Atom {
    let mut parser = SExprParser::new(atom_str);
    let atom = parser.parse(&Tokenizer::new()).unwrap().expect("Single atom is expected");
    atom
}
