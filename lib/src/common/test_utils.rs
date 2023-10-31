
use crate::*;
use crate::metta::runner::*;
use crate::metta::runner::EnvBuilder;
use crate::metta::text::SExprParser;
use crate::space::grounding::GroundingSpace;

pub(crate) fn metta_space(text: &str) -> GroundingSpace {
    let metta = Metta::new(Some(EnvBuilder::test_env()));
    let mut space = GroundingSpace::new();
    let mut parser = SExprParser::new(text);
    while let Some(atom) = parser.parse(&metta.tokenizer().borrow()).unwrap() {
        space.add(atom);
    }
    space
}

pub(crate) fn metta_atom(atom_str: &str) -> Atom {
    let metta = Metta::new(Some(EnvBuilder::test_env()));
    let mut parser = SExprParser::new(atom_str);
    let atom = parser.parse(&metta.tokenizer().borrow()).unwrap().expect("Single atom is expected");
    atom
}
