pub mod text;
pub mod interpreter;
pub mod types;

mod examples;

use crate::Atom;
use crate::space::grounding::GroundingSpace;
use text::SExprSpace;

pub fn metta_space(text: &str) -> GroundingSpace {
    let mut parser = SExprSpace::new();
    parser.add_str(text).unwrap();
    GroundingSpace::from(&parser)
}

pub fn metta_atom(atom: &str) -> Atom {
    let space = metta_space(atom);
    if space.borrow_vec().len() != 1 {
        panic!("Single atom is expected");
    } else {
        space.leak().pop().unwrap()
    }
}


