pub mod text;
pub mod interpreter;
pub mod types;

mod examples;

use text::{SExprParser, Tokenizer, SExprSpace};
use regex::Regex;

use crate::*;
use crate::common::*;
use crate::space::grounding::GroundingSpace;

pub const ATOM_TYPE_UNDEFINED : Atom = sym!("%Undefined%");
pub const ATOM_TYPE_FUNCTION : Atom = sym!("->");
pub const ATOM_TYPE_TYPE : Atom = sym!("Type");
pub const ATOM_TYPE_ATOM : Atom = sym!("Atom");
pub const ATOM_TYPE_SYMBOL : Atom = sym!("Symbol");
pub const ATOM_TYPE_VARIABLE : Atom = sym!("Variable");
pub const ATOM_TYPE_EXPRESSION : Atom = sym!("Expression");
pub const ATOM_TYPE_GROUNDED : Atom = sym!("Grounded");

pub const HAS_TYPE_SYMBOL : Atom = sym!(":");
pub const SUB_TYPE_SYMBOL : Atom = sym!(":<");
pub const EQUAL_SYMBOL : Atom = sym!("=");

pub fn metta_space(text: &str) -> GroundingSpace {
    let mut parser = SExprSpace::new(common_tokenizer());
    parser.add_str(text).unwrap();
    GroundingSpace::from(&parser)
}

fn common_tokenizer() -> Tokenizer {
    let mut tokenizer = Tokenizer::new();
    tokenizer.register_token(Regex::new(r"\d+").unwrap(),
        |n| Atom::value(n.parse::<i32>().unwrap()));
    tokenizer.register_token(Regex::new(r"true|false").unwrap(),
        |b| Atom::value(b.parse::<bool>().unwrap()));
    tokenizer.register_token(Regex::new(r"<").unwrap(), |_| Atom::gnd(LT));
    tokenizer
}

pub fn metta_atom(atom: &str) -> Atom {
    let tokenizer = common_tokenizer();
    let mut parser = SExprParser::new(atom);
    let atom = parser.parse(&tokenizer);
    if let Some(atom) = atom {
        atom
    } else {
        panic!("Single atom is expected");
    }
}


