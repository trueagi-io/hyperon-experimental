pub mod text;
pub mod interpreter;
pub mod types;

mod examples;

use crate::Atom;
use crate::space::grounding::GroundingSpace;
use text::{SExprParser, Tokenizer, SExprSpace};
use crate::common::*;
use regex::Regex;

pub fn metta_space(text: &str) -> GroundingSpace {
    let mut parser = SExprSpace::new();
    parser.register_token(Regex::new(r"\d+").unwrap(),
        |n| Atom::value(n.parse::<i32>().unwrap()));
    parser.register_token(Regex::new(r"true|false").unwrap(),
        |b| Atom::value(b.parse::<bool>().unwrap()));
    parser.register_token(Regex::new(r"<").unwrap(), |_| Atom::gnd(LT));
    parser.add_str(text).unwrap();
    GroundingSpace::from(&parser)
}

fn common_tokenizer() -> Tokenizer {
    let mut tokenizer = Tokenizer::new();
    // FIXME: this code is duplicated in metta_space()
    tokenizer.register_token(Regex::new(r"\d+").unwrap(),
        |num| Atom::value(num.parse::<i32>().unwrap()));
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


