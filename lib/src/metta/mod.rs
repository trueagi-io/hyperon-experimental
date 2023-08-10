//! Contains MeTTa specific types, constants and functions.

pub mod text;
pub mod interpreter;
#[cfg(feature = "minimal")]
pub mod interpreter2;
pub mod types;
pub mod runner;

use text::{SExprParser, Tokenizer};
use regex::Regex;

use crate::*;
use crate::common::*;
use crate::space::grounding::GroundingSpace;

pub const ATOM_TYPE_UNDEFINED : Atom = sym!("%Undefined%");
pub const ATOM_TYPE_TYPE : Atom = sym!("Type");
pub const ATOM_TYPE_ATOM : Atom = sym!("Atom");
pub const ATOM_TYPE_SYMBOL : Atom = sym!("Symbol");
pub const ATOM_TYPE_VARIABLE : Atom = sym!("Variable");
pub const ATOM_TYPE_EXPRESSION : Atom = sym!("Expression");
pub const ATOM_TYPE_GROUNDED : Atom = sym!("Grounded");

pub const HAS_TYPE_SYMBOL : Atom = sym!(":");
pub const SUB_TYPE_SYMBOL : Atom = sym!(":<");
pub const EQUAL_SYMBOL : Atom = sym!("=");
pub const ARROW_SYMBOL : Atom = sym!("->");
pub const ERROR_SYMBOL : Atom = sym!("Error");
pub const BAD_TYPE_SYMBOL : Atom = sym!("BadType");
pub const INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL : Atom = sym!("IncorrectNumberOfArguments");
pub const NOT_REDUCIBLE_SYMBOL : Atom = sym!("NotReducible");
pub const NO_VALID_ALTERNATIVES : Atom = sym!("NoValidAlternatives");

pub const EMPTY_SYMBOL : Atom = sym!("Empty");
pub const VOID_SYMBOL : Atom = sym!("Void");

pub const EVAL_SYMBOL : Atom = sym!("eval");
pub const CHAIN_SYMBOL : Atom = sym!("chain");
pub const UNIFY_SYMBOL : Atom = sym!("unify");
pub const DECONS_SYMBOL : Atom = sym!("decons");
pub const CONS_SYMBOL : Atom = sym!("cons");

// TODO: use stdlib to parse input text
pub fn metta_space(text: &str) -> GroundingSpace {
    let tokenizer = common_tokenizer();
    let mut parser = SExprParser::new(text);
    let mut space = GroundingSpace::new();
    loop {
        let atom = parser.parse(&tokenizer).unwrap();
        if let Some(atom) = atom {
            space.add(atom);
        } else {
            break;
        }
    }
    space
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

// TODO: use stdlib to parse input text
pub fn metta_atom(atom: &str) -> Atom {
    let tokenizer = common_tokenizer();
    let mut parser = SExprParser::new(atom);
    let atom = parser.parse(&tokenizer).unwrap();
    if let Some(atom) = atom {
        atom
    } else {
        panic!("Single atom is expected");
    }
}


