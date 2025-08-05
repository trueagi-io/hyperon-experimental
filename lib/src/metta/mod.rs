//! Contains MeTTa specific types, constants and functions.

pub mod text;
pub mod interpreter;
pub mod types;
pub mod runner;

use hyperon_atom::*;
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
pub const STACK_OVERFLOW_SYMBOL : Atom = sym!("StackOverflow");
pub const NO_RETURN_SYMBOL : Atom = sym!("NoReturn");

pub const EMPTY_SYMBOL : Atom = sym!("Empty");

pub const EVAL_SYMBOL : Atom = sym!("eval");
pub const EVALC_SYMBOL : Atom = sym!("evalc");
pub const CHAIN_SYMBOL : Atom = sym!("chain");
pub const UNIFY_SYMBOL : Atom = sym!("unify");
pub const MATCH_SYMBOL : Atom = sym!("match");
pub const DECONS_ATOM_SYMBOL : Atom = sym!("decons-atom");
pub const CONS_ATOM_SYMBOL : Atom = sym!("cons-atom");
pub const FUNCTION_SYMBOL : Atom = sym!("function");
pub const RETURN_SYMBOL : Atom = sym!("return");
pub const COLLAPSE_BIND_SYMBOL : Atom = sym!("collapse-bind");
pub const SUPERPOSE_BIND_SYMBOL : Atom = sym!("superpose-bind");

pub const METTA_SYMBOL : Atom = sym!("metta");
pub const CALL_NATIVE_SYMBOL : Atom = sym!("call-native");
pub const CONTEXT_SPACE_SYMBOL : Atom = sym!("context-space");

pub const UNIT_ATOM: Atom = constexpr!();
pub const UNIT_TYPE: Atom = constexpr!(("->"));

/// Initializes an error expression atom
pub fn error_atom(err_atom: Option<Atom>, err_code: Option<Atom>, message: String) -> Atom {
    let err_atom = match err_atom {
        Some(err_atom) => err_atom,
        None => EMPTY_SYMBOL,
    };
    if let Some(err_code) = err_code {
        Atom::expr([ERROR_SYMBOL, err_atom, err_code, Atom::sym(message)])
    } else {
        Atom::expr([ERROR_SYMBOL, err_atom, Atom::sym(message)])
    }
}

/// Tests whether or not an atom is an error expression
pub fn atom_is_error(atom: &Atom) -> bool {
    match atom {
        Atom::Expression(expr) => {
            expr.children().len() > 0 && expr.children()[0] == ERROR_SYMBOL
        },
        _ => false,
    }
}

/// Returns a message string from an error expression
///
/// NOTE: this function will panic if the supported atom is not a valid error expression
pub fn atom_error_message(atom: &Atom) -> &str {
    const PANIC_STR: &str = "Atom is not error expression";
    match atom {
        Atom::Expression(expr) => {
            let sym_atom = match expr.children().len() {
                3 => expr.children().get(2).unwrap(),
                4 => expr.children().get(3).unwrap(),
                _ => panic!("{}", PANIC_STR)
            };
            let sym_atom = <&SymbolAtom>::try_from(sym_atom).expect(PANIC_STR);
            sym_atom.name()
        },
        _ => panic!("{}", PANIC_STR)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unit_type() {
        assert_eq!(UNIT_ATOM, Atom::expr([]));
        assert_eq!(UNIT_TYPE, Atom::expr([ARROW_SYMBOL]));
    }
}
