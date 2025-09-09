//! Contains MeTTa specific types, constants and functions.

pub mod text;
pub mod interpreter;
pub mod types;
pub mod runner;

use hyperon_atom::*;
use hyperon_macros::*;
use crate::metta::runner::str::atom_to_string;
use crate::space::grounding::GroundingSpace;

pub const ATOM_TYPE_UNDEFINED : Atom = metta_const!(%Undefined%);
pub const ATOM_TYPE_TYPE : Atom = metta_const!(Type);
pub const ATOM_TYPE_ATOM : Atom = metta_const!(Atom);
pub const ATOM_TYPE_SYMBOL : Atom = metta_const!(Symbol);
pub const ATOM_TYPE_VARIABLE : Atom = metta_const!(Variable);
pub const ATOM_TYPE_EXPRESSION : Atom = metta_const!(Expression);
pub const ATOM_TYPE_GROUNDED : Atom = metta_const!(Grounded);

pub const HAS_TYPE_SYMBOL : Atom = metta_const!(:);
pub const SUB_TYPE_SYMBOL : Atom = metta_const!(:<);
pub const EQUAL_SYMBOL : Atom = metta_const!(=);
pub const ARROW_SYMBOL : Atom = metta_const!(->);
pub const ERROR_SYMBOL : Atom = metta_const!(Error);
pub const BAD_TYPE_SYMBOL : Atom = metta_const!(BadType);
pub const INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL : Atom = metta_const!(IncorrectNumberOfArguments);
pub const NOT_REDUCIBLE_SYMBOL : Atom = metta_const!(NotReducible);
pub const STACK_OVERFLOW_SYMBOL : Atom = metta_const!(StackOverflow);
pub const NO_RETURN_SYMBOL : Atom = metta_const!(NoReturn);

pub const EMPTY_SYMBOL : Atom = metta_const!(Empty);

pub const EVAL_SYMBOL : Atom = metta_const!(eval);
pub const EVALC_SYMBOL : Atom = metta_const!(evalc);
pub const CHAIN_SYMBOL : Atom = metta_const!(chain);
pub const UNIFY_SYMBOL : Atom = metta_const!(unify);
pub const DECONS_ATOM_SYMBOL : Atom = metta_const!(decons-atom);
pub const CONS_ATOM_SYMBOL : Atom = metta_const!(cons-atom);
pub const FUNCTION_SYMBOL : Atom = metta_const!(function);
pub const RETURN_SYMBOL : Atom = metta_const!(return);
pub const COLLAPSE_BIND_SYMBOL : Atom = metta_const!(collapse-bind);
pub const SUPERPOSE_BIND_SYMBOL : Atom = metta_const!(superpose-bind);

pub const METTA_SYMBOL : Atom = metta_const!(metta);
pub const CALL_NATIVE_SYMBOL : Atom = metta_const!(call-native);
pub const CONTEXT_SPACE_SYMBOL : Atom = metta_const!(context-space);

pub const UNIT_ATOM: Atom = metta_const!(());
pub const UNIT_TYPE: Atom = metta_const!((->));

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
pub fn atom_error_message(atom: &Atom) -> String {
    const PANIC_STR: &str = "Atom is not error expression";
    match atom {
        Atom::Expression(expr) => {
            let sym_atom = match expr.children().len() {
                3 => expr.children().get(2).unwrap(),
                4 => expr.children().get(3).unwrap(),
                _ => panic!("{}", PANIC_STR)
            };
            atom_to_string(sym_atom)
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
