use hyperon_atom::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use super::regex;
use hyperon_atom::gnd::number::*;
use crate::metta::runner::bool::*;

use std::fmt::Display;

macro_rules! def_binary_number_op {
    ($name:ident, $op:tt, $r:ident, $ret_type:ident) => {
        #[derive(Clone, PartialEq, Debug)]
        pub struct $name{}

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, stringify!($op))
            }
        }

        impl Grounded for $name {
            fn type_(&self) -> Atom {
                Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, $r])
            }

            fn as_execute(&self) -> Option<&dyn CustomExecute> {
                Some(self)
            }
        }

        impl CustomExecute for $name {
            fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
                let arg_error = || ExecError::IncorrectArgument;
                let a = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?;
                let b = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?;

                let (a, b) = Number::promote(a, b);
                let res: $ret_type = match (a, b) {
                    (Number::Integer(a), Number::Integer(b)) => (a $op b).into(),
                    (Number::Float(a), Number::Float(b)) => (a $op b).into(),
                    _ => panic!("Unexpected state"),
                };

                Ok(vec![Atom::gnd(res)])
            }
        }
    }
}

def_binary_number_op!(SumOp, +, ATOM_TYPE_NUMBER, Number);
def_binary_number_op!(SubOp, -, ATOM_TYPE_NUMBER, Number);
def_binary_number_op!(MulOp, *, ATOM_TYPE_NUMBER, Number);
def_binary_number_op!(ModOp, %, ATOM_TYPE_NUMBER, Number);
def_binary_number_op!(LessOp, <, ATOM_TYPE_BOOL, Bool);
def_binary_number_op!(GreaterOp, >, ATOM_TYPE_BOOL, Bool);
def_binary_number_op!(LessEqOp, <=, ATOM_TYPE_BOOL, Bool);
def_binary_number_op!(GreaterEqOp, >=, ATOM_TYPE_BOOL, Bool);

macro_rules! def_binary_bool_op {
    ($name:ident, $disp:ident, $op:tt) => {
        #[derive(Clone, PartialEq, Debug)]
        pub struct $name{}

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, stringify!($disp))
            }
        }

        impl Grounded for $name {
            fn type_(&self) -> Atom {
                Atom::expr([ARROW_SYMBOL, ATOM_TYPE_BOOL, ATOM_TYPE_BOOL, ATOM_TYPE_BOOL])
            }

            fn as_execute(&self) -> Option<&dyn CustomExecute> {
                Some(self)
            }
        }

        impl CustomExecute for $name {
            fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
                let arg_error = || ExecError::IncorrectArgument;
                let Bool(a) = args.get(0).and_then(Bool::from_atom).ok_or_else(arg_error)?;
                let Bool(b) = args.get(1).and_then(Bool::from_atom).ok_or_else(arg_error)?;

                Ok(vec![Atom::gnd(Bool(a $op b))])
            }
        }
    }
}

def_binary_bool_op!(AndOp, and, &&);
def_binary_bool_op!(OrOp, or, ||);

// NOTE: xor is absent in Python intentionally for conversion testing
def_binary_bool_op!(XorOp, xor, ^);


#[derive(Clone, PartialEq, Debug)]
pub struct NotOp{}

impl Display for NotOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "not")
    }
}

impl Grounded for NotOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_BOOL, ATOM_TYPE_BOOL])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NotOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::IncorrectArgument;
        let Bool(a) = args.get(0).and_then(Bool::from_atom).ok_or_else(arg_error)?;

        Ok(vec![Atom::gnd(Bool(!a))])
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct DivOp{}

impl Display for DivOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "/")
    }
}

impl Grounded for DivOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for DivOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("Divide expects two numbers: dividend and divisor");
        let dividend = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?;
        let divisor = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?;

        let (dividend, divisor) = Number::promote(dividend, divisor);
        match (dividend, divisor) {
            (Number::Integer(_), Number::Integer(0)) => Err(ExecError::from("DivisionByZero")),
            (Number::Integer(a), Number::Integer(b)) => Ok(vec![Atom::gnd(Number::Integer(a / b))]),
            (Number::Float(a), Number::Float(b)) => Ok(vec![Atom::gnd(Number::Float(a / b))]),
            _ => panic!("Unexpected state")
        }
    }
}

pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    tref.register_fallible_token(regex(r"[\-\+]?\d+"),
        |token| { Ok(Atom::gnd(Number::from_int_str(token)?)) });
    tref.register_fallible_token(regex(r"[\-\+]?\d+\.\d+"),
        |token| { Ok(Atom::gnd(Number::from_float_str(token)?)) });
    tref.register_fallible_token(regex(r"[\-\+]?\d+(\.\d+)?[eE][\-\+]?\d+"),
        |token| { Ok(Atom::gnd(Number::from_float_str(token)?)) });
    tref.register_token(regex(r"True|False"),
        |token| { Atom::gnd(Bool::from_str(token)) });

    let sum_op = Atom::gnd(SumOp{});
    tref.register_token(regex(r"\+"), move |_| { sum_op.clone() });
    let sub_op = Atom::gnd(SubOp{});
    tref.register_token(regex(r"\-"), move |_| { sub_op.clone() });
    let mul_op = Atom::gnd(MulOp{});
    tref.register_token(regex(r"\*"), move |_| { mul_op.clone() });
    let div_op = Atom::gnd(DivOp{});
    tref.register_token(regex(r"/"), move |_| { div_op.clone() });
    let mod_op = Atom::gnd(ModOp{});
    tref.register_token(regex(r"%"), move |_| { mod_op.clone() });
    let lt_op = Atom::gnd(LessOp{});
    tref.register_token(regex(r"<"), move |_| { lt_op.clone() });
    let gt_op = Atom::gnd(GreaterOp{});
    tref.register_token(regex(r">"), move |_| { gt_op.clone() });
    let le_op = Atom::gnd(LessEqOp{});
    tref.register_token(regex(r"<="), move |_| { le_op.clone() });
    let ge_op = Atom::gnd(GreaterEqOp{});
    tref.register_token(regex(r">="), move |_| { ge_op.clone() });
    let and_op = Atom::gnd(AndOp{});
    tref.register_token(regex(r"and"), move |_| { and_op.clone() });
    let or_op = Atom::gnd(OrOp{});
    tref.register_token(regex(r"or"), move |_| { or_op.clone() });
    let not_op = Atom::gnd(NotOp{});
    tref.register_token(regex(r"not"), move |_| { not_op.clone() });
    // NOTE: xor is absent in Python intentionally for conversion testing
    let xor_op = Atom::gnd(XorOp{});
    tref.register_token(regex(r"xor"), move |_| { xor_op.clone() });
}

#[cfg(test)]
mod tests {
    use crate::metta::runner::run_program;
    use super::*;

    macro_rules! assert_binary_op {
        ($name:ident, $a: expr, $b: expr, $r: expr) => {
            assert_eq!($name{}.execute(&mut vec![Atom::gnd($a), Atom::gnd($b)]), Ok(vec![Atom::gnd($r)]));
        }
    }

    macro_rules! assert_unary_op {
        ($name:ident, $a: expr, $r: expr) => {
            assert_eq!($name{}.execute(&mut vec![Atom::gnd($a)]), Ok(vec![Atom::gnd($r)]));
        }
    }

    #[test]
    fn div_errors() {
        assert_eq!(run_program(&format!("!(assertEqual (/ 5 0) (Error (/ 5 0) DivisionByZero))")), Ok(vec![vec![UNIT_ATOM]]));
        assert_eq!(run_program(&format!("!(assertEqual (let $div (/ 5.0 0.0) (isinf-math $div)) True)")), Ok(vec![vec![UNIT_ATOM]]));
    }

    #[test]
    fn and() {
        assert_binary_op!(AndOp, Bool(true), Bool(true), Bool(true));
        assert_binary_op!(AndOp, Bool(true), Bool(false), Bool(false));
        assert_binary_op!(AndOp, Bool(false), Bool(true), Bool(false));
        assert_binary_op!(AndOp, Bool(false), Bool(false), Bool(false));
    }

    #[test]
    fn or() {
        assert_binary_op!(OrOp, Bool(true), Bool(true), Bool(true));
        assert_binary_op!(OrOp, Bool(true), Bool(false), Bool(true));
        assert_binary_op!(OrOp, Bool(false), Bool(true), Bool(true));
        assert_binary_op!(OrOp, Bool(false), Bool(false), Bool(false));
    }

    #[test]
    fn not() {
        assert_unary_op!(NotOp, Bool(true), Bool(false));
        assert_unary_op!(NotOp, Bool(false), Bool(true));
    }

    #[test]
    fn sum_op() {
        assert_binary_op!(SumOp, Number::Integer(40), Number::Integer(2), Number::Integer(42));
        assert_binary_op!(SumOp, Number::Integer(40), Number::Float(2.42), Number::Float(42.42));
        assert_binary_op!(SumOp, Number::Float(40.42), Number::Integer(2), Number::Float(42.42));
        assert_binary_op!(SumOp, Number::Float(40.40), Number::Float(2.02), Number::Float(42.42));
    }

    #[test]
    fn sub_op() {
        assert_binary_op!(SubOp, Number::Integer(44), Number::Integer(2), Number::Integer(42));
        assert_binary_op!(SubOp, Number::Integer(44), Number::Float(2.42), Number::Float(41.58));
        assert_binary_op!(SubOp, Number::Float(44.42), Number::Integer(2), Number::Float(42.42));
        assert_binary_op!(SubOp, Number::Float(44.5), Number::Float(2.5), Number::Float(42.0));
    }

    #[test]
    fn mul_op() {
        assert_binary_op!(MulOp, Number::Integer(6), Number::Integer(7), Number::Integer(42));
        assert_binary_op!(MulOp, Number::Integer(4), Number::Float(10.5), Number::Float(42.0));
        assert_binary_op!(MulOp, Number::Float(10.5), Number::Integer(4), Number::Float(42.0));
        assert_binary_op!(MulOp, Number::Float(2.5), Number::Float(16.8), Number::Float(42.0));
    }

    #[test]
    fn div_op() {
        assert_binary_op!(DivOp, Number::Integer(84), Number::Integer(2), Number::Integer(42));
        assert_binary_op!(DivOp, Number::Integer(441), Number::Float(10.5), Number::Float(42.0));
        assert_binary_op!(DivOp, Number::Float(84.0), Number::Integer(2), Number::Float(42.0));
        assert_binary_op!(DivOp, Number::Float(430.5), Number::Float(10.25), Number::Float(42.0));
    }

    #[test]
    fn mod_op() {
        assert_binary_op!(ModOp, Number::Integer(85), Number::Integer(43), Number::Integer(42));
        assert_binary_op!(ModOp, Number::Integer(85), Number::Float(43.5), Number::Float(41.5));
        assert_binary_op!(ModOp, Number::Float(85.5), Number::Integer(43), Number::Float(42.5));
        assert_binary_op!(ModOp, Number::Float(85.5), Number::Float(43.5), Number::Float(42.0));
    }
}
