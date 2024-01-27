use crate::*;
use crate::metta::*;
use crate::matcher::MatchResultIter;

use std::fmt::Display;

pub const ATOM_TYPE_NUMBER : Atom = sym!("Number");
pub const ATOM_TYPE_BOOL : Atom = sym!("Bool");

#[derive(Clone, Debug)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

impl PartialEq<Self> for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Integer(a), Number::Integer(b)) => a == b,
            (Number::Integer(a), Number::Float(b)) => (*a as f64) == *b,
            (Number::Float(a), Number::Integer(b)) => *a == (*b as f64),
            (Number::Float(a), Number::Float(b)) => a == b,
        }
    }
}

trait IntoNumber {
    fn into_num(self) -> Number;
}

impl IntoNumber for i64 {
    fn into_num(self) -> Number {
        Number::Integer(self)
    }
}

impl IntoNumber for f64 {
    fn into_num(self) -> Number {
        Number::Float(self)
    }
}

impl Number {
    pub fn from_int_str(num: &str) -> Self {
        let n = num.parse::<i64>().expect("Could not parse integer");
        Self::Integer(n)
    }

    pub fn from_float_str(num: &str) -> Self {
        let n = num.parse::<f64>().expect("Could not parse float");
        Self::Float(n)
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "{}", n),
            Self::Float(n) => write!(f, "{}", n),
        }
    }
}

impl Grounded for Number {
    fn type_(&self) -> Atom {
        ATOM_TYPE_NUMBER
    }

    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        execute_not_executable(self)
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Bool(pub bool);

impl Bool {
    pub fn from_str(b: &str) -> Self {
        match b {
            "True" => Self(true),
            "False" => Self(false),
            _ => panic!("Could not parse Bool value: {}", b),
        }
    }
}

impl Display for Bool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            true => write!(f, "True"),
            false => write!(f, "False"),
        }
    }
}

impl Grounded for Bool {
    fn type_(&self) -> Atom {
        ATOM_TYPE_BOOL
    }

    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        execute_not_executable(self)
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        //TODO: THis is a stop-gap until we have a more robust value-bridging system in place
        // https://github.com/trueagi-io/hyperon-experimental/issues/351
        match_by_string_equality(&self.to_string(), other)
    }
}

macro_rules! def_binary_number_op {
    ($name:ident, $op:tt, $r:ident, $cast:expr) => {
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

            fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
                let arg_error = || ExecError::from(concat!(stringify!($op), " expects two number arguments"));
                let a = args.get(0).ok_or_else(arg_error)?.as_gnd::<Number>().ok_or_else(arg_error)?;
                let b = args.get(1).ok_or_else(arg_error)?.as_gnd::<Number>().ok_or_else(arg_error)?;

                let res = match (a, b) {
                    (&Number::Integer(a), &Number::Integer(b)) => $cast(a $op b),
                    (&Number::Integer(a), &Number::Float(b)) => $cast((a as f64) $op b),
                    (&Number::Float(a), &Number::Integer(b)) => $cast(a $op (b as f64)),
                    (&Number::Float(a), &Number::Float(b)) => $cast(a $op b),
                };

                Ok(vec![Atom::gnd(res)])
            }

            fn match_(&self, other: &Atom) -> MatchResultIter {
                match_by_equality(self, other)
            }
        }
    }
}

def_binary_number_op!(SumOp, +, ATOM_TYPE_NUMBER, IntoNumber::into_num);
def_binary_number_op!(SubOp, -, ATOM_TYPE_NUMBER, IntoNumber::into_num);
def_binary_number_op!(MulOp, *, ATOM_TYPE_NUMBER, IntoNumber::into_num);
def_binary_number_op!(DivOp, /, ATOM_TYPE_NUMBER, IntoNumber::into_num);
def_binary_number_op!(ModOp, %, ATOM_TYPE_NUMBER, IntoNumber::into_num);
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

            fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
                let arg_error = || ExecError::from(concat!(stringify!($disp), " expects two boolean arguments"));
                let &Bool(a) = args.get(0).ok_or_else(arg_error)?.as_gnd::<Bool>().ok_or_else(arg_error)?;
                let &Bool(b) = args.get(1).ok_or_else(arg_error)?.as_gnd::<Bool>().ok_or_else(arg_error)?;

                Ok(vec![Atom::gnd(Bool(a $op b))])
            }

            fn match_(&self, other: &Atom) -> MatchResultIter {
                match_by_equality(self, other)
            }
        }
    }
}

def_binary_bool_op!(AndOp, and, &&);
def_binary_bool_op!(OrOp, or, ||);

// NOTE: xor and flip are absent in Python intentionally for conversion testing
def_binary_bool_op!(XorOp, xor, ^);

use rand;
#[derive(Clone, PartialEq, Debug)]
pub struct FlipOp{}

impl Display for FlipOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "flip")
    }
}

impl Grounded for FlipOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_BOOL])
    }

    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        Ok(vec![Atom::gnd(Bool(rand::random()))])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}


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

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("not expects one boolean arguments");
        let &Bool(a) = args.get(0).ok_or_else(arg_error)?.as_gnd::<Bool>().ok_or_else(arg_error)?;

        Ok(vec![Atom::gnd(Bool(!a))])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number() {
        assert_eq!(Number::from_int_str("12345"), Number::Integer(12345i64));
        assert_eq!(Number::from_float_str("123.45"), Number::Float(123.45f64));
        assert_eq!(Number::from_float_str("12345e-02"), Number::Float(123.45f64));
        assert_eq!(Number::from_float_str("1.2345e+2"), Number::Float(123.45f64));
        assert_eq!(format!("{}", Number::Integer(12345i64)), "12345");
        assert_eq!(format!("{}", Number::Float(123.45f64)), "123.45");
    }

    #[test]
    fn bool() {
        assert_eq!(Bool::from_str("True"), Bool(true));
        assert_eq!(Bool::from_str("False"), Bool(false));
        assert_eq!(format!("{}", Bool(true)), "True");
        assert_eq!(format!("{}", Bool(false)), "False");
    }

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
