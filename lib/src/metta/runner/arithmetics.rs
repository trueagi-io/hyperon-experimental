use crate::*;
use crate::metta::*;
use crate::matcher::MatchResultIter;

use std::fmt::Display;

pub const ATOM_TYPE_NUMBER : Atom = sym!("Number");
pub const ATOM_TYPE_BOOL : Atom = sym!("Bool");

#[derive(Clone, PartialEq, Debug)]
pub enum Number {
    Integer(i64),
    Float(f64),
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
        match_by_equality(self, other)
    }
}

macro_rules! def_binary_number_op {
    ($name:ident, $op:tt) => {
        #[derive(Clone, PartialEq, Debug)]
        pub struct $name{}

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, stringify!($op))
            }
        }

        impl Grounded for $name {
            fn type_(&self) -> Atom {
                Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
            }

            fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
                let arg_error = || ExecError::from(concat!(stringify!($op), " expects two number arguments"));
                let a = args.get(0).ok_or_else(arg_error)?.as_gnd::<Number>().ok_or_else(arg_error)?;
                let b = args.get(1).ok_or_else(arg_error)?.as_gnd::<Number>().ok_or_else(arg_error)?;

                let res = match (a, b) {
                    (Number::Integer(a), Number::Integer(b)) => Number::Integer(a $op b),
                    (Number::Integer(a), Number::Float(b)) => Number::Float((*a as f64) $op b),
                    (Number::Float(a), Number::Integer(b)) => Number::Float(a $op (*b as f64)),
                    (Number::Float(a), Number::Float(b)) => Number::Float(a $op b),
                };

                Ok(vec![Atom::gnd(res)])
            }

            fn match_(&self, other: &Atom) -> MatchResultIter {
                match_by_equality(self, other)
            }
        }
    }
}

def_binary_number_op!(SumOp, +);
def_binary_number_op!(SubOp, -);
def_binary_number_op!(MulOp, *);
def_binary_number_op!(DivOp, /);
def_binary_number_op!(ModOp, %);

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

    macro_rules! assert_number_binary_op {
        ($name:ident, $a: expr, $b: expr, $r: expr) => {
            assert_eq!($name{}.execute(&mut vec![Atom::gnd($a), Atom::gnd($b)]), Ok(vec![Atom::gnd($r)]));
        }
    }

    #[test]
    fn sum_op() {
        assert_number_binary_op!(SumOp, Number::Integer(40), Number::Integer(2), Number::Integer(42));
        assert_number_binary_op!(SumOp, Number::Integer(40), Number::Float(2.42), Number::Float(42.42));
        assert_number_binary_op!(SumOp, Number::Float(40.42), Number::Integer(2), Number::Float(42.42));
        assert_number_binary_op!(SumOp, Number::Float(40.40), Number::Float(2.02), Number::Float(42.42));
    }

    #[test]
    fn sub_op() {
        assert_number_binary_op!(SubOp, Number::Integer(44), Number::Integer(2), Number::Integer(42));
        assert_number_binary_op!(SubOp, Number::Integer(44), Number::Float(2.42), Number::Float(41.58));
        assert_number_binary_op!(SubOp, Number::Float(44.42), Number::Integer(2), Number::Float(42.42));
        assert_number_binary_op!(SubOp, Number::Float(44.5), Number::Float(2.5), Number::Float(42.0));
    }

    #[test]
    fn mul_op() {
        assert_number_binary_op!(MulOp, Number::Integer(6), Number::Integer(7), Number::Integer(42));
        assert_number_binary_op!(MulOp, Number::Integer(4), Number::Float(10.5), Number::Float(42.0));
        assert_number_binary_op!(MulOp, Number::Float(10.5), Number::Integer(4), Number::Float(42.0));
        assert_number_binary_op!(MulOp, Number::Float(2.5), Number::Float(16.8), Number::Float(42.0));
    }

    #[test]
    fn div_op() {
        assert_number_binary_op!(DivOp, Number::Integer(84), Number::Integer(2), Number::Integer(42));
        assert_number_binary_op!(DivOp, Number::Integer(441), Number::Float(10.5), Number::Float(42.0));
        assert_number_binary_op!(DivOp, Number::Float(84.0), Number::Integer(2), Number::Float(42.0));
        assert_number_binary_op!(DivOp, Number::Float(430.5), Number::Float(10.25), Number::Float(42.0));
    }

    #[test]
    fn mod_op() {
        assert_number_binary_op!(ModOp, Number::Integer(85), Number::Integer(43), Number::Integer(42));
        assert_number_binary_op!(ModOp, Number::Integer(85), Number::Float(43.5), Number::Float(41.5));
        assert_number_binary_op!(ModOp, Number::Float(85.5), Number::Integer(43), Number::Float(42.5));
        assert_number_binary_op!(ModOp, Number::Float(85.5), Number::Float(43.5), Number::Float(42.0));
    }
}
