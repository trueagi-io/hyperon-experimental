use crate::*;
use crate::metta::*;
use crate::atom::serial;

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
        let (a, b) = Number::promote(self.clone(), other.clone());
        match (a, b) {
            (Number::Integer(a), Number::Integer(b)) => a == b,
            (Number::Float(a), Number::Float(b)) => a == b,
            _ => panic!("Unexpected state!"),
        }
    }
}

impl Into<Number> for i64 {
    fn into(self) -> Number {
        Number::Integer(self)
    }
}

impl Into<Number> for f64 {
    fn into(self) -> Number {
        Number::Float(self)
    }
}

impl Into<i64> for Number {
    fn into(self) -> i64 {
        match self {
            Number::Integer(n) => n,
            Number::Float(n) => n as i64,
        }
    }
}

impl Into<f64> for Number {
    fn into(self) -> f64 {
        match self {
            Number::Integer(n) => n as f64,
            Number::Float(n) => n,
        }
    }
}

impl Number {
    pub fn from_int_str(num: &str) -> Result<Self, String> {
        let n = num.parse::<i64>().map_err(|e| format!("Could not parse integer: '{num}', {e}"))?;
        Ok(Self::Integer(n))
    }

    pub fn from_float_str(num: &str) -> Result<Self, String> {
        let n = num.parse::<f64>().map_err(|e| format!("Could not parse float: '{num}', {e}"))?;
        Ok(Self::Float(n))
    }

    pub fn promote(a: Number, b: Number) -> (Number, Number) {
        let res_type = &NumberType::widest_type(a.get_type(), b.get_type());
        (a.cast(res_type), b.cast(res_type))
    }

    fn get_type(&self) -> NumberType {
        match self {
            Number::Integer(_) => NumberType::Integer,
            Number::Float(_) => NumberType::Float,
        }
    }

    fn cast(self, t: &NumberType) -> Number {
        match t {
            NumberType::Integer => Number::Integer(self.into()),
            NumberType::Float => Number::Float(self.into()),
        }
    }
}

#[derive(PartialEq)]
enum NumberType {
    Integer,
    Float,
}

impl NumberType {
    fn widest_type(a: NumberType, b: NumberType) -> NumberType {
        // wanted using std::cmp::max but looks like this approach is much much simpler
        if a == NumberType::Float || b == NumberType::Float {
            NumberType::Float
        } else {
            NumberType::Integer
        }
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

    fn serialize(&self, serializer: &mut dyn serial::Serializer) -> serial::Result {
        match self {
            &Self::Integer(n) => serializer.serialize_i64(n),
            &Self::Float(n) => serializer.serialize_f64(n),
        }
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

impl Into<Bool> for bool {
    fn into(self) -> Bool {
        Bool(self)
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

    fn as_match(&self) -> Option<&dyn CustomMatch> {
        Some(self)
    }

    fn serialize(&self, serializer: &mut dyn serial::Serializer) -> serial::Result {
        serializer.serialize_bool(self.0)
    }
}

impl CustomMatch for Bool {
    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        match_by_bidirectional_equality(self, other)
    }
}

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
                let arg_error = || ExecError::from(concat!(stringify!($op), " expects two number arguments"));
                let a = AsPrimitive::from_atom(args.get(0).ok_or_else(arg_error)?).as_number().ok_or_else(arg_error)?;
                let b = AsPrimitive::from_atom(args.get(1).ok_or_else(arg_error)?).as_number().ok_or_else(arg_error)?;

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
def_binary_number_op!(DivOp, /, ATOM_TYPE_NUMBER, Number);
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
                let arg_error = || ExecError::from(concat!(stringify!($disp), " expects two boolean arguments"));
                let Bool(a) = AsPrimitive::from_atom(args.get(0).ok_or_else(arg_error)?).as_bool().ok_or_else(arg_error)?;
                let Bool(b) = AsPrimitive::from_atom(args.get(1).ok_or_else(arg_error)?).as_bool().ok_or_else(arg_error)?;

                Ok(vec![Atom::gnd(Bool(a $op b))])
            }
        }
    }
}

def_binary_bool_op!(AndOp, and, &&);
def_binary_bool_op!(OrOp, or, ||);

// NOTE: xor and flip are absent in Python intentionally for conversion testing
def_binary_bool_op!(XorOp, xor, ^);

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

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FlipOp {
    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        Ok(vec![Atom::gnd(Bool(rand::random()))])
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

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NotOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("not expects one boolean arguments");
        let &Bool(a) = args.get(0).ok_or_else(arg_error)?.as_gnd::<Bool>().ok_or_else(arg_error)?;

        Ok(vec![Atom::gnd(Bool(!a))])
    }
}

#[derive(Default)]
struct BoolSerializer {
    value: Option<Bool>,
}

impl serial::Serializer for BoolSerializer {
    fn serialize_bool(&mut self, v: bool) -> serial::Result {
        self.value = Some(Bool(v));
        Ok(())
    }
}

#[derive(Default)]
struct NumberSerializer {
    value: Option<Number>,
}

impl serial::Serializer for NumberSerializer {
    fn serialize_i64(&mut self, v: i64) -> serial::Result {
        self.value = Some(Number::Integer(v));
        Ok(())
    }
    fn serialize_f64(&mut self, v: f64) -> serial::Result {
        self.value = Some(Number::Float(v));
        Ok(())
    }
}

pub struct AsPrimitive<'a> {
    atom: &'a super::Atom
}

impl<'a> AsPrimitive<'a> {
    pub fn from_atom(atom: &'a super::Atom) -> Self {
        Self{ atom }
    }

    fn as_gnd(&self) -> Option<&dyn super::GroundedAtom> {
        std::convert::TryInto::<&dyn super::GroundedAtom>::try_into(self.atom).ok()
    }

    fn as_type<T: 'static + Clone, S: serial::ConvertingSerializer<T>>(&self, mut serializer: S) -> Option<T> {
       self.as_gnd()
           .map(|gnd| {
               gnd.as_any_ref()
                   .downcast_ref::<T>()
                   .cloned()
                   .or_else(|| {
                       let _ = gnd.serialize(serializer.as_mut());
                       serializer.into_type()
                   })
           }).flatten()
    }

    pub fn as_bool(self) -> Option<Bool> {
       self.as_type(BoolSerializer::default())
    }

    pub fn as_number(self) -> Option<Number> {
       self.as_type(NumberSerializer::default())
    }
}

impl serial::ConvertingSerializer<Bool> for BoolSerializer {
    fn as_mut(&mut self) -> &mut dyn serial::Serializer {
        self
    }
    fn into_type(self) -> Option<Bool> {
        self.value
    }
}

impl serial::ConvertingSerializer<Number> for NumberSerializer {
    fn as_mut(&mut self) -> &mut dyn serial::Serializer {
        self
    }
    fn into_type(self) -> Option<Number> {
        self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number() {
        assert_eq!(Number::from_int_str("12345").unwrap(), Number::Integer(12345i64));
        assert_eq!(Number::from_float_str("123.45").unwrap(), Number::Float(123.45f64));
        assert_eq!(Number::from_float_str("12345e-02").unwrap(), Number::Float(123.45f64));
        assert_eq!(Number::from_float_str("1.2345e+2").unwrap(), Number::Float(123.45f64));
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
