use crate::*;

use std::fmt::Display;
use std::cmp::Ordering;

pub const ATOM_TYPE_NUMBER : Atom = sym!("Number");

#[derive(Clone, Debug)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

impl PartialEq<Self> for Number {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl PartialOrd<Self> for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => Some(a.cmp(b)),
            (Self::Float(a), Self::Float(b)) => a.partial_cmp(b),
            (Self::Integer(a), Self::Float(b)) => compare_integer_float(*a, *b),
            (Self::Float(a), Self::Integer(b)) => compare_integer_float(*b, *a).map(Ordering::reverse),
        }
    }
}

// Keep the integer exact. Within this half-open interval the truncated float
// fits in i64; only an equal truncation needs its fractional part examined.
fn compare_integer_float(integer: i64, float: f64) -> Option<Ordering> {
    if float.is_nan() { return None; }
    if float >= 9223372036854775808.0 { return Some(Ordering::Less); }
    if float < -9223372036854775808.0 { return Some(Ordering::Greater); }
    let integral_order = integer.cmp(&(float as i64));
    if integral_order != Ordering::Equal {
        Some(integral_order)
    } else {
        0.0f64.partial_cmp(&float.fract())
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

impl TryFrom<&Atom> for Number {
    type Error = &'static str;
    fn try_from(value: &Atom) -> Result<Self, Self::Error> {
        std::convert::TryInto::<&dyn GroundedAtom>::try_into(value)
            .and_then(NumberSerializer::convert)
    }
}

impl TryFrom<&dyn GroundedAtom> for Number {
    type Error = &'static str;
    fn try_from(value: &dyn GroundedAtom) -> Result<Self, Self::Error> {
        NumberSerializer::convert(value)
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

    pub fn from_atom(atom: &Atom) -> Option<Self> {
        Number::try_from(atom).ok()
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
            Self::Float(n) => write!(f, "{:?}", n),
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

impl ConvertingSerializer<Number> for NumberSerializer {
    fn check_type(gnd: &dyn GroundedAtom) -> bool {
        gnd.type_() == ATOM_TYPE_NUMBER
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
    fn mixed_comparisons_keep_integer_precision() {
        let cases = [
            (9007199254740993, 9007199254740992.0, Ordering::Greater),
            (-9007199254740993, -9007199254740992.0, Ordering::Less),
            (i64::MAX, 9223372036854775808.0, Ordering::Less),
            (i64::MIN, -9223372036854775808.0, Ordering::Equal),
            (0, f64::from_bits(1), Ordering::Less),
            (0, -f64::from_bits(1), Ordering::Greater),
            (0, -0.0, Ordering::Equal),
            (1, 1.25, Ordering::Less),
            (-1, -1.25, Ordering::Greater),
            (42, 42.0, Ordering::Equal),
            (i64::MAX, f64::INFINITY, Ordering::Less),
            (i64::MIN, f64::NEG_INFINITY, Ordering::Greater),
        ];
        for (integer, float, order) in cases {
            let a = Number::Integer(integer);
            let b = Number::Float(float);
            assert_eq!(a.partial_cmp(&b), Some(order));
            assert_eq!(b.partial_cmp(&a), Some(order.reverse()));
            assert_eq!(a == b, order == Ordering::Equal);
        }
        assert_eq!(Number::Integer(0).partial_cmp(&Number::Float(f64::NAN)), None);
        assert_ne!(Number::Integer(0), Number::Float(f64::NAN));
    }

    #[test]
    fn mixed_equality_does_not_bridge_distinct_integers() {
        let low = Number::Integer(9007199254740992);
        let rounded = Number::Float(9007199254740992.0);
        let high = Number::Integer(9007199254740993);
        assert_eq!(low, rounded);
        assert_ne!(rounded, high);
        assert_ne!(low, high);
        for a in [&low, &rounded, &high] {
            for b in [&low, &rounded, &high] {
                for c in [&low, &rounded, &high] {
                    if a == b && b == c { assert_eq!(a, c); }
                }
            }
        }
    }
}
