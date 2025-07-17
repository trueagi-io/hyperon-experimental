use hyperon_atom::*;
use hyperon_atom::serial;
use hyperon_atom::ConvertingSerializer;

use std::fmt::Display;

pub const ATOM_TYPE_NUMBER : Atom = sym!("Number");

#[derive(Clone, Debug)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

impl PartialEq<Self> for Number {
    fn eq(&self, other: &Self) -> bool {
        // TODO: this promoting is helpful for the atoms which contain Number
        // objects, but it breaks logic for Rust data structures which use
        // Number. For example Map can mix up Float and Number because of
        // promoting. Possible solution is to have separate equality
        // implementation for the Grounded trait.
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

    pub fn from_atom(atom: &Atom) -> Option<Self> {
        NumberSerializer::convert(atom)
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
}
