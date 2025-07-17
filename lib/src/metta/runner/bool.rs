use hyperon_atom::*;
use hyperon_atom::serial;
use hyperon_atom::ConvertingSerializer;

use std::fmt::Display;

pub const ATOM_TYPE_BOOL : Atom = sym!("Bool");

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

    pub fn from_atom(atom: &Atom) -> Option<Self> {
        BoolSerializer::convert(atom)
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

impl ConvertingSerializer<Bool> for BoolSerializer {
    fn into_type(self) -> Option<Bool> {
        self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bool() {
        assert_eq!(Bool::from_str("True"), Bool(true));
        assert_eq!(Bool::from_str("False"), Bool(false));
        assert_eq!(format!("{}", Bool(true)), "True");
        assert_eq!(format!("{}", Bool(false)), "False");
    }
}
