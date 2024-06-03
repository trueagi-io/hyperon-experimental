use crate::*;
use crate::common::collections::ImmutableString;
use crate::matcher::MatchResultIter;
use crate::serial;

pub const ATOM_TYPE_STRING : Atom = sym!("String");

#[derive(Clone, PartialEq, Debug)]
pub struct Str(ImmutableString);

impl Str {
    pub fn from_str(s: &'static str) -> Self {
        Str(ImmutableString::Literal(s))
    }
    pub fn from_string(s: String) -> Self {
        Str(ImmutableString::Allocated(s))
    }
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Grounded for Str {
    fn type_(&self) -> Atom {
        ATOM_TYPE_STRING
    }

    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        execute_not_executable(self)
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }

    fn serialize(&self, serializer: &mut dyn serial::Serializer) -> serial::Result {
        serializer.serialize_str(self.as_str())
    }
}

impl std::fmt::Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}
