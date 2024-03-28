use crate::*;
use crate::common::collections::ImmutableString;
use crate::matcher::MatchResultIter;

pub const ATOM_TYPE_STRING : Atom = sym!("String");

#[derive(Clone, PartialEq, Debug)]
pub struct Str(ImmutableString);

impl Str {
    pub fn from_string(s: String) -> Self {
        Str(ImmutableString::Allocated(s))
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
}

impl std::fmt::Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}
