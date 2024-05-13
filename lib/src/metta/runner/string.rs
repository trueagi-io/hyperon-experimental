use crate::*;
use crate::common::collections::ImmutableString;
use crate::matcher::MatchResultIter;

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

impl AsRef<str> for Str {
    fn as_ref(&self) -> &str {
        self.as_str()
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

/// A utility function to return the part of a string in between starting and ending quotes
pub fn strip_quotes(src: &str) -> &str {
    if let Some(first) = src.chars().next() {
        if first == '"' {
            if let Some(last) = src.chars().last() {
                if last == '"' {
                    if src.len() > 1 {
                        return &src[1..src.len()-1]
                    }
                }
            }
        }
    }
    src
}
