use crate::*;
use crate::common::collections::ImmutableString;
use crate::serial;
use crate::atom::serial::ConvertingSerializer;

/// String type
pub const ATOM_TYPE_STRING : Atom = sym!("String");

/// Grounded Rust string representation
#[derive(Clone, PartialEq, Debug)]
pub struct Str(ImmutableString);

impl Str {
    /// Construct new instance from string literal
    pub fn from_str(s: &'static str) -> Self {
        Str(ImmutableString::Literal(s))
    }
    /// Construct new instance from owned string
    pub fn from_string(s: String) -> Self {
        Str(ImmutableString::Allocated(s))
    }
    /// Return reference to string slice
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
    /// Try to convert an atom into `Str` instance
    pub fn from_atom(atom: &Atom) -> Option<Self> {
        StrSerializer::convert(atom)
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

    fn serialize(&self, serializer: &mut dyn serial::Serializer) -> serial::Result {
        serializer.serialize_str(self.as_str())
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

#[derive(Default)]
struct StrSerializer {
    value: Option<Str>,
}

impl serial::Serializer for StrSerializer {
    fn serialize_str(&mut self, v: &str) -> serial::Result {
        self.value = Some(Str::from_string(v.into()));
        Ok(())
    }
}

impl serial::ConvertingSerializer<Str> for StrSerializer {
    fn as_mut(&mut self) -> &mut dyn serial::Serializer {
        self
    }
    fn into_type(self) -> Option<Str> {
        self.value
    }
}
