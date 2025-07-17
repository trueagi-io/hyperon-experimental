use hyperon_atom::*;
use hyperon_common::collections::ImmutableString;
use hyperon_atom::serial;
use hyperon_atom::ConvertingSerializer;
use unescaper;

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
        write!(f, "{:?}", self.0.as_str())
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

impl ConvertingSerializer<Str> for StrSerializer {
    fn check_type(gnd: &dyn GroundedAtom) -> bool {
        gnd.type_() == ATOM_TYPE_STRING
    }

    fn into_type(self) -> Option<Str> {
        self.value
    }
}

pub fn atom_to_string(atom: &Atom) -> String {
    match atom {
        Atom::Grounded(gnd) if gnd.type_() == ATOM_TYPE_STRING =>
            Str::from_atom(atom).unwrap().as_str().into(),
        _ => atom.to_string(),
    }
}

pub fn unescape(str: &str) -> unescaper::Result<String> {
    unescaper::unescape(str).map(|mut s| {
        s.remove(0);
        s.pop();
        s
    })
}

pub(crate) fn expect_string_like_atom(atom: &Atom) -> Option<String> {
    match atom {
        Atom::Symbol(_) | Atom::Grounded(_) => Some(atom_to_string(atom)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn str_display_escape() {
        let s = Str::from_str("\\ \" \' \n \r \t \x1b abc");
        assert_eq!(r#""\\ \" ' \n \r \t \u{1b} abc""#, s.to_string());
    }

    #[test]
    fn str_unescape() {
        let s = unescape(r#""\\ \" ' \n \r \t \u{1b} abc""#);
        assert_eq!("\\ \" \' \n \r \t \x1b abc", s.unwrap());
    }

    #[test]
    fn test_atom_to_string() {
        let atom = Atom::gnd(Str::from_str("A\nB"));
        assert_eq!("A\nB", atom_to_string(&atom));
        let atom = Atom::sym(r#""AB""#);
        assert_eq!(r#""AB""#, atom_to_string(&atom));
    }
}
