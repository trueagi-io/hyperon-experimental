#[derive(Debug, Clone)]
pub enum ImmutableString {
    Allocated(String),
    Literal(&'static str),
}

impl ImmutableString {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Allocated(name) => name.as_str(),
            Self::Literal(name) => name,
        }
    }
}

impl PartialEq for ImmutableString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for ImmutableString {}

impl std::hash::Hash for ImmutableString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl std::fmt::Display for ImmutableString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<&'static str> for ImmutableString {
    fn from(s: &'static str) -> Self {
        ImmutableString::Literal(s)
    }
}

impl From<String> for ImmutableString {
    fn from(s: String) -> Self {
        ImmutableString::Allocated(s)
    }
}

