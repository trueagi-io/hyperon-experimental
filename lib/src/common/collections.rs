use std::fmt::Display;

#[derive(Clone)]
pub struct ListMap<K, V> {
    list: Vec<(K, V)>,
}

impl<K: PartialEq, V> ListMap<K, V> {
    pub fn new() -> Self {
        Self{ list: vec![] }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        for (k, v) in &self.list {
            if *k == *key {
                return Some(&v)
            }
        }
        None
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.list.push((key, value))
    }

    pub fn clear(&mut self) {
        self.list.clear()
    }
}

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

impl Display for ImmutableString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
