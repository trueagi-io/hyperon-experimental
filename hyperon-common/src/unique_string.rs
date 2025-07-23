use bimap::BiMap;
use std::hash::{Hash, Hasher};
use std::sync::{Mutex, LazyLock};
use crate::immutable_string::ImmutableString;

static UNIQUE_STRINGS: LazyLock<Mutex<UniqueStringStorage>> = LazyLock::new(|| {
    Mutex::new(UniqueStringStorage::new())
});

struct UniqueStringStorage {
    next_id: usize,
    strings: BiMap<ImmutableString, usize>,
}

impl UniqueStringStorage {
    fn new() -> Self {
        Self {
            next_id: 0,
            strings: BiMap::new(),
        }
    }

    fn insert(&mut self, key: ImmutableString) -> usize {
        match self.strings.get_by_left(&key) {
            Some(id) => *id,
            None => {
                let id = self.next_id;
                self.next_id += 1;
                self.strings.insert(key, id);
                id
            },
        }
    }

    fn get(&self, id: usize) -> &ImmutableString {
        unsafe {
            self.strings.get_by_right(&id).unwrap_unchecked()
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub enum UniqueString {
    Const(&'static str),
    Store(usize),
}

impl UniqueString {
    pub fn as_str(&self) -> String {
        match self {
            Self::Store(id) => UNIQUE_STRINGS.lock().unwrap().get(*id).as_str().into(),
            Self::Const(s) => (*s).into(),
        }
    }
}

impl std::fmt::Display for UniqueString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl PartialEq for UniqueString {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Store(a), Self::Store(b)) => a == b,
            (Self::Const(a), Self::Const(b)) => a == b,
            _ => self.as_str() == other.as_str(),
        }
    }
}

impl Hash for UniqueString {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self {
            Self::Const(s) => s.hash(state),
            Self::Store(id) => id.hash(state),
        }
    }
}

impl From<ImmutableString> for UniqueString {
    fn from(value: ImmutableString) -> Self {
        Self::Store(UNIQUE_STRINGS.lock().unwrap().insert(value))
    }
}

impl<I: Into<String>> From<I> for UniqueString {
    fn from(value: I) -> Self {
        let s: String = value.into();
        Self::Store(UNIQUE_STRINGS.lock().unwrap().insert(s.into()))
    }
}
