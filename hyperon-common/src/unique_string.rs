use std::sync::Arc;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::hash::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::{Mutex, LazyLock};
use crate::immutable_string::ImmutableString;

static UNIQUE_STRINGS: LazyLock<Mutex<UniqueStringStorage>> = LazyLock::new(|| {
    Mutex::new(UniqueStringStorage::new())
});

struct UniqueStringStorage {
    strings: HashMap<Arc<UniqueStringInternal>, ()>,
}

impl UniqueStringStorage {
    fn new() -> Self {
        Self {
            strings: HashMap::new(),
        }
    }

    fn insert(&mut self, key: ImmutableString) -> Arc<UniqueStringInternal> {
        let key = Arc::new(key.into());
        match self.strings.entry(key) {
            Entry::Occupied(o) => o.key().clone(),
            Entry::Vacant(v) => {
                let key = v.key().clone();
                v.insert(());
                key
            }
        }
    }

    fn remove(&mut self, key: &Arc<UniqueStringInternal>) {
        self.strings.remove(key);
    }
}

#[derive(Eq, Debug)]
struct UniqueStringInternal {
    string: ImmutableString,
    hash: u64,
}

impl UniqueStringInternal {
    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }
}

impl PartialEq for UniqueStringInternal {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Hash for UniqueStringInternal {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        state.write_u64(self.hash)
    }
}

impl From<ImmutableString> for UniqueStringInternal {
    fn from(value: ImmutableString) -> Self {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        Self {
            string: value,
            hash: hasher.finish(),
        }
    }
}

#[allow(private_interfaces)]
#[derive(Debug, Clone, Eq)]
pub enum UniqueString {
    Const(&'static str),
    Store(Arc<UniqueStringInternal>),
}

impl UniqueString {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Store(rc) => rc.as_str(),
            Self::Const(s) => s,
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
            (Self::Store(a), Self::Store(b)) => Arc::as_ptr(a) == Arc::as_ptr(b),
            (Self::Const(a), Self::Const(b)) => *a == *b,
            _ => self.as_str() == other.as_str(),
        }
    }
}

impl Hash for UniqueString {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self {
            Self::Const(s) => s.hash(state),
            Self::Store(rc) => state.write_u64(rc.hash),
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

impl Drop for UniqueString {
    fn drop(&mut self) {
        match self {
            Self::Store(rc) => {
                let lock = UNIQUE_STRINGS.lock();
                // one instance is inside the storage and second one is inside self
                if Arc::strong_count(rc) == 2 {
                    lock.unwrap().remove(rc);
                }
            },
            Self::Const(_) => {},
        }
    }
}

