use std::sync::Arc;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::hash::{Hash, Hasher};
use std::sync::{Mutex, LazyLock};
use crate::immutable_string::ImmutableString;

static UNIQUE_STRINGS: LazyLock<Mutex<UniqueStringStorage>> = LazyLock::new(|| {
    Mutex::new(UniqueStringStorage::new())
});

struct UniqueStringStorage {
    strings: HashMap<Arc<ImmutableString>, ()>,
}

impl UniqueStringStorage {
    fn new() -> Self {
        Self {
            strings: HashMap::new(),
        }
    }

    fn insert(&mut self, key: ImmutableString) -> Arc<ImmutableString> {
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

    fn remove(&mut self, key: &Arc<ImmutableString>) {
        self.strings.remove(key);
    }
}

#[allow(private_interfaces)]
#[derive(Debug, Clone, Eq)]
pub enum UniqueString {
    Const(&'static str),
    Store(Arc<ImmutableString>),
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
            Self::Store(rc) => state.write_usize(Arc::as_ptr(rc) as usize),
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

#[cfg(test)]
mod test {
    use super::*;
    use std::hint::black_box;

    #[test]
    fn unique_string_drop() {
        {
            let a = UniqueString::from("test string");
            let b = UniqueString::from("test string");
            let count = UNIQUE_STRINGS.lock().unwrap().strings.len();
            assert_eq!(count, 1);
            black_box(a);
            black_box(b);
        }
        let count = UNIQUE_STRINGS.lock().unwrap().strings.len();
        assert_eq!(count, 0);
    }

    #[test]
    fn unique_string_equal() {
        let store_a = UniqueString::from("test string");
        let store_b = UniqueString::from("test string");
        let const_c = UniqueString::Const("test string");
        let const_d = UniqueString::Const("test string");
        assert_eq!(store_a, store_b);
        assert_eq!(store_a, const_c);
        assert_eq!(const_c, store_a);
        assert_eq!(const_c, const_d);
    }

    #[test]
    fn unique_string_from() {
        let store_a = UniqueString::from("test string");
        assert!(matches!(store_a, UniqueString::Store(_)));
        let store_b = UniqueString::from(String::from("test string"));
        assert!(matches!(store_b, UniqueString::Store(_)));
    }
}
