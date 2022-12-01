use std::fmt::Display;

#[derive(Clone)]
pub struct ListMap<K, V> {
    list: Vec<(K, V)>,
}

pub enum ListMapEntry<'a, K, V> {
    Occupied(K, &'a mut ListMap<K, V>),
    Vacant(K, &'a mut ListMap<K, V>),
}

impl<'a, K: PartialEq, V> ListMapEntry<'a, K, V> {
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            ListMapEntry::Occupied(key, map) => map.get_mut(&key).unwrap(),
            ListMapEntry::Vacant(key, map) => map.insert_entry(key, default),
        }
    }
}

pub struct ListMapIter<'a, K, V> {
    delegate: std::slice::Iter<'a, (K, V)>,
}

impl<'a, K, V> Iterator for ListMapIter<'a, K, V> {
    type Item = (&'a K, &'a V);
    fn next(&mut self) -> Option<Self::Item> {
        match self.delegate.next() {
            Some((key, value)) => Some((key, value)),
            None => None,
        }
    }
}

impl<K: PartialEq, V> ListMap<K, V> {
    pub fn new() -> Self {
        Self{ list: vec![] }
    }

    pub fn entry<'a>(&'a mut self, key: K) -> ListMapEntry<'a, K, V> {
        for (k, _) in &mut self.list {
            if *k == key {
                return ListMapEntry::Occupied(key, self);
            }
        }
        ListMapEntry::Vacant(key, self)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        // FIXME: merge loops here and in entry and possible in get_mut?
        for (k, v) in &self.list {
            if *k == *key {
                return Some(&v)
            }
        }
        None
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        for (k, v) in &mut self.list {
            if *k == *key {
                return Some(v)
            }
        }
        None
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.insert_entry(key, value);
    }

    fn insert_entry(&mut self, key: K, value: V) -> &mut V {
        self.list.push((key, value));
        &mut self.list.last_mut().unwrap().1
    }

    pub fn clear(&mut self) {
        self.list.clear()
    }

    pub fn iter(&self) -> ListMapIter<'_, K, V> {
        ListMapIter{ delegate: self.list.iter() }
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
