use std::fmt::Display;

#[derive(Clone, Debug)]
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

macro_rules! list_map_iterator {
    ( $ListMapIter:ident, $Iter:ident, {$( $mut_:tt )?} ) => {
        pub struct $ListMapIter<'a, K, V> {
            delegate: std::slice::$Iter<'a, (K, V)>,
        }

        impl<'a, K, V> Iterator for $ListMapIter<'a, K, V> {
            type Item = (&'a K, &'a $( $mut_ )? V);
            fn next(&mut self) -> Option<Self::Item> {
                match self.delegate.next() {
                    Some((key, value)) => Some((key, value)),
                    None => None,
                }
            }
        }
    }
}

list_map_iterator!(ListMapIter, Iter, {});
list_map_iterator!(ListMapIterMut, IterMut, { mut });

macro_rules! list_map_get {
    ($get:ident, {$( $mut_:tt )?}) => {
        pub fn $get(& $( $mut_ )? self, key: &K) -> Option<& $( $mut_ )? V> {
            for (k, v) in & $( $mut_ )? self.list {
                if *k == *key {
                    return Some(v)
                }
            }
            None
        }
    }
}

impl<K: PartialEq, V> ListMap<K, V> {
    pub fn new() -> Self {
        Self{ list: vec![] }
    }

    pub fn entry<'a>(&'a mut self, key: K) -> ListMapEntry<'a, K, V> {
        match self.get_mut(&key) {
            Some(_) => ListMapEntry::Occupied(key, self),
            None => ListMapEntry::Vacant(key, self)
        }
    }

    list_map_get!(get, {});
    list_map_get!(get_mut, {mut});

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

    pub fn iter_mut(&mut self) -> ListMapIterMut<'_, K, V> {
        ListMapIterMut{ delegate: self.list.iter_mut() }
    }
}

impl<K: PartialEq, V: PartialEq> PartialEq for ListMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        fn left_includes_right<K: PartialEq, V: PartialEq>(left: &ListMap<K, V>, right: &ListMap<K, V>) -> bool {
            for e in right.iter() {
                if left.get(e.0) != Some(e.1) {
                    return false;
                }
            }
            true
        }
        left_includes_right(self, other) && left_includes_right(other, self)
    }
}

impl<K: PartialEq, V: PartialEq> From<Vec<(K, V)>> for ListMap<K, V> {
    fn from(list: Vec<(K, V)>) -> Self {
        let mut map = ListMap::new();
        for (k, v) in list.into_iter() {
            map.insert(k, v)
        }
        map
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn list_map_eq() {
        let a = ListMap::from(vec![("A", 1), ("B", 2)]);
        let b = ListMap::from(vec![("B", 2), ("A", 1)]);
        let c = ListMap::from(vec![("B", 1), ("A", 2)]);

        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_ne!(b, c);
    }
}
