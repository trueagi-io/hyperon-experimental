use std::fmt::Display;

pub trait Equality<T> {
    fn eq(a: &T, b: &T) -> bool;
}

#[derive(Debug)]
pub struct DefaultEquality {}

impl<T: PartialEq> Equality<T> for DefaultEquality {
    fn eq(a: &T, b: &T) -> bool {
        a == b
    }
}

#[derive(Clone, Debug)]
pub struct ListMap<K, V, E: Equality<K> = DefaultEquality>
{
    list: Vec<(K, V)>,
    _phantom: std::marker::PhantomData<E>,
}

pub enum ListMapEntry<'a, K, V, E: Equality<K>> {
    Occupied(K, &'a mut ListMap<K, V, E>),
    Vacant(K, &'a mut ListMap<K, V, E>),
}

impl<'a, K, V, E: Equality<K>> ListMapEntry<'a, K, V, E> {
    pub fn or_default(self) -> &'a mut V where V: Default {
        self.or_insert(Default::default())
    }

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

type ListMapIntoIter<K, V> = std::vec::IntoIter<(K, V)>;

macro_rules! list_map_get {
    ($get:ident, {$( $mut_:tt )?}) => {
        pub fn $get(& $( $mut_ )? self, key: &K) -> Option<& $( $mut_ )? V> {
            for (k, v) in & $( $mut_ )? self.list {
                if E::eq(k, key) {
                    return Some(v)
                }
            }
            None
        }
    }
}

impl<K, V, E: Equality<K>> ListMap<K, V, E> {
    pub fn new() -> Self {
        Self{ list: vec![], _phantom: std::marker::PhantomData }
    }

    pub fn entry<'a>(&'a mut self, key: K) -> ListMapEntry<'a, K, V, E> {
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

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
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

    pub fn into_iter(self) -> ListMapIntoIter<K, V> {
        self.list.into_iter()
    }
}

impl<K, V: PartialEq, E: Equality<K>> PartialEq for ListMap<K, V, E> {
    fn eq(&self, other: &Self) -> bool {
        fn left_includes_right<K, V: PartialEq, E: Equality<K>>(left: &ListMap<K, V, E>, right: &ListMap<K, V, E>) -> bool {
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

impl<K: PartialEq, V> From<Vec<(K, V)>> for ListMap<K, V> {
    fn from(list: Vec<(K, V)>) -> Self {
        let mut map = ListMap::new();
        for (k, v) in list.into_iter() {
            map.insert(k, v)
        }
        map
    }
}

impl<K, V, E: Equality<K>> FromIterator<(K, V)> for ListMap<K, V, E> {
    fn from_iter<T>(iter: T) -> Self where T: IntoIterator<Item = (K, V)> {
        let mut map = ListMap::new();
        iter.into_iter().for_each(|(k, v)| map.insert(k, v));
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

#[derive(Debug, Clone)]
pub enum CowArray<T: 'static> {
    Allocated(Box<[T]>),
    Literal(&'static [T]),
}

impl<T: 'static> CowArray<T> {

    pub fn new() -> Self {
        Self::Literal(&[])
    }

    pub fn as_slice(&self) -> &[T] {
        match self {
            Self::Allocated(array) => &*array,
            Self::Literal(array) => array,
        }
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] where T: Clone {
        match self {
            Self::Allocated(array) => &mut *array,
            Self::Literal(array) => {
                *self = Self::Allocated((*array).into());
                self.as_slice_mut()
            }
        }
    }

    pub fn len(&self) -> usize {
        self.as_slice().len()
    }

    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.as_slice().iter()
    }
}

impl<T: PartialEq> PartialEq for CowArray<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<T: Eq> Eq for CowArray<T> {}

impl<T: Display> Display for CowArray<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")
            .and_then(|_| self.as_slice().iter().take(1).fold(Ok(()),
                |res, atom| res.and_then(|_| write!(f, "{}", atom))))
            .and_then(|_| self.as_slice().iter().skip(1).fold(Ok(()),
                |res, atom| res.and_then(|_| write!(f, " {}", atom))))
            .and_then(|_| write!(f, "]"))
    }
}

impl<T: 'static> From<&'static [T]> for CowArray<T> {
    fn from(a: &'static [T]) -> Self {
        CowArray::Literal(a)
    }
}

impl<T, const N: usize> From<[T; N]> for CowArray<T> {
    fn from(a: [T; N]) -> Self {
        CowArray::Allocated(Box::new(a))
    }
}

impl<T> From<Vec<T>> for CowArray<T> {
    fn from(v: Vec<T>) -> Self {
        CowArray::Allocated(v.into_boxed_slice())
    }
}

impl<T: Clone> Into<Vec<T>> for CowArray<T> {
    fn into(self) -> Vec<T> {
        match self {
            Self::Allocated(array) => array.into(),
            Self::Literal(array) => array.into(),
        }
    }
}

impl<'a, T> Into<&'a [T]> for &'a CowArray<T> {
    fn into(self) -> &'a [T] {
        self.as_slice()
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
