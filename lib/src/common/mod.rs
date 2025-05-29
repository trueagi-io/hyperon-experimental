//! Common datastructures used in other modules. Common grounded atoms library.

pub mod collections;
pub mod shared;
pub mod assert;
pub mod reformove;
pub mod multitrie;
pub mod holeyvec;
pub mod owned_or_borrowed;
pub mod vecondemand;

mod flex_ref;
pub use flex_ref::FlexRef;

use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::collections::HashMap;

#[cfg(test)]
pub(crate) mod test_utils;

// GndRefCell is used to keep pointer to the data located on heap as GroundedAtom.
// RefCell itself doesn't implement Display, and forwards PartialEq to internal
// data even when kept type doesn't implement PartialEq. GndRefCell fixes this
// by implementing dummy Display and implementing PartialEq via comparing
// pointers to the data.
#[derive(Clone, Debug)]
pub struct GndRefCell<T>(RefCell<T>);

impl<T> GndRefCell<T> {
    pub const fn new(value: T) -> Self {
        Self(RefCell::new(value))
    }
    pub fn raw(&self) -> &RefCell<T> {
        &self.0
    }
}

impl<T> PartialEq for GndRefCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<T> Display for GndRefCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GndRefCell")
    }
}

#[derive(Clone)]
pub struct CachingMapper<K: Clone + std::hash::Hash + Eq + ?Sized, V: Clone, F: Fn(K) -> V> {
    mapper: F,
    mapping: HashMap<K, V>,
}

impl<K: Clone + std::hash::Hash + Eq + ?Sized, V: Clone, F: Fn(K) -> V> CachingMapper<K, V, F> {
    pub fn new(mapper: F) -> Self {
        Self{ mapper, mapping: HashMap::new() }
    }

    pub fn replace(&mut self, key: K) -> V {
        match self.mapping.get(&key) {
            Some(mapped) => mapped.clone(),
            None => {
                let new_val = (self.mapper)(key.clone());
                self.mapping.insert(key, new_val.clone());
                new_val
            }
        }
    }

    pub fn mapping(&self) -> &HashMap<K, V> {
        &self.mapping
    }

    pub fn mapping_mut(&mut self) -> &mut HashMap<K, V> {
        &mut self.mapping
    }

    pub fn as_fn_mut<'a>(&'a mut self) -> impl 'a + FnMut(K) -> V {
        move |k| { self.replace(k) }
    }
}
