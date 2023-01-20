//! Space is a storage for atoms with search queries execution capability.
//! This module is intended to keep different space implementations.

pub mod grounding;

use std::rc::Rc;
use std::cell::RefCell;

use crate::atom::Atom;
use crate::atom::matcher::Bindings;

/// Contains information about space modification event.
#[derive(Clone, Debug, PartialEq)]
pub enum SpaceEvent {
    /// Atom is added into a space.
    Add(Atom),
    /// Atom is removed from space.
    Remove(Atom),
    /// First atom is replaced by the second one.
    Replace(Atom, Atom),
}

/// Space modification event observer trait.
///
/// # Examples
///
/// ```
/// use hyperon::sym;
/// use hyperon::space::*;
/// use hyperon::space::grounding::*;
/// use std::rc::Rc;
/// use std::cell::RefCell;
///
/// struct MyObserver {
///     events: Vec<SpaceEvent>
/// }
///
/// impl SpaceObserver for MyObserver {
///     fn notify(&mut self, event: &SpaceEvent) {
///         self.events.push(event.clone());
///     }
/// }
///
/// let mut space = GroundingSpace::new();
/// let observer = Rc::new(RefCell::new(MyObserver{ events: Vec::new() }));
///
/// space.register_observer(observer.clone());
/// space.add(sym!("A"));
/// space.replace(&sym!("A"), sym!("B"));
/// space.remove(&sym!("B"));
///
/// assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("A")),
///     SpaceEvent::Replace(sym!("A"), sym!("B")),
///     SpaceEvent::Remove(sym!("B"))]);
/// ```
pub trait SpaceObserver {
    /// Notifies about space modification.
    fn notify(&mut self, event: &SpaceEvent);
}

/// Space iterator.
pub struct SpaceIter<'a> {
    iter: Box<dyn Iterator<Item=&'a Atom> + 'a>
}

impl<'a> SpaceIter<'a> {
    fn new<I: Iterator<Item=&'a Atom> + 'a>(iter: I) -> Self {
        Self{ iter: Box::new(iter) }
    }
}

impl<'a> Iterator for SpaceIter<'a> {
    type Item = &'a Atom;

    fn next(&mut self) -> Option<&'a Atom> {
        self.iter.next()
    }
}

/// Space trait.
pub trait Space {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>);
    fn add(&mut self, atom: Atom);
    fn remove(&mut self, atom: &Atom) -> bool;
    fn replace(&mut self, from: &Atom, to: Atom) -> bool;
    fn query(&self, query: &Atom) -> Vec<Bindings>;
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom>;
    fn iter(&self) -> SpaceIter;
}
