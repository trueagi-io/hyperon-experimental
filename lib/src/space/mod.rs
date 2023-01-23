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
    fn query(&self, query: &Atom) -> Vec<Bindings>;
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom>;
    fn iter(&self) -> SpaceIter;
}

/// Space trait.
pub trait SpaceMut {
    fn add(&mut self, atom: Atom);
    fn remove(&mut self, atom: &Atom) -> bool;
    fn replace(&mut self, from: &Atom, to: Atom) -> bool;
}

use crate::common::shared::Shared;

impl<T: Space> Space for Shared<T> {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>) {
        self.borrow().register_observer(observer)
    }
    fn query(&self, query: &Atom) -> Vec<Bindings> {
        self.borrow().query(query)
    }
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        self.borrow().subst(pattern, template)
    }
    fn iter(&self) -> SpaceIter {
        todo!("Not implemented")
    }
}

impl<T: SpaceMut> SpaceMut for Shared<T> {
    fn add(&mut self, atom: Atom) {
        self.borrow_mut().add(atom)
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        self.borrow_mut().remove(atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        self.borrow_mut().replace(from, to)
    }
}


impl<T: Space> Space for &T {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>) {
        T::register_observer(*self, observer)
    }
    fn query(&self, query: &Atom) -> Vec<Bindings> {
        T::query(*self, query)
    }
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        T::subst(*self, pattern, template)
    }
    fn iter(&self) -> SpaceIter {
        todo!("Not implemented")
    }
}

impl<T: SpaceMut> SpaceMut for &mut T {
    fn add(&mut self, atom: Atom) {
        (*self).add(atom)
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        (*self).remove(atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        (*self).replace(from, to)
    }
}
