//! Space is a storage for atoms with search queries execution capability.
//! This module is intended to keep different space implementations.

pub mod grounding;

use std::rc::Rc;
use std::cell::RefCell;

use crate::atom::Atom;
use crate::atom::matcher::{BindingsSet, apply_bindings_to_atom};

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

/// Read-only space trait.
pub trait Space {
    /// Registers space modifications `observer`. Observer is automatically
    /// deregistered when `Rc` counter reaches zero. See [SpaceObserver] for
    /// examples.
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>);

    /// Executes `query` on the space and returns variable bindings found.
    /// Query may include sub-queries glued by [grounding::COMMA_SYMBOL] symbol. 
    /// Each [Bindings](crate::atom::matcher::Bindings) instance in the returned [BindingsSet]
    /// represents single result.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::{expr, bind_set, sym};
    /// use hyperon::matcher::BindingsSet;
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let space = GroundingSpace::from_vec(vec![expr!("A" "B"), expr!("B" "C")]);
    /// let query = expr!("," ("A" x) (x "C"));
    ///
    /// let result = space.query(&query);
    ///
    /// assert_eq!(result, bind_set![{x: sym!("B")}]);
    /// ```
    fn query(&self, query: &Atom) -> BindingsSet;

    /// Executes `pattern` query on the space and for each result substitutes
    /// variables in `template` by the values from `pattern`. Returns results
    /// of the substitution.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::{expr, assert_eq_no_order};
    /// use hyperon::space::Space;
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let space = GroundingSpace::from_vec(vec![expr!("A" "B"), expr!("A" "C")]);
    ///
    /// let result = space.subst(&expr!("A" x), &expr!("D" x));
    ///
    /// assert_eq_no_order!(result, vec![expr!("D" "B"), expr!("D" "C")]);
    /// ```
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        self.query(pattern).drain(0..)
            .map(| bindings | apply_bindings_to_atom(template, &bindings))
            .collect()
    }
}

/// Mutable space trait.
pub trait SpaceMut {
    /// Adds `atom` into space.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::sym;
    /// use hyperon::space::grounding::GroundingSpace;
    /// use hyperon::atom::matcher::BindingsSet;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    /// 
    /// space.add(sym!("B"));
    ///
    /// assert_eq!(space.query(&sym!("A")), BindingsSet::single());
    /// assert_eq!(space.query(&sym!("B")), BindingsSet::single());
    /// assert_eq!(space.query(&sym!("C")), BindingsSet::empty());
    /// ```
    fn add(&mut self, atom: Atom);

    /// Removes `atom` from space. Returns true if atom was found and removed,
    /// and false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::sym;
    /// use hyperon::matcher::BindingsSet;
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    /// 
    /// space.remove(&sym!("A"));
    ///
    /// assert_eq!(space.query(&sym!("A")), BindingsSet::empty());
    /// ```
    fn remove(&mut self, atom: &Atom) -> bool;

    /// Replaces `from` atom to `to` atom inside space. Doesn't add `to` when
    /// `from` is not found. Returns true if atom was found and replaced, and
    /// false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::sym;
    /// use hyperon::space::grounding::GroundingSpace;
    /// use hyperon::atom::matcher::BindingsSet;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    /// 
    /// space.replace(&sym!("A"), sym!("B"));
    ///
    /// assert_eq!(space.query(&sym!("A")), BindingsSet::empty());
    /// assert_eq!(space.query(&sym!("B")), BindingsSet::single());
    /// ```
    fn replace(&mut self, from: &Atom, to: Atom) -> bool;
}

use crate::common::shared::Shared;

impl<T: Space> Space for Shared<T> {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>) {
        self.borrow().register_observer(observer)
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        self.borrow().query(query)
    }
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        self.borrow().subst(pattern, template)
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
    fn query(&self, query: &Atom) -> BindingsSet {
        T::query(*self, query)
    }
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        T::subst(*self, pattern, template)
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
