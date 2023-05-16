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
    pub fn new<I: Iterator<Item=&'a Atom> + 'a>(iter: I) -> Self {
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

    /// Returns the number of Atoms in the space, or None if this can't be determined
    fn atom_count(&self) -> Option<usize>;

    /// Returns an iterator over every atom in the space, or None if that is not possible
    fn atom_iter(&self) -> Option<SpaceIter>;

    /// Returns an &dyn [Any] for spaces where this is possible
    fn as_any(&self) -> Option<&dyn std::any::Any>;
}

/// Mutable space trait.
pub trait SpaceMut: Space {
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

    /// Turn a &dyn SpaceMut into an &dyn Space.  Obsolete when Trait Upcasting is stabilized.
    /// https://github.com/rust-lang/rust/issues/65991  Any month now.
    fn as_space(&self) -> &dyn Space;
}

use crate::common::shared::Shared;

impl Space for Box<dyn SpaceMut> {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>) {
        (**self).register_observer(observer)
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        (**self).query(query)
    }
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        (**self).subst(pattern, template)
    }
    fn atom_count(&self) -> Option<usize> {
        (**self).atom_count()
    }
    fn atom_iter(&self) -> Option<SpaceIter> {
        (**self).atom_iter()
    }
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        (*self).as_space().as_any()
    }
}

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
    fn atom_count(&self) -> Option<usize> {
        self.borrow().atom_count()
    }
    fn atom_iter(&self) -> Option<SpaceIter> {
        None
    }
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        None
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
    fn as_space(&self) -> &dyn Space {
        self
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
    fn atom_count(&self) -> Option<usize> {
        T::atom_count(*self)
    }
    fn atom_iter(&self) -> Option<SpaceIter> {
        T::atom_iter(*self)
    }
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        None
    }
}

impl<T: Space> Space for &mut T {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>) {
        T::register_observer(*self, observer)
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        T::query(*self, query)
    }
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        T::subst(*self, pattern, template)
    }
    fn atom_count(&self) -> Option<usize> {
        T::atom_count(*self)
    }
    fn atom_iter(&self) -> Option<SpaceIter> {
        T::atom_iter(*self)
    }
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        None
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
    fn as_space(&self) -> &dyn Space {
        self
    }
}
