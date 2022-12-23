//! Simple implementation of space which uses in-memory vector of atoms as
//! an underlying storage.

use crate::*;
use crate::atom::*;
use crate::atom::matcher::{Bindings, Unifications, match_atoms};
use crate::atom::subexpr::split_expr;
use crate::matcher::MatchResultIter;

use std::fmt::{Display, Debug};
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashSet;
use std::collections::BTreeSet;
use std::collections::HashMap;

// Grounding space

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
enum IndexKey {
    Symbol(SymbolAtom),
    Wildcard,
    Expression(usize),
    ExpressionBegin,
    ExpressionEnd,
}

impl IndexKey {
    fn keys_from_atom(atom: &Atom) -> Vec<IndexKey> {
        match atom {
            Atom::Symbol(sym) => vec![IndexKey::Symbol(sym.clone())],
            Atom::Expression(expr) => {
                let mut keys = Vec::new();
                let mut expr_len = 0usize;

                expr_len += 1;
                keys.push(IndexKey::ExpressionEnd);

                for child in expr.children().iter().rev() {
                    let mut children = IndexKey::keys_from_atom(child);
                    expr_len += children.len();
                    keys.append(&mut children);
                }

                keys.push(IndexKey::Expression(expr_len));
                keys
            },
            // TODO: At the moment all grounding symbols are matched as wildcards
            // because they potentially may have custom Grounded::match_()
            // implementation and we cannot understand it from data. We could improve
            // speed of extracting grounded values from the index if GroundedAtom
            // has a flag which says whether match_() is match_by_equality() or
            // not. GroundedAtom with match_by_equality() implementation can be
            // added as separate IndexKey::GroundedValue to navigate through
            // the index quickly. GroundedAtom with custom match_() will be added
            // as a wildcard to be matched after search in index. It also requires
            // implementing Hash on Grounded.
            _ => vec![IndexKey::Wildcard],
        }
    }
}

#[derive(Clone)]
struct IndexTree<T> {
    next: HashMap<IndexKey, Box<IndexTree<T>>>,
    leaf: Vec<T>,
}

macro_rules! walk_index_tree {
    ( $IndexTreeIter:ident, {$( $mut_:tt )?}, $raw_mut:tt ) => {
        struct $IndexTreeIter<'a, T, F>
            where F: Fn(&'a $( $mut_ )? IndexTree<T>, IndexKey, Vec<IndexKey>, &mut dyn FnMut(*$raw_mut IndexTree<T>, Vec<IndexKey>))
        {
            queue: Vec<(* $raw_mut IndexTree<T>, Vec<IndexKey>)>,
            next_op: F,
            _marker: std::marker::PhantomData<&'a $( $mut_ )? IndexTree<T>>,
        }

        impl<'a, T, F> $IndexTreeIter<'a, T, F>
            where F: Fn(&'a $( $mut_ )? IndexTree<T>, IndexKey, Vec<IndexKey>, &mut dyn FnMut(*$raw_mut IndexTree<T>, Vec<IndexKey>))
        {
            fn new(idx: &'a $( $mut_ )? IndexTree<T>, atom: &Atom, next_op: F) -> Self {
                let idx: * $raw_mut IndexTree<T> = idx;
                let mut queue = Vec::new();

                queue.push((idx, IndexKey::keys_from_atom(&atom)));

                Self{ queue: queue, next_op, _marker: std::marker::PhantomData }
            }

            fn call_next(&mut self, idx: * $raw_mut IndexTree<T>, key: IndexKey, keys: Vec<IndexKey>) {
                let queue = &mut self.queue;
                (self.next_op)(unsafe{ & $( $mut_ )? *idx}, key, keys, &mut |index, keys| queue.push((index, keys)));
            }
        }

        impl<'a, T, F> Iterator for $IndexTreeIter<'a, T, F>
            where F: Fn(&'a $( $mut_ )? IndexTree<T>, IndexKey, Vec<IndexKey>, &mut dyn FnMut(*$raw_mut IndexTree<T>, Vec<IndexKey>))
        {
            type Item = &'a $( $mut_ )? IndexTree<T>;

            fn next(&mut self) -> Option<Self::Item> {
                while let Some((idx, mut keys)) = self.queue.pop() {
                    match keys.pop() {
                        None => return Some(unsafe{ & $( $mut_ )? *idx }),
                        Some(key) => self.call_next(idx, key, keys),
                    }
                }
                None
            }
        }
    }
}

walk_index_tree!(IndexTreeIterMut, { mut }, mut);
walk_index_tree!(IndexTreeIter, { /* no mut */ }, const);

impl<T: Debug + PartialEq + Clone> IndexTree<T> {

    fn new() -> Self {
        Self{ next: HashMap::new(), leaf: Vec::new() }
    }

    fn next_get_or_insert(&mut self, key: IndexKey) -> &mut IndexTree<T> {
        self.next.entry(key).or_insert(Box::new(IndexTree::new()))
    }

    fn next_get(&self, key: &IndexKey) -> Option<&IndexTree<T>> {
        self.next.get(key).map(Box::as_ref)
    }

    fn next_get_mut(&mut self, key: &IndexKey) -> Option<&mut IndexTree<T>> {
        self.next.get_mut(key).map(Box::as_mut)
    }

    fn next_iter<'a>(&'a self, filter: &'a dyn Fn(&IndexKey)->bool) -> impl Iterator<Item=&'a IndexTree<T>> + 'a {
        self.next.iter().filter_map(move |(key, idx)| {
            match filter(key) {
                true => Some(idx.as_ref()),
                false => None,
            }
        })
    }

    fn next_iter_mut<'a>(&'a mut self, filter: &'a dyn Fn(&IndexKey)->bool) -> impl Iterator<Item=&'a mut IndexTree<T>> + 'a {
        self.next.iter_mut().filter_map(move |(key, idx)| {
            match filter(key) {
                true => Some(idx.as_mut()),
                false => None,
            }
        })
    }

    fn next_for_add<'a>(&'a mut self, key: IndexKey, keys: Vec<IndexKey>,
            callback: &mut dyn FnMut(*mut IndexTree<T>, Vec<IndexKey>)) {
        if let IndexKey::Expression(expr_len) = key {
            let wildmatch_idx = self.next_get_or_insert(key);
            let tail = &keys.as_slice()[..(keys.len() - expr_len)];
            callback(wildmatch_idx, tail.to_vec());

            let full_idx = self.next_get_or_insert(IndexKey::ExpressionBegin);
            callback(full_idx, keys);
        } else {
            let idx = self.next_get_or_insert(key);
            callback(idx, keys)
        }
    }

    fn next_for_remove<'a>(&'a mut self, key: IndexKey, keys: Vec<IndexKey>,
            callback: &mut dyn FnMut(*mut IndexTree<T>, Vec<IndexKey>)) {
        match key {
            IndexKey::Symbol(_) => {
                self.next_get_mut(&key).map_or((), |idx| callback(idx, keys.clone()));
                self.next_get_mut(&IndexKey::Wildcard).map_or((), |idx| callback(idx, keys));
            },
            IndexKey::ExpressionEnd =>
                self.next_get_mut(&key).map_or((), |idx| callback(idx, keys)),
            IndexKey::Expression(expr_len) => {
                let len = keys.len() - expr_len;
                let tail = &keys.as_slice()[..len];
                self.next_get_mut(&IndexKey::Wildcard).map_or((), |idx| callback(idx, tail.to_vec()));
                self.next_get_mut(&key).map_or((), |idx| callback(idx, tail.to_vec()));
                self.next_get_mut(&IndexKey::ExpressionBegin).map_or((), |idx| callback(idx, keys));
            },
            IndexKey::Wildcard => self.next_iter_mut(&|key|
                *key != IndexKey::ExpressionEnd && *key != IndexKey::ExpressionBegin)
                .for_each(|idx| callback(idx, keys.clone())),
            IndexKey::ExpressionBegin => panic!("Should not be included into a key from atom"),
        }
    }
    
    fn remove_value(&mut self, value: &T) -> bool {
        match self.leaf.iter().position(|other| *other == *value) {
            Some(position) => {
                self.leaf.remove(position);
                true
            },
            None => false,
        }
    }

    fn next_for_get<'a>(&'a self, key: IndexKey, keys: Vec<IndexKey>,
            callback: &mut dyn FnMut(*const IndexTree<T>, Vec<IndexKey>)) {
        match key {
            IndexKey::Symbol(_) => {
                self.next_get(&key).map_or((), |idx| callback(idx, keys.clone()));
                self.next_get(&IndexKey::Wildcard).map_or((), |idx| callback(idx, keys));
            },
            IndexKey::ExpressionEnd =>
                self.next_get(&key).map_or((), |idx| callback(idx, keys)),
            IndexKey::Expression(expr_len) => {
                let len = keys.len() - expr_len;
                let tail = &keys.as_slice()[..len];
                self.next_get(&IndexKey::Wildcard).map_or((), |idx| callback(idx, tail.to_vec()));
                self.next_get(&IndexKey::ExpressionBegin).map_or((), |idx| callback(idx, keys));
            },
            IndexKey::Wildcard => self.next_iter(&|key|
                *key != IndexKey::ExpressionEnd && *key != IndexKey::ExpressionBegin)
                .for_each(|idx| callback(idx, keys.clone())),
            IndexKey::ExpressionBegin => panic!("Should not be included into a key from atom"),
        }
    }
    
    fn add(&mut self, key: &Atom, value: T) {
        log::debug!("IndexTree::add(): key: {:?}, value: {:?}", key, value);
        IndexTreeIterMut::new(self, key, |idx, key, keys, callback| {
            idx.next_for_add(key, keys, callback)
        }).for_each(|idx| idx.leaf.push(value.clone()));
    }

    // TODO: at the moment the method doesn't remove the key from the index. 
    // It removes only value.  It can be fixed by using links to parent in the
    // IndexTree nodes and cleaning up the map entries which point to the empty
    // nodes only.
    fn remove(&mut self, key: &Atom, value: &T) -> bool {
        log::debug!("IndexTree::remove(): key: {:?}, value: {:?}", key, value);
        IndexTreeIterMut::new(self, &key, |idx, key, keys, callback| {
            idx.next_for_remove(key, keys, callback)
        }).map(|idx| idx.remove_value(value)).fold(false, |a, b| a | b)
    }

    fn get(&self, pattern: &Atom) -> impl Iterator<Item=&T> {
        IndexTreeIter::new(self, &pattern, |idx, key, keys, callback| {
            idx.next_for_get(key, keys, callback)
        }).flat_map(|idx| idx.leaf.as_slice().iter())
    }
}

/// Symbol to concatenate queries to space.
pub const COMMA_SYMBOL : Atom = sym!(",");

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
/// space.register_observer(Rc::clone(&observer));
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

struct GroundingSpaceIter<'a> {
    space: &'a GroundingSpace,
    i: usize,
}

impl<'a> GroundingSpaceIter<'a> {
    fn new(space: &'a GroundingSpace) -> Self {
        GroundingSpaceIter { space, i: 0 }
    }
}

impl<'a> Iterator for GroundingSpaceIter<'a> {
    type Item = &'a Atom;

    fn next(&mut self) -> Option<Self::Item> {
        let len = self.space.content.len();
        let mut i = self.i;
        while i < len && self.space.free.contains(&i) {
            i += 1;
        }
        if i >= len {
            self.i = i;
            None
        } else {
            self.i = i + 1;
            self.space.content.get(i)
        }
    }
}

/// In-memory space which can contain grounded atoms.
// TODO: Clone is required by C API
#[derive(Clone)]
pub struct GroundingSpace {
    index: IndexTree<usize>,
    content: Vec<Atom>,
    free: BTreeSet<usize>,
    observers: RefCell<Vec<Weak<RefCell<dyn SpaceObserver>>>>,
}

impl GroundingSpace {

    /// Constructs new empty space.
    pub fn new() -> Self {
        Self {
            index: IndexTree::new(),
            content: Vec::new(),
            free: BTreeSet::new(),
            observers: RefCell::new(Vec::new()),
        }
    }

    /// Constructs space from vector of atoms.
    pub fn from_vec(atoms: Vec<Atom>) -> Self {
        let mut index = IndexTree::new();
        for (i, atom) in atoms.iter().enumerate() {
            index.add(atom, i);
        }
        Self{
            index,
            content: atoms,
            free: BTreeSet::new(),
            observers: RefCell::new(Vec::new()),
        }
    }

    /// Registers space modifications `observer`. Observer is automatically
    /// deregistered when `Rc` counter reaches zero. See [SpaceObserver] for
    /// examples.
    pub fn register_observer<T>(&self, observer: Rc<RefCell<T>>)
        where T: SpaceObserver + 'static
    {
        self.observers.borrow_mut().push(Rc::downgrade(&observer) as Weak<RefCell<dyn SpaceObserver>>);
    }

    /// Notifies registered observers about space modification `event`.
    fn notify(&self, event: &SpaceEvent) {
        let mut cleanup = false;
        for observer in self.observers.borrow_mut().iter() {
            if let Some(observer) = observer.upgrade() {
                observer.borrow_mut().notify(event);
            } else {
                cleanup = true;
            }
        }
        if cleanup {
            self.observers.borrow_mut().retain(|w| w.strong_count() > 0);
        }
    }

    /// Adds `atom` into space.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::sym;
    /// use hyperon::space::grounding::GroundingSpace;
    /// use hyperon::atom::matcher::Bindings;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    /// 
    /// space.add(sym!("B"));
    ///
    /// assert_eq!(space.query(&sym!("A")), vec![Bindings::new()]);
    /// assert_eq!(space.query(&sym!("B")), vec![Bindings::new()]);
    /// assert_eq!(space.query(&sym!("C")), vec![]);
    /// ```
    pub fn add(&mut self, atom: Atom) {
        log::debug!("GroundingSpace::add(): self: {:?}, atom: {:?}", self as *const GroundingSpace, atom);
        self.add_internal(atom.clone());
        self.notify(&SpaceEvent::Add(atom));
    }

    fn add_internal(&mut self, atom: Atom) {
        if self.free.is_empty() {
            let pos = self.content.len();
            self.index.add(&atom, pos);
            self.content.push(atom);
        } else {
            let pos = *self.free.iter().next().unwrap();
            self.free.remove(&pos);
            self.index.add(&atom, pos);
            self.content[pos] = atom;
        }
    }

    /// Removes `atom` from space. Returns true if atom was found and removed,
    /// and false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::sym;
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    /// 
    /// space.remove(&sym!("A"));
    ///
    /// assert_eq!(space.query(&sym!("A")), vec![]);
    /// ```
    pub fn remove(&mut self, atom: &Atom) -> bool {
        log::debug!("GroundingSpace::remove(): self: {:?}, atom: {:?}", self as *const GroundingSpace, atom);
        let is_removed = self.remove_internal(atom);
        if is_removed {
            self.notify(&SpaceEvent::Remove(atom.clone()));
        }
        is_removed
    }

    fn remove_internal(&mut self, atom: &Atom) -> bool {
        let indexes: Vec<usize> = self.index.get(atom).map(|i| *i).collect();
        let mut indexes: Vec<usize> = indexes.into_iter()
            .filter(|i| self.content[*i] == *atom).collect();
        indexes.sort_by(|a, b| b.partial_cmp(a).unwrap());
        let is_removed = indexes.len() > 0;
        for i in indexes {
            self.index.remove(atom, &i);
            self.free.insert(i);
        }
        is_removed
    }

    /// Replaces `from` atom to `to` atom inside space. Doesn't add `to` when
    /// `from` is not found. Returns true if atom was found and replaced, and
    /// false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::sym;
    /// use hyperon::space::grounding::GroundingSpace;
    /// use hyperon::atom::matcher::Bindings;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    /// 
    /// space.replace(&sym!("A"), sym!("B"));
    ///
    /// assert_eq!(space.query(&sym!("A")), vec![]);
    /// assert_eq!(space.query(&sym!("B")), vec![Bindings::new()]);
    /// ```
    pub fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        let is_replaced = self.replace_internal(from, to.clone());
        if is_replaced {
            self.notify(&SpaceEvent::Replace(from.clone(), to));
        }
        is_replaced
    }

    fn replace_internal(&mut self, from: &Atom, to: Atom) -> bool {
        let is_replaced = self.remove_internal(from);
        if is_replaced {
            self.add_internal(to);
        }
        is_replaced
    }

    /// Executes `query` on the space and returns variable bindings found.
    /// Query may include sub-queries glued by [COMMA_SYMBOL] symbol. Number
    /// of results is equal to the length of the `Vec<Bindings>` returned.
    /// Each [Bindings] instance represents single result.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::{expr, bind, sym};
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let space = GroundingSpace::from_vec(vec![expr!("A" "B"), expr!("B" "C")]);
    /// let query = expr!("," ("A" x) (x "C"));
    ///
    /// let result = space.query(&query);
    ///
    /// assert_eq!(result, vec![bind!{x: sym!("B")}]);
    /// ```
    pub fn query(&self, query: &Atom) -> Vec<Bindings> {
        match split_expr(query) {
            // Cannot match with COMMA_SYMBOL here, because Rust allows
            // it only when Atom has PartialEq and Eq derived.
            Some((sym @ Atom::Symbol(_), args)) if *sym == COMMA_SYMBOL => {
                let vars = collect_variables(&query);
                let mut result = args.fold(vec![bind!{}],
                    |mut acc, query| {
                        let result = if acc.is_empty() {
                            acc
                        } else {
                            acc.drain(0..).flat_map(|prev| -> Vec<Bindings> {
                                let query = matcher::apply_bindings_to_atom(&query, &prev);
                                let mut res = self.query(&query);
                                res.drain(0..)
                                    .map(|next| Bindings::merge(&prev, &next))
                                    .filter(Option::is_some).map(Option::unwrap)
                                    .map(|next| matcher::apply_bindings_to_bindings(&next, &next)
                                        .expect("Self consistent bindings are expected"))
                                    .collect()
                            }).collect()
                        };
                        log::debug!("query: current result: {:?}", result);
                        result
                    });
                result.iter_mut().for_each(|bindings| bindings.filter(|k, _v| vars.contains(k)));
                result
            },
            _ => self.single_query(query),
        }
    }

    /// Executes simple `query` without sub-queries on the space.
    fn single_query(&self, query: &Atom) -> Vec<Bindings> {
        log::debug!("single_query: query: {}", query);
        let mut result = Vec::new();
        for i in self.index.get(query) {
            let next = self.content.get(*i).expect(format!("Index contains absent atom: key: {:?}, position: {}", query, i).as_str());
            let next = make_variables_unique(next);
            log::trace!("single_query: match next: {}", next);
            for bindings in match_atoms(&next, query) {
                log::trace!("single_query: push result: {}", bindings);
                result.push(bindings);
            }
        }
        log::debug!("single_query: result: {:?}", result);
        result
    }

    /// Executes `pattern` query on the space and for each result substitutes
    /// variables in `template` by the values from `pattern`. Returns results
    /// of the substitution.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::{expr, assert_eq_no_order};
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let space = GroundingSpace::from_vec(vec![expr!("A" "B"), expr!("A" "C")]);
    ///
    /// let result = space.subst(&expr!("A" x), &expr!("D" x));
    ///
    /// assert_eq_no_order!(result, vec![expr!("D" "B"), expr!("D" "C")]);
    /// ```
    pub fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        self.query(pattern).drain(0..)
            .map(| bindings | matcher::apply_bindings_to_atom(template, &bindings))
            .collect()
    }

    // TODO: for now we have separate methods query() and unify() but
    // they probably can be merged. One way of doing it is designating
    // in the query which part of query should be unified and which matched.
    // For example for the typical query in a form (= (+ a b) $X) the
    // (= (...) $X) level should not be unified otherwise we will recursively
    // infer that we need calculating (+ a b) again which is equal to original
    // query. Another option is designating this in the data itself.
    // After combining match and unification we could leave only single
    // universal method.
    #[doc(hidden)]
    pub fn unify(&self, pattern: &Atom) -> Vec<(Bindings, Unifications)> {
        log::debug!("unify: pattern: {}", pattern);
        let mut result = Vec::new();
        for i in self.index.get(pattern) {
            let next = self.content.get(*i).expect(format!("Index contains absent atom: key: {:?}, position: {}", pattern, i).as_str());
            match matcher::unify_atoms(next, pattern) {
                Some(res) => {
                    let bindings = matcher::apply_bindings_to_bindings(&res.data_bindings, &res.pattern_bindings);
                    if let Ok(bindings) = bindings {
                        // TODO: implement Display for bindings
                        log::debug!("unify: push result: {}, bindings: {:?}", next, bindings);
                        result.push((bindings, res.unifications));
                    }
                },
                None => continue,
            }
        }
        result
    }

    /// Returns the iterator over content of the space.
    pub fn iter(&self) -> impl Iterator<Item=&Atom> {
        GroundingSpaceIter::new(self)
    }
}

impl PartialEq for GroundingSpace {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl Debug for GroundingSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundingSpace")
    }
}

impl Display for GroundingSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundingSpace")
    }
}

impl Grounded for GroundingSpace {
    fn type_(&self) -> Atom {
        rust_type_atom::<GroundingSpace>()
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        Box::new(self.query(other).into_iter())
    }

    fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        execute_not_executable(self)
    }
}

fn collect_variables(atom: &Atom) -> HashSet<VariableAtom> {
    fn recursion(atom: &Atom, vars: &mut HashSet<VariableAtom>) {
        match atom {
            Atom::Variable(var) => { vars.insert(var.clone()); },
            Atom::Expression(expr) => {
                expr.children().iter().for_each(|child| recursion(child, vars));
            }
            _ => {},
        }
    }
    let mut vars = HashSet::new();
    recursion(atom, &mut vars);
    vars
}


#[cfg(test)]
mod test {
    use super::*;

    struct SpaceEventCollector {
        events: Vec<SpaceEvent>,
    }

    impl SpaceEventCollector {
        fn new() -> Self {
            Self{ events: Vec::new() }
        }
    }

    impl SpaceObserver for SpaceEventCollector {
        fn notify(&mut self, event: &SpaceEvent) {
            self.events.push(event.clone());
        }
    }

    #[test]
    fn add_atom() {
        let mut space = GroundingSpace::new();
        let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
        space.register_observer(Rc::clone(&observer));

        space.add(expr!("a"));
        space.add(expr!("b"));
        space.add(expr!("c"));

        assert_eq_no_order!(space, vec![expr!("a"), expr!("b"), expr!("c")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a")),
            SpaceEvent::Add(sym!("b")), SpaceEvent::Add(sym!("c"))]);
    }

    #[test]
    fn remove_atom() {
        let mut space = GroundingSpace::new();
        let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
        space.register_observer(Rc::clone(&observer));

        space.add(expr!("a"));
        space.add(expr!("b"));
        space.add(expr!("c"));
        assert_eq!(space.remove(&expr!("b")), true);

        assert_eq_no_order!(space, vec![expr!("a"), expr!("c")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a")),
            SpaceEvent::Add(sym!("b")), SpaceEvent::Add(sym!("c")),
            SpaceEvent::Remove(sym!("b"))]);
    }

    #[test]
    fn remove_atom_not_found() {
        let mut space = GroundingSpace::new();
        let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
        space.register_observer(Rc::clone(&observer));

        space.add(expr!("a"));
        assert_eq!(space.remove(&expr!("b")), false);

        assert_eq_no_order!(space, vec![expr!("a")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a"))]);
    }

    #[test]
    fn replace_atom() {
        let mut space = GroundingSpace::new();
        let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
        space.register_observer(Rc::clone(&observer));

        space.add(expr!("a"));
        space.add(expr!("b"));
        space.add(expr!("c"));
        assert_eq!(space.replace(&expr!("b"), expr!("d")), true);

        assert_eq_no_order!(space, vec![expr!("a"), expr!("d"), expr!("c")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a")),
            SpaceEvent::Add(sym!("b")), SpaceEvent::Add(sym!("c")),
            SpaceEvent::Replace(sym!("b"), sym!("d"))]);
    }

    #[test]
    fn replace_atom_not_found() {
        let mut space = GroundingSpace::new();
        let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
        space.register_observer(Rc::clone(&observer));

        space.add(expr!("a"));
        assert_eq!(space.replace(&expr!("b"), expr!("d")), false);

        assert_eq_no_order!(space, vec![expr!("a")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a"))]);
    }

    #[test]
    fn remove_replaced_atom() {
        let mut space = GroundingSpace::new();
        let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
        space.register_observer(Rc::clone(&observer));

        space.add(expr!("a"));
        space.replace(&expr!("a"), expr!("b"));
        assert_eq!(space.remove(&expr!("b")), true);

        assert_eq_no_order!(space, Vec::<Atom>::new());
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a")),
            SpaceEvent::Replace(expr!("a"), expr!("b")),
            SpaceEvent::Remove(expr!("b"))]);
    }

    #[test]
    fn get_atom_after_removed() {
        let mut space = GroundingSpace::new();

        space.add(Atom::sym("A"));
        space.add(Atom::sym("B"));
        space.remove(&Atom::sym("A"));

        assert_eq!(space.query(&Atom::sym("B")), vec![Bindings::new()]);
    }

    #[test]
    fn iter_empty() {
        let space = GroundingSpace::from_vec(vec![]);

        let iter = space.iter();
        assert_eq!(iter.count(), 0);
    }

    #[test]
    fn iter_after_remove() {
        let mut space = GroundingSpace::from_vec(vec![expr!("a"), expr!("b"), expr!("c")]);
        space.remove(&expr!("b"));

        let mut iter = space.iter();
        assert_eq!(iter.next(), Some(&expr!("a")));
        assert_eq!(iter.next(), Some(&expr!("c")));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn mut_cloned_atomspace() {
        let mut first = GroundingSpace::new();
        let mut second = first.clone(); 

        first.add(expr!("b"));
        second.add(expr!("d"));

        assert_eq_no_order!(first, vec![expr!("b")]);
        assert_eq_no_order!(second, vec![expr!("d")]);
    }

    #[test]
    fn test_match_symbol() {
        let mut space = GroundingSpace::new();
        space.add(expr!("foo"));
        assert_eq!(space.query(&expr!("foo")), vec![bind!{}]);
    }

    #[test]
    fn test_match_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("foo"));
        assert_eq!(space.query(&expr!(x)), vec![bind!{x: expr!("foo")}]);
    }

    #[test]
    fn test_match_expression() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "a" ("*" "b" "c")));
        assert_eq!(space.query(&expr!("+" "a" ("*" "b" "c"))), vec![bind!{}]);
    }

    #[test]
    fn test_match_expression_with_variables() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" b c))),
        vec![bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }]);
    }

    #[test]
    fn test_match_different_value_for_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" a c))), vec![]);
    }

    fn get_var<'a>(bindings: &'a Bindings, name: &str) -> &'a Atom {
        bindings.get(&VariableAtom::new(name)).unwrap()
    }

    #[test]
    fn test_match_query_variable_has_priority() {
        let mut space = GroundingSpace::new();
        space.add(expr!("equals" x x));
        
        let result = space.query(&expr!("equals" y z));
        assert_eq!(result.len(), 1);
        assert!(matches!(get_var(&result[0], "y"), Atom::Variable(_)));
        assert!(matches!(get_var(&result[0], "z"), Atom::Variable(_)));
    }

    #[test]
    fn test_match_query_variable_via_data_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!(x x));
        assert_eq!(space.query(&expr!(y (z))), vec![bind!{y: expr!((z))}]);
    }

    #[test]
    fn test_match_if_then_with_x() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("if" "True" then) then));
        assert_eq!(space.query(&expr!("=" ("if" "True" "42") X)),
        vec![bind!{X: expr!("42")}]);
    }

    #[test]
    fn test_match_combined_query() {
        let mut space = GroundingSpace::new();
        space.add(expr!("posesses" "Sam" "baloon"));
        space.add(expr!("likes" "Sam" ("blue" "stuff")));
        space.add(expr!("has-color" "baloon" "blue"));

        let result = space.query(&expr!("," ("posesses" "Sam" object)
        ("likes" "Sam" (color "stuff"))
        ("has-color" object color)));
        assert_eq!(result, vec![bind!{object: expr!("baloon"), color: expr!("blue")}]);
    }

    #[test]
    fn test_unify_variables_inside_conjunction_query() {
        let mut space = GroundingSpace::new();
        space.add(expr!("lst1" ("Cons" "a1" ("Cons" "b2" "b3"))));
        space.add(expr!("lst2" ("Cons" "a2" ("Cons" "b3" "b4"))));
        space.add(expr!("Concat" x1 x2 x3));

        let result = space.subst(
            &expr!("," ("lst1" l1) ("lst2" l2) ("Concat" l1 "a2" "a3")),
            &expr!(l1));
        assert_eq!(result, vec![expr!("Cons" "a1" ("Cons" "b2" "b3"))]);
    }

    #[test]
    fn test_type_check_in_query() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "Human" "Type"));
        space.add(expr!(":" "Socrates" "Human"));
        space.add(expr!("Cons" "Socrates" "Nil"));

        let result = space.query(&expr!("," (":" h "Human") ("Cons" h t)));
        assert_eq!(result, vec![bind!{h: expr!("Socrates"), t: expr!("Nil")}]);
    }

    #[test]
    fn cleanup_observer() {
        let mut space = GroundingSpace::new();
        {
            let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
            space.register_observer(Rc::clone(&observer));
            assert_eq!(space.observers.borrow().len(), 1);
        }

        space.add(expr!("a"));

        assert_eq_no_order!(space, vec![expr!("a")]);
        assert_eq!(space.observers.borrow().len(), 0);
    }

    #[test]
    fn complex_query_applying_bindings_to_next_pattern() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":=" ("sum" a b) ("+" a b)));
        space.add(expr!(":=" "a" {4}));

        let result = space.query(&expr!("," (":=" "a" b) (":=" ("sum" {3} b) W)));

        assert_eq!(result, vec![bind!{b: expr!({4}), W: expr!("+" {3} {4})}]);
    }

    #[test]
    fn complex_query_chain_of_bindings() {
        let mut space = GroundingSpace::new();
        space.add(expr!("implies" ("B" x) ("C" x)));
        space.add(expr!("implies" ("A" x) ("B" x)));
        space.add(expr!("A" "Sam"));

        let result = space.query(&expr!("," ("implies" ("B" x) z) ("implies" ("A" x) y) ("A" x)));
        assert_eq!(result, vec![bind!{x: sym!("Sam"), y: expr!("B" "Sam"), z: expr!("C" "Sam")}]);
    }

    #[test]
    fn test_custom_match_with_space() {
        let space = GroundingSpace::from_vec(vec![
            expr!("A" {1} x "a"),
            expr!("B" {1} x "b"),
            expr!("A" {2} x "c"),
        ]);
        let result: Vec<Bindings> = match_atoms(&Atom::gnd(space), &expr!("A" {1} x x)).collect();
        assert_eq!(result, vec![bind!{x: sym!("a")}]);
    }

    trait IntoVec<T: Ord> {
        fn to_vec(self) -> Vec<T>;
    }

    impl<'a, T: 'a + Ord + Clone, I: Iterator<Item=&'a T>> IntoVec<T> for I {
        fn to_vec(self) -> Vec<T> {
            let mut vec: Vec<T> = self.cloned().collect();
            vec.sort();
            vec
        }
    }

    #[test]
    fn index_tree_add_atom_basic() {
        let mut index = IndexTree::new();
        index.add(&Atom::sym("A"), 1);
        index.add(&Atom::value(1), 2);
        index.add(&Atom::var("a"), 3);
        index.add(&expr!("A" "B"), 4);

        // TODO: index doesn't match grounded atoms yet, it considers them as wildcards
        // as matching can be redefined for them
        assert_eq!(index.get(&Atom::sym("A")).to_vec(), vec![1, 2, 3]);
        assert_eq!(index.get(&Atom::sym("B")).to_vec(), vec![2, 3]);

        assert_eq!(index.get(&Atom::value(1)).to_vec(), vec![1, 2, 3, 4]);
        assert_eq!(index.get(&Atom::value(2)).to_vec(), vec![1, 2, 3, 4]);

        assert_eq!(index.get(&Atom::var("a")).to_vec(), vec![1, 2, 3, 4]);
        assert_eq!(index.get(&Atom::var("b")).to_vec(), vec![1, 2, 3, 4]);

        assert_eq!(index.get(&expr!("A" "B")).to_vec(), vec![2, 3, 4]);
        assert_eq!(index.get(&expr!("A" "C")).to_vec(), vec![2, 3]);
    }

    #[test]
    fn index_tree_add_atom_expr() {
        let mut index = IndexTree::new();
        index.add(&expr!(("A") "B"), 1);
        index.add(&expr!(a "C"), 2);

        assert_eq!(index.get(&expr!(a "B")).to_vec(), vec![1]);
        assert_eq!(index.get(&expr!(("A") "C")).to_vec(), vec![2]);
    }

    #[test]
    fn index_tree_remove_atom_basic() {
        let mut index = IndexTree::new();

        index.add(&Atom::sym("A"), 1);
        index.add(&Atom::value(1), 2);
        index.add(&Atom::var("a"), 3);
        index.add(&expr!("A" "B"), 4);

        index.remove(&Atom::sym("A"), &1);
        index.remove(&Atom::value(1), &2);
        index.remove(&Atom::var("a"), &3);
        index.remove(&expr!("A" "B"), &4);

        assert_eq!(index.get(&Atom::sym("A")).to_vec(), vec![]);
        assert_eq!(index.get(&Atom::value(1)).to_vec(), vec![]);
        assert_eq!(index.get(&Atom::var("a")).to_vec(), vec![]);
        assert_eq!(index.get(&expr!("A" "B")).to_vec(), vec![]);
    }

}
