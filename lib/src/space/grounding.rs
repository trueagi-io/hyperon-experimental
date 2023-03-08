//! Simple implementation of space which uses in-memory vector of atoms as
//! an underlying storage.

use crate::*;
use super::*;
use crate::atom::*;
use crate::atom::matcher::{Bindings, match_atoms};
use crate::atom::subexpr::split_expr;
use crate::matcher::MatchResultIter;
use crate::common::multitrie::{MultiTrie, TrieKey, NodeKey};

use std::fmt::{Display, Debug};
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::collections::HashSet;

// Grounding space

/// Symbol to concatenate queries to space.
pub const COMMA_SYMBOL : Atom = sym!(",");

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

#[derive(Clone)]
struct Index(MultiTrie<SymbolAtom, usize>);

impl Index {

    fn new() -> Self {
        Index(MultiTrie::new())
    }

    fn add(&mut self, atom: &Atom, i: usize) {
        self.0.add(Self::key(atom), i)
    }

    fn remove(&mut self, atom: &Atom, i: &usize) -> bool {
        self.0.remove(Self::key(atom), i)
    }

    fn get(&self, pattern: &Atom) -> impl Iterator<Item=&usize> {
        self.0.get(Self::key(pattern))
    }

    fn key(atom: &Atom) -> TrieKey<SymbolAtom> {
        TrieKey::from_list(Self::key_vec(atom))
    }

    fn key_vec(atom: &Atom) -> Vec<NodeKey<SymbolAtom>> {
        match atom {
            Atom::Symbol(sym) => vec![NodeKey::Exact(sym.clone())],
            Atom::Expression(expr) => {
                let mut keys: Vec<NodeKey<SymbolAtom>> = expr.children().iter()
                    .flat_map(Self::key_vec).collect();
                keys.push(NodeKey::ExpressionEnd);
                keys.insert(0, NodeKey::Expression(keys.len()));
                keys
            },
            // TODO: At the moment all grounding symbols are matched as wildcards
            // because they potentially may have custom Grounded::match_()
            // implementation and we cannot understand it from data. We could improve
            // speed of extracting grounded values from the index if GroundedAtom
            // has a flag which says whether match_() is match_by_equality() or
            // not. GroundedAtom with match_by_equality() implementation can be
            // added as separate NodeKey::GroundedValue to navigate through
            // the index quickly. GroundedAtom with custom match_() will be added
            // as a wildcard to be matched after search in index. It also requires
            // implementing Hash on Grounded.
            _ => vec![NodeKey::Wildcard],
        }
    }

}

/// In-memory space which can contain grounded atoms.
// TODO: Clone is required by C API
#[derive(Clone)]
pub struct GroundingSpace {
    index: Index,
    content: Vec<Atom>,
    free: BTreeSet<usize>,
    observers: RefCell<Vec<Weak<RefCell<dyn SpaceObserver>>>>,
}

impl GroundingSpace {

    /// Constructs new empty space.
    pub fn new() -> Self {
        Self {
            index: Index::new(),
            content: Vec::new(),
            free: BTreeSet::new(),
            observers: RefCell::new(Vec::new()),
        }
    }

    /// Constructs space from vector of atoms.
    pub fn from_vec(atoms: Vec<Atom>) -> Self {
        let mut index = Index::new();
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
    pub fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>)
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
        //log::debug!("GroundingSpace::add(): self: {:?}, atom: {:?}", self as *const GroundingSpace, atom);
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
        //log::debug!("GroundingSpace::remove(): self: {:?}, atom: {:?}", self as *const GroundingSpace, atom);
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
                let result = args.fold(vec![bind!{}],
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
                result
            },
            _ => self.single_query(query),
        }
    }

    /// Executes simple `query` without sub-queries on the space.
    fn single_query(&self, query: &Atom) -> Vec<Bindings> {
        log::debug!("single_query: query: {}", query);
        let mut result = Vec::new();
        let mut query_vars = HashSet::new();
        query.iter().filter_map(AtomIter::extract_var).for_each(|var| { query_vars.insert(var.clone()); });
        for i in self.index.get(query) {
            let next = self.content.get(*i).expect(format!("Index contains absent atom: key: {:?}, position: {}", query, i).as_str());
            let next = make_variables_unique(next);
            log::trace!("single_query: match next: {}", next);
            for bindings in match_atoms(&next, query) {
                let bindings = bindings.narrow_vars(&query_vars);
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

    /// Returns the iterator over content of the space.
    pub fn iter(&self) -> SpaceIter {
        SpaceIter::new(GroundingSpaceIter::new(self))
    }
}

impl Space for GroundingSpace {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>) {
        GroundingSpace::register_observer(self, observer)
    }
    fn query(&self, query: &Atom) -> Vec<Bindings> {
        GroundingSpace::query(self, query)
    }
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        GroundingSpace::subst(self, pattern, template)
    }
}

impl SpaceMut for GroundingSpace {
    fn add(&mut self, atom: Atom) {
        GroundingSpace::add(self, atom)
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        GroundingSpace::remove(self, atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        GroundingSpace::replace(self, from, to)
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
        space.register_observer(observer.clone());

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
        space.register_observer(observer.clone());

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
        space.register_observer(observer.clone());

        space.add(expr!("a"));
        assert_eq!(space.remove(&expr!("b")), false);

        assert_eq_no_order!(space, vec![expr!("a")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a"))]);
    }

    #[test]
    fn replace_atom() {
        let mut space = GroundingSpace::new();
        let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
        space.register_observer(observer.clone());

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
        space.register_observer(observer.clone());

        space.add(expr!("a"));
        assert_eq!(space.replace(&expr!("b"), expr!("d")), false);

        assert_eq_no_order!(space, vec![expr!("a")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a"))]);
    }

    #[test]
    fn remove_replaced_atom() {
        let mut space = GroundingSpace::new();
        let observer = Rc::new(RefCell::new(SpaceEventCollector::new()));
        space.register_observer(observer.clone());

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

    #[test]
    fn test_match_query_variable_has_priority() {
        let mut space = GroundingSpace::new();
        space.add(expr!("equals" x x));
        
        let result = space.query(&expr!("equals" y z));
        assert_eq!(result, vec![bind!{ y: expr!(z) }]);
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
            space.register_observer(observer.clone());
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

        assert_eq!(result.len(), 1);
        let result = &result[0];
        assert_eq!(result.resolve(&VariableAtom::new("W")), Some(expr!("+" {3} {4})));
        assert_eq!(result.resolve(&VariableAtom::new("b")), Some(expr!({4})));
    }

    #[test]
    fn complex_query_chain_of_bindings() {
        let mut space = GroundingSpace::new();
        space.add(expr!("implies" ("B" x) ("C" x)));
        space.add(expr!("implies" ("A" x) ("B" x)));
        space.add(expr!("A" "Sam"));

        let result = space.query(&expr!("," ("implies" ("B" x) z) ("implies" ("A" x) y) ("A" x)));
        assert_eq!(result, vec![bind!{x: sym!("Sam"), y: expr!("B" x), z: expr!("C" x)}]);
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
    fn index_add_atom_basic() {
        let mut index = Index::new();
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
    fn index_add_atom_expr() {
        let mut index = Index::new();
        index.add(&expr!(("A") "B"), 1);
        index.add(&expr!(a "C"), 2);

        assert_eq!(index.get(&expr!(a "B")).to_vec(), vec![1]);
        assert_eq!(index.get(&expr!(("A") "C")).to_vec(), vec![2]);
    }

    #[test]
    fn index_remove_atom_basic() {
        let mut index = Index::new();

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
