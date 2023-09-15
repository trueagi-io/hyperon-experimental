//! Simple implementation of space which uses in-memory vector of atoms as
//! an underlying storage.

use crate::*;
use super::*;
use crate::atom::*;
use crate::atom::matcher::{BindingsSet, MatchResultIter, match_atoms};
use crate::atom::subexpr::split_expr;
use crate::common::multitrie::{MultiTrie, TrieKey, TrieToken};

use std::fmt::{Display, Debug};
use std::collections::BTreeSet;

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

fn atom_to_trie_key(atom: &Atom) -> TrieKey<SymbolAtom> {
    fn fill_key(atom: &Atom, tokens: &mut Vec<TrieToken<SymbolAtom>>) {
        match atom {
            Atom::Symbol(sym) => tokens.push(TrieToken::Exact(sym.clone())),
            Atom::Expression(expr) => {
                tokens.push(TrieToken::LeftPar);
                expr.children().iter().for_each(|child| fill_key(child, tokens));
                tokens.push(TrieToken::RightPar);
            },
            // TODO: At the moment all grounding symbols are matched as wildcards
            // because they potentially may have custom Grounded::match_()
            // implementation and we cannot understand it from data. We could improve
            // speed of extracting grounded values from the index if GroundedAtom
            // has a flag which says whether match_() is match_by_equality() or
            // not. GroundedAtom with match_by_equality() implementation can be
            // added as separate TrieToken::GroundedValue to navigate through
            // the index quickly. GroundedAtom with custom match_() will be added
            // as a wildcard to be matched after search in index. It also requires
            // implementing Hash on Grounded.
            _ => tokens.push(TrieToken::Wildcard),
        }
    }

    let mut tokens = Vec::new();
    fill_key(atom, &mut tokens);
    TrieKey::from(tokens)
}

/// In-memory space which can contain grounded atoms.
// TODO: Clone is required by C API
#[derive(Clone)]
pub struct GroundingSpace {
    index: MultiTrie<SymbolAtom, usize>,
    content: Vec<Atom>,
    free: BTreeSet<usize>,
    common: SpaceCommon,
}

impl GroundingSpace {

    /// Constructs new empty space.
    pub fn new() -> Self {
        Self {
            index: MultiTrie::new(),
            content: Vec::new(),
            free: BTreeSet::new(),
            common: SpaceCommon::default(),
        }
    }

    /// Constructs space from vector of atoms.
    pub fn from_vec(atoms: Vec<Atom>) -> Self {
        let mut index = MultiTrie::new();
        for (i, atom) in atoms.iter().enumerate() {
            index.insert(atom_to_trie_key(atom), i);
        }
        Self{
            index,
            content: atoms,
            free: BTreeSet::new(),
            common: SpaceCommon::default(),
        }
    }

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
    pub fn add(&mut self, atom: Atom) {
        //log::debug!("GroundingSpace::add(): self: {:?}, atom: {:?}", self as *const GroundingSpace, atom);
        self.add_internal(atom.clone());
        self.common.notify_all_observers(&SpaceEvent::Add(atom));
    }

    fn add_internal(&mut self, atom: Atom) {
        if self.free.is_empty() {
            let pos = self.content.len();
            self.index.insert(atom_to_trie_key(&atom), pos);
            self.content.push(atom);
        } else {
            let pos = *self.free.iter().next().unwrap();
            self.free.remove(&pos);
            self.index.insert(atom_to_trie_key(&atom), pos);
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
    /// use hyperon::matcher::BindingsSet;
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    ///
    /// space.remove(&sym!("A"));
    ///
    /// assert_eq!(space.query(&sym!("A")), BindingsSet::empty());
    /// ```
    pub fn remove(&mut self, atom: &Atom) -> bool {
        //log::debug!("GroundingSpace::remove(): self: {:?}, atom: {:?}", self as *const GroundingSpace, atom);
        let is_removed = self.remove_internal(atom);
        if is_removed {
            self.common.notify_all_observers(&SpaceEvent::Remove(atom.clone()));
        }
        is_removed
    }

    fn remove_internal(&mut self, atom: &Atom) -> bool {
        let index_key = atom_to_trie_key(atom);
        let indexes: Vec<usize> = self.index.get(&index_key).map(|i| *i).collect();
        let mut indexes: Vec<usize> = indexes.into_iter()
            .filter(|i| self.content[*i] == *atom).collect();
        indexes.sort_by(|a, b| b.partial_cmp(a).unwrap());
        let is_removed = indexes.len() > 0;
        for i in indexes {
            self.index.remove(&index_key, &i);
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
    /// use hyperon::atom::matcher::BindingsSet;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    ///
    /// space.replace(&sym!("A"), sym!("B"));
    ///
    /// assert_eq!(space.query(&sym!("A")), BindingsSet::empty());
    /// assert_eq!(space.query(&sym!("B")), BindingsSet::single());
    /// ```
    pub fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        let is_replaced = self.replace_internal(from, to.clone());
        if is_replaced {
            self.common.notify_all_observers(&SpaceEvent::Replace(from.clone(), to));
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
    /// Query may include sub-queries glued by [COMMA_SYMBOL] symbol.
    /// Each [Bindings](matcher::Bindings) instance in the returned [BindingsSet]
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
    pub fn query(&self, query: &Atom) -> BindingsSet {
        match split_expr(query) {
            // Cannot match with COMMA_SYMBOL here, because Rust allows
            // it only when Atom has PartialEq and Eq derived.
            Some((sym @ Atom::Symbol(_), args)) if *sym == COMMA_SYMBOL => {
                args.fold(BindingsSet::single(),
                    |mut acc, query| {
                        let result = if acc.is_empty() {
                            acc
                        } else {
                            acc.drain(0..).flat_map(|prev| -> BindingsSet {
                                let query = matcher::apply_bindings_to_atom(&query, &prev);
                                let mut res = self.query(&query);
                                res.drain(0..)
                                    .flat_map(|next| next.merge_v2(&prev))
                                    .collect()
                            }).collect()
                        };
                        log::debug!("query: current result: {:?}", result);
                        result
                    })
            },
            _ => self.single_query(query),
        }
    }

    /// Executes simple `query` without sub-queries on the space.
    fn single_query(&self, query: &Atom) -> BindingsSet {
        log::debug!("single_query: query: {}", query);
        let mut result = BindingsSet::empty();
        let query_vars = query.iter().filter_type::<&VariableAtom>().collect();
        for i in self.index.get(&atom_to_trie_key(query)) {
            let next = self.content.get(*i).expect(format!("Index contains absent atom: key: {:?}, position: {}", query, i).as_str());
            let next = make_variables_unique(next.clone());
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

    /// Returns the iterator over content of the space.
    pub fn iter(&self) -> SpaceIter {
        SpaceIter::new(GroundingSpaceIter::new(self))
    }
}

impl Space for GroundingSpace {
    fn common(&self) -> FlexRef<SpaceCommon> {
        FlexRef::from_simple(&self.common)
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        GroundingSpace::query(self, query)
    }
    fn atom_count(&self) -> Option<usize> {
        Some(self.iter().count())
    }
    fn atom_iter(&self) -> Option<SpaceIter> {
        Some(self.iter())
    }
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        Some(self)
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
    fn as_space(&self) -> &dyn Space {
        self
    }
}

impl PartialEq for GroundingSpace {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl Debug for GroundingSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundingSpace-{self:p}")
    }
}

impl Display for GroundingSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundingSpace-{self:p}")
    }
}

impl Grounded for GroundingSpace {
    fn type_(&self) -> Atom {
        rust_type_atom::<GroundingSpace>()
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        Box::new(self.query(other).into_iter())
    }

    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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
        let observer = space.common.register_observer(SpaceEventCollector::new());

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
        let observer = space.common.register_observer(SpaceEventCollector::new());

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
        let observer = space.common.register_observer(SpaceEventCollector::new());

        space.add(expr!("a"));
        assert_eq!(space.remove(&expr!("b")), false);

        assert_eq_no_order!(space, vec![expr!("a")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a"))]);
    }

    #[test]
    fn replace_atom() {
        let mut space = GroundingSpace::new();
        let observer = space.common.register_observer(SpaceEventCollector::new());

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
        let observer = space.common.register_observer(SpaceEventCollector::new());

        space.add(expr!("a"));
        assert_eq!(space.replace(&expr!("b"), expr!("d")), false);

        assert_eq_no_order!(space, vec![expr!("a")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a"))]);
    }

    #[test]
    fn remove_replaced_atom() {
        let mut space = GroundingSpace::new();
        let observer = space.common.register_observer(SpaceEventCollector::new());

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

        assert_eq!(space.query(&Atom::sym("B")), BindingsSet::single());
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
        assert_eq!(space.query(&expr!("foo")), BindingsSet::single());
    }

    #[test]
    fn test_match_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("foo"));
        assert_eq!(space.query(&expr!(x)), bind_set![{x: expr!("foo")}]);
    }

    #[test]
    fn test_match_expression() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "a" ("*" "b" "c")));
        assert_eq!(space.query(&expr!("+" "a" ("*" "b" "c"))), BindingsSet::single());
    }

    #[test]
    fn test_match_expression_with_variables() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" b c))),
        bind_set![{a: expr!("A"), b: expr!("B"), c: expr!("C") }]);
    }

    #[test]
    fn test_match_different_value_for_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" a c))), BindingsSet::empty());
    }

    #[test]
    fn test_match_query_variable_has_priority() {
        let mut space = GroundingSpace::new();
        space.add(expr!("equals" x x));

        let result = space.query(&expr!("equals" y z));
        assert_eq!(result, bind_set![{ y: expr!(z) }]);
    }

    #[test]
    fn test_match_query_variable_via_data_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!(x x));
        assert_eq!(space.query(&expr!(y (z))), bind_set![{y: expr!((z))}]);
    }

    #[test]
    fn test_match_if_then_with_x() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("if" "True" then) then));
        assert_eq!(space.query(&expr!("=" ("if" "True" "42") X)),
        bind_set![{X: expr!("42")}]);
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
        assert_eq!(result, bind_set![{object: expr!("baloon"), color: expr!("blue")}]);
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
        assert_eq!(result, bind_set![{h: expr!("Socrates"), t: expr!("Nil")}]);
    }

    #[test]
    fn cleanup_observer() {
        let mut space = GroundingSpace::new();
        {
            let _observer = space.common.register_observer(SpaceEventCollector::new());
            assert_eq!(space.common.observers.borrow().len(), 1);
        }

        space.add(expr!("a"));

        assert_eq_no_order!(space, vec![expr!("a")]);
        assert_eq!(space.common.observers.borrow().len(), 0);
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
        assert_eq!(result, bind_set![{x: sym!("Sam"), y: expr!("B" x), z: expr!("C" x)}]);
    }

    #[test]
    fn test_custom_match_with_space() {
        let space = GroundingSpace::from_vec(vec![
            expr!("A" {1} x "a"),
            expr!("B" {1} x "b"),
            expr!("A" {2} x "c"),
        ]);
        let result: BindingsSet = match_atoms(&Atom::gnd(space), &expr!("A" {1} x x)).collect();
        assert_eq!(result, bind_set![{x: sym!("a")}]);
    }

    #[test]
    fn index_atom_to_key() {
        assert_eq!(atom_to_trie_key(&Atom::sym("A")), TrieKey::from([TrieToken::Exact(SymbolAtom::new("A".into()))]));
        assert_eq!(atom_to_trie_key(&Atom::value(1)), TrieKey::from([TrieToken::Wildcard]));
        assert_eq!(atom_to_trie_key(&Atom::var("a")), TrieKey::from([TrieToken::Wildcard]));
        assert_eq!(atom_to_trie_key(&expr!("A" "B")), TrieKey::from([
                TrieToken::LeftPar,
                TrieToken::Exact(SymbolAtom::new("A".into())),
                TrieToken::Exact(SymbolAtom::new("B".into())),
                TrieToken::RightPar
        ]));
    }
}
