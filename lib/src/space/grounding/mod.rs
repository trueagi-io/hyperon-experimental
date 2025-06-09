//! Atomspace implementation with in-memory atom storage

use hyperon_atom::{matcher::BindingsSet, *};
use hyperon_common::FlexRef;
#[cfg(test)]
use hyperon_space::DynSpace;

use std::fmt::{Debug, Display};
use std::collections::HashSet;
use hyperon_space::{complex_query, index::{AllowDuplication, AtomIndex, DuplicationStrategy, ALLOW_DUPLICATION}, Space, SpaceCommon, SpaceEvent, SpaceMut, SpaceVisitor};

// Grounding space

/// In-memory space which can contain grounded atoms.
// TODO: Clone is required by C API
#[derive(Clone)]
pub struct GroundingSpace<D: DuplicationStrategy = AllowDuplication> {
    index: AtomIndex<D>,
    common: SpaceCommon,
    name: Option<String>,
}

impl GroundingSpace {
    /// Constructs new empty space.
    pub fn new() -> Self {
        Self::with_strategy(ALLOW_DUPLICATION)
    }

    /// Constructs space from vector of atoms.
    pub fn from_vec(atoms: Vec<Atom>) -> Self {
        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        for atom in atoms {
            index.insert(atom);
        }
        Self{
            index,
            common: SpaceCommon::default(),
            name: None,
        }
    }
}

impl<D: DuplicationStrategy> GroundingSpace<D> {
    /// Constructs new empty space using duplication strategy.
    pub fn with_strategy(strategy: D) -> Self {
        Self {
            index: AtomIndex::with_strategy(strategy),
            common: SpaceCommon::default(),
            name: None,
        }
    }

    /// Adds `atom` into space.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon_atom::sym;
    /// use hyperon_atom::matcher::BindingsSet;
    /// use hyperon::space::grounding::GroundingSpace;
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
        log::debug!("GroundingSpace::add: {}, atom: {}", self, atom);
        self.index.insert(atom.clone());
        self.common.notify_all_observers(&SpaceEvent::Add(atom));
    }

    /// Removes `atom` from space. Returns true if atom was found and removed,
    /// and false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon_atom::sym;
    /// use hyperon_atom::matcher::BindingsSet;
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    ///
    /// space.remove(&sym!("A"));
    ///
    /// assert_eq!(space.query(&sym!("A")), BindingsSet::empty());
    /// ```
    pub fn remove(&mut self, atom: &Atom) -> bool {
        log::debug!("GroundingSpace::remove: {}, atom: {}", self, atom);
        let is_removed = self.index.remove(atom);
        if is_removed {
            self.common.notify_all_observers(&SpaceEvent::Remove(atom.clone()));
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
    /// use hyperon_atom::sym;
    /// use hyperon_atom::matcher::BindingsSet;
    /// use hyperon::space::grounding::GroundingSpace;
    ///
    /// let mut space = GroundingSpace::from_vec(vec![sym!("A")]);
    ///
    /// space.replace(&sym!("A"), sym!("B"));
    ///
    /// assert_eq!(space.query(&sym!("A")), BindingsSet::empty());
    /// assert_eq!(space.query(&sym!("B")), BindingsSet::single());
    /// ```
    pub fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        let is_replaced = self.index.remove(from);
        if is_replaced {
            self.index.insert(to.clone());
            self.common.notify_all_observers(&SpaceEvent::Replace(from.clone(), to));
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
    /// use hyperon_atom::{expr, bind_set, sym};
    /// use hyperon_atom::matcher::BindingsSet;
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
        complex_query(query, |query| self.single_query(query))
    }

    /// Executes simple `query` without sub-queries on the space.
    fn single_query(&self, query: &Atom) -> BindingsSet {
        log::debug!("GroundingSpace::single_query: {} query: {}", self, query);
        let mut result = BindingsSet::empty();
        let query_vars: HashSet<&VariableAtom> = query.iter().filter_type::<&VariableAtom>().collect();
        for bindings in self.index.query(query) {
            let bindings = bindings.narrow_vars(&query_vars);
            log::trace!("single_query: push result: {}", bindings);
            result.push(bindings);
        }
        log::debug!("GroundinSpace::single_query: {} result: {}", self, result);
        result
    }

    /// Sets the name property for the `GroundingSpace` which can be useful for debugging
    pub fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }

    /// Returns the name property for the `GroundingSpace`, if one has been set
    pub fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|s| s.as_str())
    }

    #[cfg(test)]
    fn into_vec(&self) -> Vec<Atom> {
        self.index.iter().map(|a| a.into_owned()).collect()
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
        Some(self.index.iter().count())
    }
    fn visit(&self, v: &mut dyn SpaceVisitor) -> Result<(), ()> {
       Ok(self.index.iter().for_each(|atom| v.accept(atom)))
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
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
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl<D: DuplicationStrategy> Debug for GroundingSpace<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(f, "GroundingSpace-{name} ({self:p})"),
            None => write!(f, "GroundingSpace-{self:p}")
        }
    }
}

impl<D: DuplicationStrategy> Display for GroundingSpace<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(f, "GroundingSpace-{name}"),
            None => write!(f, "GroundingSpace-{self:p}")
        }
    }
}

#[cfg(test)]
use crate::metta::text::*;

#[cfg(test)]
pub(crate) fn metta_space(text: &str) -> DynSpace {
    let mut space = GroundingSpace::new();
    let mut parser = SExprParser::new(text);
    while let Some(atom) = parser.parse(&Tokenizer::new()).unwrap() {
        space.add(atom);
    }
    space.into()
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use super::*;
    use hyperon_atom::matcher::*;
    use hyperon_common::assert_eq_no_order;
    use hyperon_space::SpaceObserver;

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

        assert_eq_no_order!(space.into_vec(), vec![expr!("a"), expr!("b"), expr!("c")]);
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

        assert_eq_no_order!(space.into_vec(), vec![expr!("a"), expr!("c")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a")),
            SpaceEvent::Add(sym!("b")), SpaceEvent::Add(sym!("c")),
            SpaceEvent::Remove(sym!("b"))]);
    }

    #[test]
    fn remove_duplicated_atom() {
        let mut space = GroundingSpace::new();
        let observer = space.common.register_observer(SpaceEventCollector::new());

        space.add(expr!("a"));
        space.add(expr!("a"));
        space.add(expr!("a"));
        assert_eq!(space.remove(&expr!("a")), true);

        assert_eq_no_order!(space.into_vec(), vec![expr!("a"), expr!("a")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a")),
            SpaceEvent::Add(sym!("a")), SpaceEvent::Add(sym!("a")),
            SpaceEvent::Remove(sym!("a"))]);
    }

    #[test]
    fn remove_atom_not_found() {
        let mut space = GroundingSpace::new();
        let observer = space.common.register_observer(SpaceEventCollector::new());

        space.add(expr!("a"));
        assert_eq!(space.remove(&expr!("b")), false);

        assert_eq_no_order!(space.into_vec(), vec![expr!("a")]);
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

        assert_eq_no_order!(space.into_vec(), vec![expr!("a"), expr!("d"), expr!("c")]);
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

        assert_eq_no_order!(space.into_vec(), vec![expr!("a")]);
        assert_eq!(observer.borrow().events, vec![SpaceEvent::Add(sym!("a"))]);
    }

    #[test]
    fn remove_replaced_atom() {
        let mut space = GroundingSpace::new();
        let observer = space.common.register_observer(SpaceEventCollector::new());

        space.add(expr!("a"));
        space.replace(&expr!("a"), expr!("b"));
        assert_eq!(space.remove(&expr!("b")), true);

        assert_eq_no_order!(space.into_vec(), Vec::<Atom>::new());
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

        assert_eq!(space.atom_count(), Some(0));
    }

    #[test]
    fn iter_after_remove() {
        let mut space = GroundingSpace::from_vec(vec![expr!("a"), expr!("b"), expr!("c")]);
        space.remove(&expr!("b"));

        let mut atoms = Vec::new();
        assert_eq!(Ok(()), space.visit(&mut |atom: Cow<Atom>| atoms.push(atom.into_owned())));
        assert_eq_no_order!(atoms, vec![expr!("a"), expr!("c")]);
    }

    #[test]
    fn mut_cloned_atomspace() {
        let mut first = GroundingSpace::new();
        let mut second = first.clone();

        first.add(expr!("b"));
        second.add(expr!("d"));

        assert_eq_no_order!(first.into_vec(), vec![expr!("b")]);
        assert_eq_no_order!(second.into_vec(), vec![expr!("d")]);
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

        assert_eq_no_order!(space.into_vec(), vec![expr!("a")]);
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
        //assert_eq!(result, bind_set![{x: sym!("Sam"), y: expr!("B" x), z: expr!("C" x)}]);
        assert_eq!(result.len(), 1);
        let result = result.into_iter().next().unwrap();
        assert_eq!(result.resolve(&VariableAtom::new("x")), Some(sym!("Sam")));
        assert_eq!(result.resolve(&VariableAtom::new("y")), Some(expr!("B" "Sam")));
        assert_eq!(result.resolve(&VariableAtom::new("z")), Some(expr!("C" "Sam")));
    }

    #[test]
    fn test_match_results_with_variable_loop() {
        let space = GroundingSpace::from_vec(vec![
            expr!("R" a a),
        ]);
        let result = space.query(&expr!("R" b ("S" b)));
        assert_eq!(result, bind_set![]);
    }

    #[test]
    fn test_custom_match_with_space() {
        let space = GroundingSpace::from_vec(vec![
            expr!("A" {1} x "a"),
            expr!("B" {1} x "b"),
            expr!("A" {2} x "c"),
        ]);
        let result: BindingsSet = match_atoms(&Atom::gnd(DynSpace::new(space)), &expr!("A" {1} x x)).collect();
        assert_eq!(result, bind_set![{x: sym!("a")}]);
    }
}
