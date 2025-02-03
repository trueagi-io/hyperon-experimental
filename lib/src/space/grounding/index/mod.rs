pub mod storage;
pub mod trie;

pub use trie::{ALLOW_DUPLICATION, NO_DUPLICATION, DuplicationStrategy, AllowDuplication, NoDuplication};
use storage::*;
use trie::*;

use crate::atom::*;

use std::fmt::Debug;
use std::borrow::Cow;

// TODO: should we duplicate structure for an owned and borrowed cases to eliminate Cow
#[derive(PartialEq, Debug)]
enum AtomToken<'a> {
    Atom(Cow<'a, Atom>),
    StartExpr(Option<&'a Atom>), // atom is required to match custom entries from index
    EndExpr,
}

#[derive(Default, Debug, Clone)]
enum AtomIterState<'a> {
    /// Start from a Symbol, Variable or Grounded
    StartSingle(Cow<'a, Atom>),
    /// Start from Expression atom
    StartExpression(Cow<'a, Atom>),

    /// Iterate via Expression recursively
    Iterate {
        /// Expression to iterate
        expr: Cow<'a, ExpressionAtom>,
        /// Current index
        idx: usize,
        /// Next state to apply after end of the expression
        next: Box<AtomIterState<'a>>
    },

    #[default]
    /// End of iterator
    End,
}

#[derive(Debug, Clone)]
struct AtomIter<'a> {
    state: AtomIterState<'a>,
}

impl<'a> AtomIter<'a> {
    fn from_ref(atom: &'a Atom) -> Self {
        Self::from_cow(Cow::Borrowed(atom))
    }

    fn from_atom(atom: Atom) -> Self {
        Self::from_cow(Cow::Owned(atom))
    }

    fn from_cow(atom: Cow<'a, Atom>) -> Self {
        let state = match atom {
            Cow::Owned(Atom::Expression(_)) =>
                AtomIterState::StartExpression(atom),
            Cow::Borrowed(Atom::Expression(_)) =>
                AtomIterState::StartExpression(atom),
            _ => AtomIterState::StartSingle(atom),
        };
        Self{ state }
    }
}

impl<'a> Iterator for AtomIter<'a> {
    type Item = AtomToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        type State<'a> = AtomIterState<'a>;

        match std::mem::take(&mut self.state) {
            State::StartSingle(atom) => {
                self.state = State::End;
                Some(AtomToken::Atom(atom))
            },
            State::StartExpression(Cow::Owned(Atom::Expression(expr))) => {
                self.state = State::Iterate {
                    expr: Cow::Owned(expr),
                    idx: 0,
                    next: Box::new(State::End)
                };
                Some(AtomToken::StartExpr(None))
            },
            State::StartExpression(Cow::Borrowed(atom @ Atom::Expression(expr))) => {
                self.state = State::Iterate {
                    expr: Cow::Borrowed(expr),
                    idx: 0,
                    next: Box::new(State::End)
                };
                Some(AtomToken::StartExpr(Some(atom)))
            },
            State::StartExpression(_) => panic!("Only expressions are expected!"),
            State::Iterate { expr, idx, next } => {
                if idx < expr.children().len() {
                    fn extract_atom(mut expr: Cow<'_, ExpressionAtom>, idx: usize) -> (Cow<'_, Atom>, Cow<'_, ExpressionAtom>) {
                        match expr {
                            Cow::Owned(ref mut e) => {
                                let cell = unsafe { e.children_mut().get_unchecked_mut(idx) };
                                let atom = std::mem::replace(cell, Atom::sym(""));
                                (Cow::Owned(atom), expr)
                            },
                            Cow::Borrowed(e) => {
                                let atom = unsafe { e.children().get_unchecked(idx) };
                                (Cow::Borrowed(atom), expr)
                            },
                        }
                    }
                    let (atom, expr) = extract_atom(expr, idx);
                    let next_state = State::Iterate{ expr, idx: idx + 1, next };
                    match atom {
                        Cow::Owned(Atom::Expression(expr)) => {
                            self.state = State::Iterate {
                                expr: Cow::Owned(expr),
                                idx: 0,
                                next: Box::new(next_state)
                            };
                            Some(AtomToken::StartExpr(None))
                        },
                        Cow::Borrowed(atom @ Atom::Expression(expr)) => {
                            self.state = State::Iterate {
                                expr: Cow::Borrowed(expr),
                                idx: 0,
                                next: Box::new(next_state)
                            };
                            Some(AtomToken::StartExpr(Some(atom)))
                        },
                        _ => {
                            self.state = next_state;
                            Some(AtomToken::Atom(atom))
                        },
                    }
                } else {
                    self.state = *next;
                    Some(AtomToken::EndExpr)
                }
            },
            State::End => None,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AtomIndex<D: DuplicationStrategy = NoDuplication> {
    storage: AtomStorage,
    trie: AtomTrie<D>,
}

impl AtomIndex {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<D: DuplicationStrategy> AtomIndex<D> {
    pub fn with_strategy(_strategy: D) -> Self {
        Self::default()
    }

    pub fn insert(&mut self, atom: Atom) {
        let key = AtomIter::from_atom(atom)
            .map(|token| Self::atom_token_to_insert_index_key(&mut self.storage, token));
        self.trie.insert(key)
    }

    fn atom_token_to_insert_index_key<'a>(storage: &mut AtomStorage, token: AtomToken<'a>) -> InsertKey {
        match token {
            AtomToken::Atom(Cow::Owned(atom @ Atom::Variable(_))) => {
                InsertKey::Custom(atom)
            },
            AtomToken::Atom(Cow::Owned(atom)) => {
                match storage.insert(atom) {
                    Ok(id) => InsertKey::Exact(id),
                    Err(atom) => InsertKey::Custom(atom),
                }
            },
            AtomToken::StartExpr(_) => InsertKey::StartExpr,
            AtomToken::EndExpr => InsertKey::EndExpr,
            _ => panic!("Only owned atoms are expected to be inserted"),
        }
    }

    pub fn query(&self, atom: &Atom) -> QueryResult {
        let key = AtomIter::from_ref(&atom)
            .map(|token| Self::atom_token_to_query_index_key(&self.storage, token));
        Box::new(self.trie.query(key, &self.storage).into_iter())
    }

    fn atom_token_to_query_index_key<'a>(storage: &AtomStorage, token: AtomToken<'a>) -> QueryKey<'a> {
        match token {
            AtomToken::Atom(Cow::Borrowed(atom @ Atom::Variable(_))) => {
                QueryKey::Custom(atom)
            },
            AtomToken::Atom(Cow::Borrowed(atom @ Atom::Grounded(gnd)))
                if gnd.as_grounded().as_match().is_some() => {
                QueryKey::Custom(atom)
            },
            AtomToken::Atom(Cow::Borrowed(atom)) => {
                match storage.get_id(atom) {
                    Some(id) => QueryKey::Exact(Some(id), atom),
                    None => QueryKey::Exact(None, atom),
                }
            },
            AtomToken::StartExpr(Some(atom)) => QueryKey::StartExpr(atom),
            AtomToken::EndExpr => QueryKey::EndExpr,
            _ => panic!("Only borrowed atoms are expected to be queried"),
        }
    }

    pub fn remove(&mut self, atom: &Atom) -> bool {
        let key = AtomIter::from_ref(&atom)
            .map(|token| Self::atom_token_to_query_index_key(&self.storage, token));
        self.trie.remove(key)
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item=Cow<'_, Atom>> + '_> {
       self.trie.unpack_atoms(&self.storage)
    }

    pub fn is_empty(&self) -> bool {
        self.trie.is_empty()
    }

    pub fn stats(&self) -> AtomIndexStats {
        AtomIndexStats{
            storage_count: self.storage.count(),
            trie_stats: self.trie.stats(),
        }
    }
}

#[derive(Debug)]
pub struct AtomIndexStats {
    storage_count: usize,
    trie_stats: AtomTrieNodeStats,
}

impl AtomIndexStats {
    pub fn storage_count(&self) -> usize {
        self.storage_count
    }
    pub fn trie_stats(&self) -> &AtomTrieNodeStats {
        &self.trie_stats
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::matcher::*;
    use crate::{expr, sym, bind, assert_eq_no_order};
    use crate::metta::runner::number::Number;
    use std::fmt::{Debug, Display, Formatter};

    #[test]
    fn atom_token_iter_symbol() {
        let atom = Atom::sym("sym");
        let it = AtomIter::from_ref(&atom);
        let actual: Vec<AtomToken> = it.collect();
        assert_eq!(vec![AtomToken::Atom(Cow::Borrowed(&atom))], actual);
    }

    #[test]
    fn atom_token_iter_expr() {
        let atom = expr!(("sym" var {Number::Integer(42)} ("sym1" var1 {Number::Integer(43)})));
        let it = AtomIter::from_ref(&atom);
        let actual: Vec<AtomToken> = it.collect();
        assert_eq!(vec![AtomToken::StartExpr(Some(&atom)), AtomToken::Atom(Cow::Borrowed(&expr!("sym"))),
            AtomToken::Atom(Cow::Borrowed(&expr!(var))), AtomToken::Atom(Cow::Borrowed(&expr!({Number::Integer(42)}))),
            AtomToken::StartExpr(Some(&expr!("sym1" var1 {Number::Integer(43)}))),
            AtomToken::Atom(Cow::Borrowed(&expr!("sym1"))), AtomToken::Atom(Cow::Borrowed(&expr!(var1))),
            AtomToken::Atom(Cow::Borrowed(&expr!({Number::Integer(43)}))),
            AtomToken::EndExpr, AtomToken::EndExpr], actual);
    }

    fn plain_vars(bind: Bindings) -> Bindings {
        fn plain(var: VariableAtom) -> VariableAtom {
            let mut name = var.name();
            match name.find('#') {
                Some(p) => { let _ = name.split_off(p); },
                None => {},
            }
            VariableAtom::new(name)
        }
        let mut res = Bindings::new();
        for (var, mut atom) in bind.iter() {
            let var = plain(var.clone());
            res = match atom {
                Atom::Variable(varval) => res.add_var_equality(&var, &plain(varval)),
                Atom::Expression(_) => {
                    atom.iter_mut().filter_type::<&mut VariableAtom>().for_each(|var| {
                        *var = plain(var.clone())
                    });
                    res.add_var_binding(var, atom)
                },
                _ => res.add_var_binding(var, atom),
            }.expect("Not expected");
        }
        println!("plain_vars: {} -> {}", bind, res);
        res
    }

    macro_rules! assert_eq_bind_no_order {
        ($actual:expr, $expected:expr) => {
            assert_eq_no_order!($actual.map(plain_vars).collect::<Vec<Bindings>>(), $expected);
        }
    }

    #[test]
    fn atom_index_query_single() {
        let mut index = AtomIndex::new();
        index.insert(Atom::sym("A"));
        index.insert(Atom::var("a"));
        index.insert(Atom::gnd(Number::Integer(42)));

        assert_eq_bind_no_order!(index.query(&Atom::sym("A")), vec![bind!{}, bind!{a: Atom::sym("A")}]);
        assert_eq_bind_no_order!(index.query(&Atom::var("b")), vec![bind!{ b: Atom::sym("A") }, bind!{ b: Atom::gnd(Number::Integer(42)) }, bind!{ b: Atom::var("a") }]);
        assert_eq_bind_no_order!(index.query(&Atom::gnd(Number::Integer(42))), vec![bind!{}, bind!{a: Atom::gnd(Number::Integer(42))}]);
        assert_eq_bind_no_order!(index.query(&sym!("B")), vec![bind!{a: Atom::sym("B")}]);
        assert_eq_bind_no_order!(index.query(&Atom::gnd(Number::Integer(43))), vec![bind!{a: Atom::gnd(Number::Integer(43))}]);
    }

    #[test]
    fn atom_index_unique_var_names() {
        let mut index = AtomIndex::new();
        let expected = Atom::var("a");
        index.insert(expected.clone());

        let mut result = index.query(&Atom::var("x"));
        let actual = result.next().expect("Result is empty").resolve(&VariableAtom::new("x")).unwrap();

        assert!(result.next().is_none());
        assert!(atoms_are_equivalent(&actual, &expected));
        assert_ne!(actual, expected);

        let mut index = AtomIndex::new();
        let expected = expr!("A" b "C");
        index.insert(expected.clone());

        let mut result = index.query(&Atom::var("x"));
        let actual = result.next().expect("Result is empty").resolve(&VariableAtom::new("x")).unwrap();

        assert!(result.next().is_none());
        assert!(atoms_are_equivalent(&actual, &expected));
        assert_ne!(actual, expected);
    }

    #[test]
    fn atom_index_query_expression() {
        let mut index = AtomIndex::new();
        index.insert(expr!("A" a {Number::Integer(42)} a));

        assert_eq_bind_no_order!(index.query(&expr!("A" "B" {Number::Integer(42)} "B")), vec![bind!{a: expr!("B")}]);
        assert_eq_bind_no_order!(index.query(&expr!("A" ("B" "C") {Number::Integer(42)} ("B" "C"))), vec![bind!{a: expr!("B" "C")}]);
        assert_eq_bind_no_order!(index.query(&expr!("A" "B" {Number::Integer(42)} "C")), Vec::<Bindings>::new());
        assert_eq_bind_no_order!(index.query(&expr!(b)), vec![bind!{ b: expr!("A" a {Number::Integer(42)} a)}]);
    }

    fn get_atoms<D: DuplicationStrategy>(index: &AtomIndex<D>) -> Vec<Atom> {
        index.iter().map(|a| a.into_owned()).collect()
    }

    #[test]
    fn atom_index_iter_single() {
        let mut index = AtomIndex::new();
        index.insert(Atom::sym("A"));
        index.insert(Atom::var("a"));
        index.insert(Atom::gnd(Number::Integer(42)));

        assert_eq_no_order!(get_atoms(&index), vec![Atom::sym("A"), Atom::var("a"), Atom::gnd(Number::Integer(42))]);
    }

    #[test]
    fn atom_index_iter_expression() {
        let mut index = AtomIndex::new();
        index.insert(expr!("A" a {Number::Integer(42)} a));

        assert_eq_no_order!(get_atoms(&index), vec![expr!("A" a {Number::Integer(42)} a)]);
    }

    #[test]
    fn atom_index_iter_expression_1() {
        let mut index = AtomIndex::new();
        index.insert(expr!("A" "B" "C"));
        index.insert(expr!("A" "D" "C"));

        assert_eq_no_order!(get_atoms(&index), vec![expr!("A" "B" "C"), expr!("A" "D" "C")]);
    }

    #[test]
    fn atom_index_iter_expression_2() {
        let mut index = AtomIndex::new();
        index.insert(expr!(()));
        index.insert(expr!(() "A"));
        index.insert(expr!("A" ("B") "C"));
        index.insert(expr!("A" ("D") "C"));

        assert_eq_no_order!(get_atoms(&index), vec![expr!(()), expr!(() "A"), expr!("A" ("B") "C"), expr!("A" ("D") "C")]);
    }

    fn dup<T: Clone>(mut vec: Vec<T>) -> Vec<T> {
        let copy: Vec<T> = vec.iter().cloned().collect();
        vec.extend(copy);
        vec
    }

    #[test]
    fn atom_index_query_single_duplicate() {
        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        index.insert(Atom::sym("A"));
        index.insert(Atom::sym("A"));
        index.insert(Atom::var("a"));
        index.insert(Atom::var("a"));
        index.insert(Atom::gnd(Number::Integer(42)));
        index.insert(Atom::gnd(Number::Integer(42)));

        assert_eq_bind_no_order!(index.query(&Atom::sym("A")),
            dup(vec![bind!{}, bind!{a: Atom::sym("A")}]));
        assert_eq_bind_no_order!(index.query(&Atom::var("a")),
            dup(vec![bind!{ a: Atom::sym("A") }, bind!{ a: Atom::gnd(Number::Integer(42)) }, bind!{ a: Atom::var("a") }]));
        assert_eq_bind_no_order!(index.query(&Atom::gnd(Number::Integer(42))),
            dup(vec![bind!{}, bind!{a: Atom::gnd(Number::Integer(42))}]));
        assert_eq_bind_no_order!(index.query(&sym!("B")), dup(vec![bind!{a: Atom::sym("B")}]));
        assert_eq_bind_no_order!(index.query(&Atom::var("b")),
            dup(vec![bind!{ b: Atom::sym("A") }, bind!{ b: Atom::gnd(Number::Integer(42)) }, bind!{ b: Atom::var("a") }]));
        assert_eq_bind_no_order!(index.query(&Atom::gnd(Number::Integer(43))),
            dup(vec![bind!{a: Atom::gnd(Number::Integer(43))}]));
    }

    #[test]
    fn atom_index_query_expression_duplicate() {
        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        index.insert(expr!("A" a {Number::Integer(42)} a));
        index.insert(expr!("A" a {Number::Integer(42)} a));

        assert_eq_bind_no_order!(index.query(&expr!("A" "B" {Number::Integer(42)} "B")),
            dup(vec![bind!{a: expr!("B")}]));
        assert_eq_bind_no_order!(index.query(&expr!("A" ("B" "C") {Number::Integer(42)} ("B" "C"))),
            dup(vec![bind!{a: expr!("B" "C")}]));
        assert_eq_bind_no_order!(index.query(&expr!("A" "B" {Number::Integer(42)} "C")),
            dup(Vec::<Bindings>::new()));
        assert_eq_bind_no_order!(index.query(&expr!(b)),
            dup(vec![bind!{ b: expr!("A" a {Number::Integer(42)} a)}]));
    }

    #[test]
    fn atom_index_query_expression_duplicate_1() {
        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        index.insert(expr!("A" ("B" "C") "D"));
        index.insert(expr!("A" ("B" "C") "D"));

        assert_eq_bind_no_order!(index.query(&expr!("A" x "D")),
            vec![bind!{x: expr!("B" "C")}, bind!{x: expr!("B" "C")}]);

        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        index.insert(expr!("A" ("B" "C") "D"));
        index.insert(expr!("A" ("B" "C") "E"));

        assert_eq_bind_no_order!(index.query(&expr!("A" x "D")),
            vec![bind!{x: expr!("B" "C")}]);
        assert_eq_bind_no_order!(index.query(&expr!("A" x y)),
            vec![bind!{x: expr!("B" "C"), y: sym!("D")}, bind!{x: expr!("B" "C"), y: sym!("E")}]);
    }

    #[test]
    fn atom_index_iter_single_duplicate() {
        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        index.insert(Atom::sym("A"));
        index.insert(Atom::sym("A"));
        index.insert(Atom::var("a"));
        index.insert(Atom::var("a"));
        index.insert(Atom::gnd(Number::Integer(42)));
        index.insert(Atom::gnd(Number::Integer(42)));

        assert_eq_no_order!(get_atoms(&index),
            dup(vec![Atom::sym("A"), Atom::var("a"), Atom::gnd(Number::Integer(42))]));
    }

    #[test]
    fn atom_index_iter_expression_duplicate() {
        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        index.insert(expr!(()));
        index.insert(expr!(()));
        index.insert(expr!(() "A"));
        index.insert(expr!(() "A"));
        index.insert(expr!("A" ("B") "C"));
        index.insert(expr!("A" ("B") "C"));

        assert_eq_no_order!(get_atoms(&index), dup(vec![expr!(()), expr!(() "A"), expr!("A" ("B") "C")]));
    }

    #[test]
    fn atom_index_iter_expression_duplicate_no_dup_strategy() {
        let mut index = AtomIndex::with_strategy(NO_DUPLICATION);
        index.insert(expr!(()));
        index.insert(expr!(()));
        index.insert(expr!(() "A"));
        index.insert(expr!(() "A"));
        index.insert(expr!("A" ("B") "C"));
        index.insert(expr!("A" ("B") "C"));

        assert_eq_no_order!(get_atoms(&index), vec![expr!(()), expr!(() "A"), expr!("A" ("B") "C")]);
    }

    #[test]
    fn atom_index_remove_single_duplicate() {
        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        index.insert(Atom::sym("A"));
        index.insert(Atom::sym("A"));
        index.insert(Atom::var("a"));
        index.insert(Atom::var("a"));
        index.insert(Atom::gnd(Number::Integer(42)));
        index.insert(Atom::gnd(Number::Integer(42)));

        assert_eq_no_order!(get_atoms(&index),
            dup(vec![Atom::sym("A"), Atom::var("a"), Atom::gnd(Number::Integer(42))]));

        assert!(index.remove(&Atom::sym("A")));
        assert_eq_no_order!(get_atoms(&index),
            vec![Atom::sym("A"), Atom::var("a"), Atom::var("a"), Atom::gnd(Number::Integer(42)), Atom::gnd(Number::Integer(42))]);

        assert!(index.remove(&Atom::gnd(Number::Integer(42))));
        assert_eq_no_order!(get_atoms(&index),
            vec![Atom::sym("A"), Atom::var("a"), Atom::var("a"), Atom::gnd(Number::Integer(42))]);

        assert!(index.remove(&Atom::var("a")));
        assert_eq_no_order!(get_atoms(&index),
            vec![Atom::sym("A"), Atom::var("a"), Atom::gnd(Number::Integer(42))]);

        assert!(index.remove(&Atom::var("a")));
        assert!(index.remove(&Atom::gnd(Number::Integer(42))));
        assert!(index.remove(&Atom::sym("A")));
        assert!(index.is_empty());
    }

    #[test]
    fn atom_index_remove_expression_duplicate() {
        let mut index = AtomIndex::with_strategy(ALLOW_DUPLICATION);
        index.insert(expr!(()));
        index.insert(expr!(()));
        index.insert(expr!(() "A"));
        index.insert(expr!(() "A"));
        index.insert(expr!("A" b "C"));
        index.insert(expr!("A" b "C"));
        index.insert(expr!("A" ("D") "C"));
        index.insert(expr!("A" ("D") "C"));

        assert_eq_no_order!(get_atoms(&index),
            dup(vec![expr!(()), expr!(() "A"), expr!("A" b "C"), expr!("A" ("D") "C")]));

        assert!(index.remove(&expr!(())));
        assert_eq_no_order!(get_atoms(&index),
            vec![expr!(()), expr!(() "A"), expr!("A" b "C"), expr!("A" ("D") "C"), expr!(() "A"), expr!("A" b "C"), expr!("A" ("D") "C")]);

        assert!(index.remove(&expr!(() "A")));
        assert_eq_no_order!(get_atoms(&index),
            vec![expr!(()), expr!("A" b "C"), expr!("A" ("D") "C"), expr!(() "A"), expr!("A" b "C"), expr!("A" ("D") "C")]);

        assert!(index.remove(&expr!("A" b "C")));
        assert_eq_no_order!(get_atoms(&index),
            vec![expr!(()), expr!("A" ("D") "C"), expr!(() "A"), expr!("A" b "C"), expr!("A" ("D") "C")]);

        assert!(index.remove(&expr!("A" ("D") "C")));
        assert_eq_no_order!(get_atoms(&index),
            vec![expr!(()), expr!(() "A"), expr!("A" b "C"), expr!("A" ("D") "C")]);

        assert!(index.remove(&expr!("A" ("D") "C")));
        assert!(index.remove(&expr!("A" b "C")));
        assert!(index.remove(&expr!(() "A")));
        assert!(index.remove(&expr!(())));
        assert!(index.is_empty());
    }

    #[test]
    fn atom_index_remove_expression_duplicate_no_duplication() {
        let mut index = AtomIndex::with_strategy(NO_DUPLICATION);
        index.insert(expr!(()));
        index.insert(expr!(()));
        index.insert(expr!(() "A"));
        index.insert(expr!(() "A"));
        index.insert(expr!("A" b "C"));
        index.insert(expr!("A" b "C"));
        index.insert(expr!("A" ("D") "C"));
        index.insert(expr!("A" ("D") "C"));

        assert_eq_no_order!(get_atoms(&index),
            vec![expr!(()), expr!(() "A"), expr!("A" b "C"), expr!("A" ("D") "C")]);

        assert!(index.remove(&expr!(())));
        assert_eq_no_order!(get_atoms(&index),
            vec![expr!(() "A"), expr!("A" b "C"), expr!("A" ("D") "C")]);

        assert!(index.remove(&expr!(() "A")));
        assert_eq_no_order!(get_atoms(&index),
            vec![expr!("A" b "C"), expr!("A" ("D") "C")]);

        assert!(index.remove(&expr!("A" b "C")));
        assert_eq_no_order!(get_atoms(&index),
            vec![expr!("A" ("D") "C")]);

        assert!(index.remove(&expr!("A" ("D") "C")));
        assert_eq_no_order!(get_atoms(&index), Vec::<Atom>::new());

        assert!(index.is_empty());

        assert!(!index.remove(&expr!("A" ("D") "C")));
        assert!(!index.remove(&expr!("A" b "C")));
        assert!(!index.remove(&expr!(() "A")));
        assert!(!index.remove(&expr!(())));
    }

    #[derive(PartialEq, Clone, Debug)]
    struct MatchAsX { }

    impl Display for MatchAsX {
        fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
            write!(f, "match-as-x")
        }
    }

    impl CustomMatch for MatchAsX {
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            Box::new(std::iter::once(bind!{ x: other.clone() }))
        }
    }

    impl Grounded for MatchAsX {
        fn type_(&self) -> Atom {
            rust_type_atom::<Self>()
        }

        fn as_match(&self) -> Option<&dyn CustomMatch> {
            Some(self)
        }
    }

    #[test]
    fn atom_index_query_matchable() {
        let mut index = AtomIndex::new();
        index.insert(expr!("A" "B" "C"));
        index.insert(Atom::sym("D"));

        let actual: Vec<_> = index.query(&Atom::gnd(MatchAsX{})).collect();
        assert_eq_no_order!(actual, vec![bind!{ x: sym!("D") },
            bind!{ x: expr!("A" "B" "C") }]);
    }

    #[test]
    fn atom_index_insert_matchable() {
        let mut index = AtomIndex::new();
        index.insert(Atom::gnd(MatchAsX{}));

        let actual: Vec<_> = index.query(&expr!("A" "B" "C")).collect();
        assert_eq_no_order!(actual, vec![bind!{ x: expr!("A" "B" "C") }]);
    }

    #[test]
    fn atom_index_exact_key_size() {
        assert_eq!(std::mem::size_of::<ExactKey>(), std::mem::size_of::<usize>());
    }
}
