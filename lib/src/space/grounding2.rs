use crate::atom::*;
use crate::serial::NullSerializer;
use crate::matcher::*;
use crate::common::CachingMapper;

use bimap::BiMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::hash::DefaultHasher;
use std::collections::HashMap;
use std::collections::hash_map::Entry as HashMapEntry;
use std::fmt::Debug;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct AtomStorage {
    next_id: usize,
    atoms: BiMap<HashAtom, usize>,
}

impl AtomStorage {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, atom: &Atom) -> Option<usize> {
        if Self::is_hashable(atom) {
            self.insert_internal(atom)
        } else {
            None
        }
    }

    fn is_hashable(atom: &Atom) -> bool {
        match atom {
            Atom::Symbol(_) => true,
            Atom::Variable(_) => true,
            Atom::Grounded(g) if g.as_grounded().as_match().is_none()
                    && Self::is_serializable(&**g) => true,
            _ => false,
        }
    }

    fn insert_internal(&mut self, atom: &Atom) -> Option<usize> {
        let id = match self.atoms.get_by_left(&HashAtom::Query(atom)) {
            Some(id) => *id,
            None => {
                let id = self.next_id();
                self.atoms.insert(HashAtom::Store(atom.clone()), id);
                id
            },
        };
        Some(id)
    }

    #[inline]
    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id = self.next_id + 1;
        id
    }

    pub fn get_atom(&self, id: usize) -> Option<&Atom> {
        self.atoms.get_by_right(&id).map(|h| h.as_atom())
    }

    pub fn get_id(&self, atom: &Atom) -> Option<usize> {
        if Self::is_hashable(atom) {
            self.atoms.get_by_left(&HashAtom::Query(atom)).map(|id| *id)
        } else {
            None
        }
    }

    // TODO: calling Grounded::serialize() is a workaround. We don't know
    // in advance whether atom is serializable and we understand it only
    // after calling Grounded::serialize.
    fn is_serializable(gnd: &dyn GroundedAtom) -> bool {
        match gnd.serialize(&mut NullSerializer::default()) {
            Ok(_) => true,
            Err(_) => false,
        }
    }
}

#[derive(Eq, Debug, Clone)]
enum HashAtom {
    // Used pointer to eliminate lifetime which leaks through the usages.
    // At the same time HashAtom::Query is used only for querying before adding
    // where one can guarantee the reference to the added atom is correct.
    Query(*const Atom),
    Store(Atom),
}

impl HashAtom {
    pub fn as_atom(&self) -> &Atom {
        match self {
            HashAtom::Query(a) => unsafe{ &**a },
            HashAtom::Store(a) => a,
        }
    }
}

impl Hash for HashAtom {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self.as_atom() {
            Atom::Symbol(s) => s.hash(state),
            Atom::Variable(v) => v.hash(state),
            Atom::Grounded(g) => {
                let mut hasher = DefaultHasher::new();
                let _ = g.serialize(&mut hasher).expect("Expected to be hashable");
                state.write_u64(hasher.finish());
            },
            _ => panic!("Not expected"),
        }
    }
}

impl PartialEq for HashAtom {
    fn eq(&self, other: &Self) -> bool {
        self.as_atom() == other.as_atom()
    }
}

#[derive(PartialEq, Debug)]
enum AtomToken<'a> {
    Atom(&'a Atom),
    StartExpr(&'a Atom),
    EndExpr,
}

#[derive(Default)]
enum AtomTokenIterState<'a> {
    Single(&'a Atom),
    Expression(&'a Atom, &'a ExpressionAtom),
    Iterate(&'a ExpressionAtom, usize, Box<AtomTokenIterState<'a>>),
    #[default]
    End,
}

struct AtomTokenIter<'a> {
    state: AtomTokenIterState<'a>,
}

impl<'a> AtomTokenIter<'a> {
    fn new(atom: &'a Atom) -> Self {
        let state = match atom {
            Atom::Symbol(_) | Atom::Variable(_) | Atom::Grounded(_) =>
                AtomTokenIterState::Single(atom),
            Atom::Expression(expr) => 
                AtomTokenIterState::Expression(atom, expr),
        };
        Self{ state }
    }
}

impl<'a> Iterator for AtomTokenIter<'a> {
    type Item = AtomToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::take(&mut self.state) {
            AtomTokenIterState::Single(atom) => {
                self.state = AtomTokenIterState::End;
                Some(AtomToken::Atom(atom))
            },
            AtomTokenIterState::Expression(atom, expr) => {
                self.state = AtomTokenIterState::Iterate(expr, 0, Box::new(AtomTokenIterState::End));
                Some(AtomToken::StartExpr(atom))
            },
            AtomTokenIterState::Iterate(expr, i, prev) => {
                let children = expr.children();
                if i < children.len() {
                    let atom = unsafe{ children.get_unchecked(i) };
                    let next_state = AtomTokenIterState::Iterate(expr, i + 1, prev);
                    match atom {
                        Atom::Symbol(_) | Atom::Variable(_) | Atom::Grounded(_) => {
                            self.state = next_state;
                            Some(AtomToken::Atom(atom))
                        },
                        Atom::Expression(e) => {
                            self.state = AtomTokenIterState::Iterate(e, 0, Box::new(next_state));
                            Some(AtomToken::StartExpr(atom))
                        },
                    }
                } else {
                    self.state = *prev;
                    Some(AtomToken::EndExpr)
                }
            },
            AtomTokenIterState::End => None,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AtomIndex {
    storage: AtomStorage,
    root: AtomIndexNode,
}

impl AtomIndex {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, atom: Atom) {
        let key = AtomTokenIter::new(&atom)
            .map(|token| Self::atom_token_to_insert_index_key(&mut self.storage, token));
        self.root.insert(key)
    }

    // FIXME: could merge two implementations atom_token_to_insert_index_key
    // and atom_token_to_query_index_key
    fn atom_token_to_insert_index_key<'a>(storage: &mut AtomStorage, token: AtomToken<'a>) -> IndexKey<'a> {
        match token {
            AtomToken::Atom(atom @ Atom::Variable(_)) => {
                IndexKey::Custom(atom.clone())
            },
            AtomToken::Atom(atom) => {
                match storage.insert(atom) {
                    Some(id) => IndexKey::Exact(id),
                    // FIXME: add iter to iterate without cloning
                    None => IndexKey::Custom(atom.clone()),
                }
            },
            AtomToken::StartExpr(e) => IndexKey::StartExpr(e),
            AtomToken::EndExpr => IndexKey::EndExpr,
        }
    }

    pub fn query(&self, atom: &Atom) -> QueryResult {
        let key: Vec<IndexKey> = AtomTokenIter::new(&atom)
            .map(|token| Self::atom_token_to_query_index_key(&self.storage, token))
            .collect();
        Box::new(self.root.query(key.into_iter(), &self.storage).into_iter())
    }

    fn atom_token_to_query_index_key<'a>(storage: &AtomStorage, token: AtomToken<'a>) -> IndexKey<'a> {
        match token {
            AtomToken::Atom(atom @ Atom::Variable(_)) => {
                IndexKey::CustomRef(atom)
            },
            AtomToken::Atom(atom) => {
                match storage.get_id(atom) {
                    Some(id) => IndexKey::ExactRef((Some(id), atom)),
                    None => IndexKey::ExactRef((None, atom)),
                }
            },
            AtomToken::StartExpr(e) => IndexKey::StartExpr(e),
            AtomToken::EndExpr => IndexKey::EndExpr,
        }
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item=Atom> + '_> {
        Box::new(self.root.unpack_atoms(&self.storage)
            .filter_map(|(a, _n)| a))
    }
}

#[derive(Clone, Debug)]
enum IndexKey<'a> {
    StartExpr(&'a Atom),
    EndExpr,
    Exact(usize),
    ExactRef((Option<usize>, &'a Atom)),
    Custom(Atom),
    CustomRef(&'a Atom),
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum ExactKey {
    StartExpr,
    EndExpr,
    Exact(usize),
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct AtomIndexNode {
    exact: HashMap<ExactKey, Box<Self>>, 
    custom: Vec<(Atom, Box<Self>)>,
}

type QueryResult = Box<dyn Iterator<Item=Bindings>>;

impl AtomIndexNode {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert<'a, I: Iterator<Item=IndexKey<'a>>>(&mut self, mut key: I) {
        match key.next() {
            Some(head) => match head {
                IndexKey::StartExpr(_) => self.insert_exact(ExactKey::StartExpr, key),
                IndexKey::EndExpr => self.insert_exact(ExactKey::EndExpr, key),
                IndexKey::Exact(id) => self.insert_exact(ExactKey::Exact(id), key),
                IndexKey::Custom(atom) => self.insert_custom(atom, key),
                IndexKey::ExactRef(_) => panic!("Not expected"),
                IndexKey::CustomRef(_) => panic!("Not expected"),
            },
            None => {},
        }
    }

    fn insert_exact<'a, I: Iterator<Item=IndexKey<'a>>>(&mut self, head: ExactKey, tail: I) {
        match self.exact.entry(head) {
            HashMapEntry::Occupied(mut old) => { old.get_mut().insert(tail); },
            HashMapEntry::Vacant(new) => { new.insert(Self::new_branch(tail)); },
        }
    }

    fn insert_custom<'a, I: Iterator<Item=IndexKey<'a>>>(&mut self, atom: Atom, tail: I) {
        for (prev, child) in &mut self.custom {
            if *prev == atom {
                child.insert(tail);
                return;
            }
        }
        self.custom.push((atom, Self::new_branch(tail)));
    }

    fn new_branch<'a, I: Iterator<Item=IndexKey<'a>>>(key: I) -> Box<Self> {
        let mut child = Box::new(AtomIndexNode::new());
        child.insert(key);
        child
    }

    // TODO: write an algorithm which returns an iterator instead of collected result
    pub fn query<'a, I: Debug + Clone + Iterator<Item=IndexKey<'a>>>(&self, key: I, storage: &AtomStorage) -> BindingsSet {
        let mut mapper = CachingMapper::new(VariableAtom::make_unique);
        self.query_internal(key, storage, &mut mapper)
    }

    fn query_internal<'a, I: Debug + Clone + Iterator<Item=IndexKey<'a>>, M: Fn(VariableAtom)->VariableAtom>(&self, mut key: I, storage: &AtomStorage, mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet {
        match key.next() {
            Some(head) => match head {
                IndexKey::ExactRef((id, atom)) => self.match_exact(&id.map(ExactKey::Exact), Some(atom), key, storage, mapper),
                IndexKey::StartExpr(atom) => self.match_exact(&Some(ExactKey::StartExpr), Some(atom), key, storage, mapper),
                IndexKey::EndExpr => self.match_exact(&Some(ExactKey::EndExpr), None, key, storage, mapper),
                IndexKey::CustomRef(atom) => self.match_custom_key(atom, key, storage, mapper),
                IndexKey::Exact(_) => panic!("Not expected"),
                IndexKey::Custom(_) => panic!("Not expected"),
            },
            None => BindingsSet::single(),
        }
    }

    fn match_exact<'a, I: Debug + Clone + Iterator<Item=IndexKey<'a>>, M: Fn(VariableAtom)->VariableAtom>(&self, exact: &Option<ExactKey>, atom: Option<&Atom>, mut tail: I, storage: &AtomStorage, mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet {
        let mut result = BindingsSet::empty();
        if let Some(exact) = exact {
            if let Some(child) = self.exact.get(exact) {
                result.extend(child.query_internal(tail.clone(), storage, mapper))
            }
            if let ExactKey::StartExpr = exact {
                // TODO: we could keep this information in the key itself
                tail = Self::skip_to_end_expr(tail);
            }
        }
        if let Some((atom, tail)) = Self::key_to_atom(exact, atom, tail) {
            for (custom, child) in &self.custom {
                let mut custom_res = Self::match_custom_index(custom, atom, child, tail.clone(), storage, mapper);
                result.extend(custom_res.drain(..));
            }
        }
        result
    }

    fn key_to_atom<'a, 'b, I: Debug + Clone + Iterator<Item=IndexKey<'a>>>(head: &Option<ExactKey>, atom: Option<&'b Atom>, tail: I) -> Option<(&'b Atom, I)> {
        match (head, atom) {
            (None, Some(atom)) => Some((atom, tail)),
            (Some(ExactKey::Exact(_)), Some(atom)) => Some((atom, tail)),
            (Some(ExactKey::StartExpr), Some(atom)) => Some((atom, tail)),
            (Some(ExactKey::EndExpr), None) => None,
            _ => panic!("Unexpected state!"),
        }
    }

    fn match_custom_index<'a, I: Debug + Clone + Iterator<Item=IndexKey<'a>>, M: Fn(VariableAtom)->VariableAtom>(matcher: &Atom, atom: &Atom, child: &AtomIndexNode, tail: I, storage: &AtomStorage, mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet {
        let mut result = BindingsSet::empty();
        let mut matcher = matcher.clone();
        matcher.iter_mut().filter_type::<&mut VariableAtom>().for_each(|var| *var = mapper.replace(var.clone()));
        // TODO: conversion to Iterator and back
        result.extend(match_atoms(&matcher, atom));
        let tail_result = child.query_internal(tail, storage, mapper);
        //log::trace!("match_custom_index: matcher: {}, atom: {}, child: {:?}, tail: {:?}, tail_result: {}", matcher, atom, child, tail.clone(), tail_result);
        // TODO: we could move BindingsSet into merge instead of passing by reference
        result.merge(&tail_result)
    }

    fn skip_to_end_expr<'a, I: Debug + Clone + Iterator<Item=IndexKey<'a>>>(mut key: I) -> I {
        let mut par = 0;
        let mut is_end = |a| {
            match a {
                IndexKey::StartExpr(_) => {
                    par = par + 1;
                    false
                },
                IndexKey::EndExpr if par == 0 => true,
                IndexKey::EndExpr if par > 0 => {
                    par = par - 1;
                    false
                },
                _ => false,
            }
        };
        loop {
            match key.next() {
                Some(atom) => if is_end(atom) { break },
                None => break,
            }
        }
        key
    }

    fn match_custom_key<'a, I: Debug + Clone + Iterator<Item=IndexKey<'a>>, M: Fn(VariableAtom)->VariableAtom>(&self, head: &Atom, tail: I, storage: &AtomStorage, mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet {
        let mut result = BindingsSet::empty();
        for (atom, child) in self.unpack_atoms(storage) {
            if atom.is_none() {
                continue;
            }
            let mut tail_result = Self::match_custom_index(&atom.unwrap(), head, child, tail.clone(), storage, mapper);
            result.extend(tail_result.drain(..));
        }
        result
    }

    fn unpack_atoms<'a>(&'a self, storage: &'a AtomStorage) -> Box<dyn Iterator<Item=(Option<Atom>, &'a AtomIndexNode)> + 'a>{
        let mut result: Vec<(Option<Atom>, &AtomIndexNode)> = Vec::new();
        for (exact, child) in &self.exact {
            match exact {
                ExactKey::Exact(id) => result.push((Some(storage.get_atom(*id).expect("Unexpected state!").clone()), child)),
                ExactKey::EndExpr => result.push((None, child)),
                ExactKey::StartExpr => {
                    let mut exprs: Vec<(Vec<Atom>, &AtomIndexNode)> = Vec::new();
                    exprs.push((Vec::new(), child));
                    loop {
                        let mut next: Vec<(Vec<Atom>, &AtomIndexNode)> = Vec::new();
                        for (expr, child) in exprs.into_iter() {
                            for (atom, child) in child.unpack_atoms(storage) {
                                if let Some(atom) = atom {
                                    let mut new_expr = expr.clone();
                                    new_expr.push(atom);
                                    next.push((new_expr, child));
                                } else {
                                    result.push((Some(Atom::expr(expr.clone())), child))
                                }
                            }
                        }
                        if next.is_empty() {
                            break;
                        }
                        exprs = next;
                    }
                }
            }
        }
        for (custom, child) in &self.custom {
            result.push((Some(custom.clone()), child));
        }
        //log::trace!("unpack_atoms: {:?}", result);
        Box::new(result.into_iter())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{expr, sym, bind, assert_eq_no_order};
    use crate::metta::runner::number::Number;

    #[test]
    fn atom_storage_insert_symbol() {
        let atom = Atom::sym("Symbol");
        let mut storage = AtomStorage::new();
        let id = storage.insert(&atom);
        assert!(id.is_some());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_variable() {
        let atom = Atom::var("Variable");
        let mut storage = AtomStorage::new();
        let id = storage.insert(&atom);
        assert!(id.is_some());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_grounded_value() {
        let atom = Atom::gnd(Number::Integer(1234));
        let mut storage = AtomStorage::new();
        let id = storage.insert(&atom);
        assert!(id.is_some());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_grounded_atom() {
        let mut storage = AtomStorage::new();
        assert!(storage.insert(&Atom::value(1)).is_none());
    }

    #[test]
    fn atom_storage_insert_expression() {
        let mut storage = AtomStorage::new();
        assert!(storage.insert(&expr!("A" b {Number::Integer(1)})).is_none());
    }

    #[test]
    fn atom_token_iter_symbol() {
        let atom = Atom::sym("sym");
        let it = AtomTokenIter::new(&atom);
        let actual: Vec<AtomToken> = it.collect();
        assert_eq!(vec![AtomToken::Atom(&atom)], actual);
    }

    #[test]
    fn atom_token_iter_expr() {
        let atom = expr!(("sym" var {Number::Integer(42)} ("sym1" var1 {Number::Integer(43)})));
        let it = AtomTokenIter::new(&atom);
        let actual: Vec<AtomToken> = it.collect();
        assert_eq!(vec![AtomToken::StartExpr(&atom), AtomToken::Atom(&expr!("sym")),
            AtomToken::Atom(&expr!(var)), AtomToken::Atom(&expr!({Number::Integer(42)})),
            AtomToken::StartExpr(&expr!("sym1" var1 {Number::Integer(43)})),
            AtomToken::Atom(&expr!("sym1")), AtomToken::Atom(&expr!(var1)),
            AtomToken::Atom(&expr!({Number::Integer(43)})),
            AtomToken::EndExpr, AtomToken::EndExpr], actual);
    }

    #[test]
    fn atom_index_query_single() {
        let mut index = AtomIndex::new();
        index.insert(Atom::sym("A"));
        index.insert(Atom::var("a"));
        index.insert(Atom::gnd(Number::Integer(42)));

        assert_eq_no_order!(index.query(&Atom::sym("A")).collect::<Vec<Bindings>>(), vec![bind!{}, bind!{a: Atom::sym("A")}]);
        assert_eq_no_order!(index.query(&Atom::var("a")).collect::<Vec<Bindings>>(), vec![bind!{ a: Atom::sym("A") }, bind!{ a: Atom::gnd(Number::Integer(42)) }, bind!{ a: Atom::var("a") }]);
        assert_eq_no_order!(index.query(&Atom::gnd(Number::Integer(42))).collect::<Vec<Bindings>>(), vec![bind!{}, bind!{a: Atom::gnd(Number::Integer(42))}]);
        assert_eq_no_order!(index.query(&sym!("B")).collect::<Vec<Bindings>>(), vec![bind!{a: Atom::sym("B")}]);
        assert_eq_no_order!(index.query(&Atom::var("b")).collect::<Vec<Bindings>>(), vec![bind!{ b: Atom::sym("A") }, bind!{ b: Atom::gnd(Number::Integer(42)) }, bind!{ b: Atom::var("a") }]);
        assert_eq_no_order!(index.query(&Atom::gnd(Number::Integer(43))).collect::<Vec<Bindings>>(), vec![bind!{a: Atom::gnd(Number::Integer(43))}]);
    }

    #[test]
    fn atom_index_query_expression() {
        let mut index = AtomIndex::new();
        index.insert(expr!("A" a {Number::Integer(42)} a));

        assert_eq_no_order!(index.query(&expr!("A" "B" {Number::Integer(42)} "B")).collect::<Vec<Bindings>>(), vec![bind!{a: expr!("B")}]);
        assert_eq_no_order!(index.query(&expr!("A" ("B" "C") {Number::Integer(42)} ("B" "C"))).collect::<Vec<Bindings>>(), vec![bind!{a: expr!("B" "C")}]);
        assert_eq_no_order!(index.query(&expr!("A" "B" {Number::Integer(42)} "C")).collect::<Vec<Bindings>>(), Vec::<Bindings>::new());
        assert_eq_no_order!(index.query(&expr!(b)).collect::<Vec<Bindings>>(), vec![bind!{ b: expr!("A" a {Number::Integer(42)} a)}]);
    }
}
