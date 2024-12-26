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
use std::borrow::Cow;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct AtomStorage {
    next_id: usize,
    atoms: BiMap<HashableAtom, usize>,
}

impl AtomStorage {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, atom: Atom) -> Result<usize, Atom> {
        if Self::is_hashable(&atom) {
            Ok(self.insert_internal(atom))
        } else {
            Err(atom)
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

    fn insert_internal(&mut self, atom: Atom) -> usize {
        match self.atoms.get_by_left(&HashableAtom::Query(&atom)) {
            Some(id) => *id,
            None => {
                let id = self.next_id();
                self.atoms.insert(HashableAtom::Store(atom), id);
                id
            },
        }
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
            self.atoms.get_by_left(&HashableAtom::Query(atom)).map(|id| *id)
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
enum HashableAtom {
    // Used pointer to eliminate lifetime which leaks through the usages.
    // At the same time HashableAtom::Query is used only for querying before adding
    // where one can guarantee the reference to the added atom is correct.
    Query(*const Atom),
    Store(Atom),
}

impl HashableAtom {
    pub fn as_atom(&self) -> &Atom {
        match self {
            HashableAtom::Query(a) => unsafe{ &**a },
            HashableAtom::Store(a) => a,
        }
    }
}

impl Hash for HashableAtom {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self.as_atom() {
            Atom::Symbol(s) => s.hash(state),
            Atom::Variable(v) => v.hash(state),
            Atom::Grounded(g) => {
                let mut hasher = DefaultHasher::new();
                let _ = g.serialize(&mut hasher).expect("Expected to be serializable");
                state.write_u64(hasher.finish());
            },
            _ => panic!("Not expected"),
        }
    }
}

impl PartialEq for HashableAtom {
    fn eq(&self, other: &Self) -> bool {
        self.as_atom() == other.as_atom()
    }
}

// TODO: should we duplicate structure for an owned and borrowed cases to eliminate Cow
// FIXME: rename to AtomIterItem? or AtomTrieToken or TrieToken?
#[derive(PartialEq, Debug)]
enum AtomToken<'a> {
    Atom(Cow<'a, Atom>),
    StartExpr(Option<&'a Atom>), // atom is required to match custom atoms in index
    EndExpr,
}

#[derive(Default, Debug, Clone)]
enum AtomIterState<'a> {
    /// Iterate a Symbol, Variable or Grounded
    StartSingle(Cow<'a, Atom>),
    /// Iterate Expression atom
    StartExpression(Cow<'a, Atom>),

    // Iterate via Expression
    Iterate(Cow<'a, ExpressionAtom>, usize, Box<AtomIterState<'a>>),
    // End of the iteration
    #[default]
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
        match std::mem::take(&mut self.state) {
            AtomIterState::StartSingle(atom) => {
                self.state = AtomIterState::End;
                Some(AtomToken::Atom(atom))
            },
            AtomIterState::StartExpression(Cow::Owned(Atom::Expression(expr))) => {
                self.state = AtomIterState::Iterate(Cow::Owned(expr), 0, Box::new(AtomIterState::End));
                Some(AtomToken::StartExpr(None))
            },
            AtomIterState::StartExpression(Cow::Borrowed(atom @ Atom::Expression(expr))) => {
                self.state = AtomIterState::Iterate(Cow::Borrowed(expr), 0, Box::new(AtomIterState::End));
                Some(AtomToken::StartExpr(Some(atom)))
            },
            AtomIterState::StartExpression(_) => panic!("Only expressions are expected!"),
            AtomIterState::Iterate(expr, i, prev) => {
                if i < expr.children().len() {
                    fn extract_atom(mut expr: Cow<'_, ExpressionAtom>, i: usize) -> (Cow<'_, Atom>, Cow<'_, ExpressionAtom>) {
                        match expr {
                            Cow::Owned(ref mut e) => {
                                let cell = unsafe { e.children_mut().get_unchecked_mut(i) };
                                let atom = std::mem::replace(cell, Atom::sym(""));
                                (Cow::Owned(atom), expr)
                            },
                            Cow::Borrowed(e) => {
                                let atom = unsafe { e.children().get_unchecked(i) };
                                (Cow::Borrowed(atom), expr)
                            },
                        }
                    }
                    let (atom, expr) = extract_atom(expr, i);
                    let next_state = AtomIterState::Iterate(expr, i + 1, prev);
                    match atom {
                        Cow::Owned(Atom::Expression(expr)) => {
                            self.state = AtomIterState::Iterate(Cow::Owned(expr), 0, Box::new(next_state));
                            Some(AtomToken::StartExpr(None))
                        },
                        Cow::Borrowed(atom @ Atom::Expression(expr)) => {
                            self.state = AtomIterState::Iterate(Cow::Borrowed(expr), 0, Box::new(next_state));
                            Some(AtomToken::StartExpr(Some(atom)))
                        },
                        _ => {
                            self.state = next_state;
                            Some(AtomToken::Atom(atom))
                        },
                    }
                } else {
                    self.state = *prev;
                    Some(AtomToken::EndExpr)
                }
            },
            AtomIterState::End => None,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AtomIndex {
    storage: AtomStorage,
    root: AtomTrieNode,
}

impl AtomIndex {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, atom: Atom) {
        let key = AtomIter::from_atom(atom)
            .map(|token| Self::atom_token_to_insert_index_key(&mut self.storage, token));
        self.root.insert(key)
    }

    fn atom_token_to_insert_index_key<'a>(storage: &mut AtomStorage, token: AtomToken<'a>) -> IndexKey<'a> {
        match token {
            AtomToken::Atom(Cow::Owned(atom @ Atom::Variable(_))) => {
                IndexKey::Custom(atom)
            },
            AtomToken::Atom(Cow::Owned(atom)) => {
                match storage.insert(atom) {
                    Ok(id) => IndexKey::ExactId(id),
                    Err(atom) => IndexKey::Custom(atom),
                }
            },
            AtomToken::StartExpr(atom) => IndexKey::StartExpr(atom),
            AtomToken::EndExpr => IndexKey::EndExpr,
            _ => panic!("Only owned atoms are expected to be inserted"),
        }
    }

    pub fn query(&self, atom: &Atom) -> QueryResult {
        let key = AtomIter::from_ref(&atom)
            .map(|token| Self::atom_token_to_query_index_key(&self.storage, token));
        Box::new(self.root.query(key, &self.storage).into_iter())
    }

    fn atom_token_to_query_index_key<'a>(storage: &AtomStorage, token: AtomToken<'a>) -> IndexKey<'a> {
        match token {
            AtomToken::Atom(Cow::Borrowed(atom @ Atom::Variable(_))) => {
                // FIXME: here CustomRef is returned only for variables but it
                // should also be returned for grounded atoms implementing
                // Grounded::as_match, because for these atoms index should be
                // unpacked and every unpacked atom or expression should be
                // matched with a key. See Self::match_custom_key function.
                IndexKey::CustomRef(atom)
            },
            AtomToken::Atom(Cow::Borrowed(atom)) => {
                match storage.get_id(atom) {
                    Some(id) => IndexKey::ExactRef((Some(id), atom)),
                    None => IndexKey::ExactRef((None, atom)),
                }
            },
            AtomToken::StartExpr(atom) => IndexKey::StartExpr(atom),
            AtomToken::EndExpr => IndexKey::EndExpr,
            _ => panic!("Only borrowed atoms are expected to be queried"),
        }
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item=Atom> + '_> {
        Box::new(self.root.unpack_atoms(&self.storage)
            .filter_map(|(a, _n)| a))
    }
}

// TODO: ma be we should split this structure on InsertKey and QueryKey
#[derive(Clone, Debug)]
enum IndexKey<'a> {
    StartExpr(Option<&'a Atom>), // FIXME: should we divide this key on insert and match?
    EndExpr,
    ExactId(usize),
    // FIXME: looks like ExactRef is not needed, CustomRef should be used instead?
    ExactRef((Option<usize>, &'a Atom)),
    /// Atom to be added into a custom part of index
    Custom(Atom),
    /// Atom to be matched with each of atoms unpacked from index
    CustomRef(&'a Atom),
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum ExactKey {
    StartExpr,
    EndExpr,
    ExactId(usize),
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct AtomTrieNode {
    exact: HashMap<ExactKey, Box<Self>>, 
    custom: Vec<(Atom, Box<Self>)>,
}

type QueryResult = Box<dyn Iterator<Item=Bindings>>;

impl AtomTrieNode {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert<'a, I: Iterator<Item=IndexKey<'a>>>(&mut self, mut key: I) {
        match key.next() {
            Some(head) => match head {
                IndexKey::StartExpr(_) => self.insert_exact(ExactKey::StartExpr, key),
                IndexKey::EndExpr => self.insert_exact(ExactKey::EndExpr, key),
                IndexKey::ExactId(id) => self.insert_exact(ExactKey::ExactId(id), key),
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
        let mut child = Box::new(AtomTrieNode::new());
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
                IndexKey::ExactRef((id, atom)) => self.match_exact(&id.map(ExactKey::ExactId), Some(atom), key, storage, mapper),
                IndexKey::StartExpr(atom) => self.match_exact(&Some(ExactKey::StartExpr), atom, key, storage, mapper),
                IndexKey::EndExpr => self.match_exact(&Some(ExactKey::EndExpr), None, key, storage, mapper),
                IndexKey::CustomRef(atom) => self.match_custom_key(atom, key, storage, mapper),
                IndexKey::ExactId(_) => panic!("Not expected"),
                IndexKey::Custom(_) => panic!("Not expected"),
            },
            None => BindingsSet::single(),
        }
    }

    fn match_exact<'a, I, M>(&self, exact: &Option<ExactKey>, atom: Option<&Atom>, tail: I,
        storage: &AtomStorage, mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=IndexKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom,
    {
        let mut result = BindingsSet::empty();
        if let Some(exact) = exact {
            if let Some(child) = self.exact.get(exact) {
                result.extend(child.query_internal(tail.clone(), storage, mapper))
            }
        }
        if let Some((atom, tail)) = Self::key_to_atom(exact, atom, tail) {
            for (entry, child) in &self.custom {
                let mut custom_res = Self::match_custom_entry(entry, atom, child, tail.clone(), storage, mapper);
                result.extend(custom_res.drain(..));
            }
        }
        result
    }

    fn key_to_atom<'a, 'b, I>(exact: &Option<ExactKey>, atom: Option<&'b Atom>, tail: I) -> Option<(&'b Atom, I)>
        where
            I: Debug + Clone + Iterator<Item=IndexKey<'a>>
    {
        match (exact, atom) {
            (None, Some(atom)) => Some((atom, tail)),
            (Some(ExactKey::ExactId(_)), Some(atom)) => Some((atom, tail)),
            (Some(ExactKey::StartExpr), Some(atom)) => {
                // TODO: At the moment we pass expression via
                // AtomToken::StartExpr(Some(atom)) -> IndexKey::StartExpr(Some(atom)) -> here,
                // because in case of query (when only reference to Atom is
                // passed) we cannot get atom out of the key, while it is still
                // there represented as a chain of tokens. Maybe using (N, [Atom; N])
                // encoding can allow solve this issue if it is an issue at all.
                // Also such encoding should allow us skip expressions in a single
                // index addition operation.
                let tail = Self::skip_expression(tail);
                Some((atom, tail))
            },
            (Some(ExactKey::EndExpr), None) => None,
            _ => panic!("Unexpected state!"),
        }
    }

    fn match_custom_entry<'a, I, M>(entry: &Atom, key: &Atom, child: &AtomTrieNode, tail: I,
        storage: &AtomStorage, mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=IndexKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom
    {
        let mut result = BindingsSet::empty();
        let mut entry = entry.clone();
        entry.iter_mut().filter_type::<&mut VariableAtom>().for_each(|var| *var = mapper.replace(var.clone()));
        // TODO: conversion to Iterator and back
        result.extend(match_atoms(&entry, key));
        let tail_result = child.query_internal(tail, storage, mapper);
        //log::trace!("match_custom_entry: entry: {}, key: {}, child: {:?}, tail: {:?}, tail_result: {}", entry, key, child, tail.clone(), tail_result);
        // TODO: we could move BindingsSet into merge instead of passing by reference
        result.merge(&tail_result)
    }

    fn skip_expression<'a, I>(mut key: I) -> I
        where I: Debug + Clone + Iterator<Item=IndexKey<'a>>
    {
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

    fn match_custom_key<'a, I, M>(&self, key: &Atom, tail: I, storage: &AtomStorage,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=IndexKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom
    {
        let mut result = BindingsSet::empty();
        for (entry, child) in self.unpack_atoms(storage) {
            if let Some(entry) = entry {
                let mut tail_result = Self::match_custom_entry(&entry, key, child, tail.clone(), storage, mapper);
                result.extend(tail_result.drain(..));
            }
        }
        result
    }

    fn unpack_atoms<'a>(&'a self, storage: &'a AtomStorage) -> Box<dyn Iterator<Item=(Option<Atom>, &'a AtomTrieNode)> + 'a>{
        let mut result: Vec<(Option<Atom>, &AtomTrieNode)> = Vec::new();
        for (exact, child) in &self.exact {
            match exact {
                ExactKey::ExactId(id) => result.push((Some(storage.get_atom(*id).expect("Unexpected state!").clone()), child)),
                ExactKey::EndExpr => result.push((None, child)),
                ExactKey::StartExpr => {
                    let mut exprs: Vec<(Vec<Atom>, &AtomTrieNode)> = Vec::new();
                    exprs.push((Vec::new(), child));
                    loop {
                        let mut next: Vec<(Vec<Atom>, &AtomTrieNode)> = Vec::new();
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
        let id = storage.insert(atom.clone());
        assert!(id.is_ok());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_variable() {
        let atom = Atom::var("Variable");
        let mut storage = AtomStorage::new();
        let id = storage.insert(atom.clone());
        assert!(id.is_ok());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_grounded_value() {
        let atom = Atom::gnd(Number::Integer(1234));
        let mut storage = AtomStorage::new();
        let id = storage.insert(atom.clone());
        assert!(id.is_ok());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_grounded_atom() {
        let mut storage = AtomStorage::new();
        assert_eq!(storage.insert(Atom::value(1)), Err(Atom::value(1)));
    }

    #[test]
    fn atom_storage_insert_expression() {
        let mut storage = AtomStorage::new();
        assert_eq!(storage.insert(expr!("A" b {Number::Integer(1)})),
            Err(expr!("A" b {Number::Integer(1)})));
    }

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
