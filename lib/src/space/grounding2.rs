use crate::atom::*;
use crate::serial::NullSerializer;
use crate::matcher::*;
use crate::common::CachingMapper;
use crate::common::collections::write_mapping;

use bimap::BiMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::hash::DefaultHasher;
use std::collections::HashMap;
use std::collections::hash_map;
use std::fmt::{Debug, Display, Formatter};
use std::borrow::Cow;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct AtomStorage {
    next_id: usize,
    atoms: BiMap<HashableAtom, usize>,
}

impl AtomStorage {
    pub fn new() -> Self {
        Default::default()
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

impl Display for AtomStorage {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut sorted: Vec<_> = self.atoms.iter()
            .map(|(atom, id)| (id, atom)).collect();
        sorted.sort_unstable_by_key(|(&id, _)| id);
        write_mapping(f, sorted.into_iter())
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

impl Display for HashableAtom {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Store(atom) => Display::fmt(atom, f),
            Self::Query(atom) => write!(f, "{}?", unsafe{ &**atom }),
        }
    }
}

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
        Box::new(self.root.query(key, &self.storage).into_iter())
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

    pub fn iter(&self) -> Box<dyn Iterator<Item=Cow<'_, Atom>> + '_> {
        Box::new(self.root.unpack_atoms(&self.storage).map(|(a, _n)| a))
    }
}

enum InsertKey {
    StartExpr,
    EndExpr,
    Exact(usize),
    Custom(Atom),
}

#[derive(Clone, Debug)]
enum QueryKey<'a> {
    StartExpr(&'a Atom),
    EndExpr,
    Exact(Option<usize>, &'a Atom),
    Custom(&'a Atom),
}

impl<'a> QueryKey<'a> {
    fn as_exact_key(&self) -> Option<ExactKey> {
        match self {
            QueryKey::StartExpr(_) => Some(ExactKey::StartExpr),
            QueryKey::EndExpr => Some(ExactKey::EndExpr),
            QueryKey::Exact(Some(id), _) => Some(ExactKey::Exact(*id)),
            QueryKey::Exact(None, _) => None,
            QueryKey::Custom(_) => None,
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum ExactKey {
    StartExpr,
    EndExpr,
    Exact(usize),
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

    pub fn insert<I: Iterator<Item=InsertKey>>(&mut self, mut key: I) {
        match key.next() {
            Some(head) => match head {
                InsertKey::StartExpr => self.insert_exact(ExactKey::StartExpr, key),
                InsertKey::EndExpr => self.insert_exact(ExactKey::EndExpr, key),
                InsertKey::Exact(id) => self.insert_exact(ExactKey::Exact(id), key),
                InsertKey::Custom(atom) => self.insert_custom(atom, key),
            },
            None => {},
        }
    }

    fn insert_exact<'a, I: Iterator<Item=InsertKey>>(&mut self, head: ExactKey, tail: I) {
        match self.exact.entry(head) {
            hash_map::Entry::Occupied(mut old) => { old.get_mut().insert(tail); },
            hash_map::Entry::Vacant(new) => { new.insert(Self::new_branch(tail)); },
        }
    }

    fn insert_custom<'a, I: Iterator<Item=InsertKey>>(&mut self, atom: Atom, tail: I) {
        for (prev, child) in &mut self.custom {
            if *prev == atom {
                child.insert(tail);
                return;
            }
        }
        self.custom.push((atom, Self::new_branch(tail)));
    }

    fn new_branch<'a, I: Iterator<Item=InsertKey>>(key: I) -> Box<Self> {
        let mut child = Box::new(AtomTrieNode::new());
        child.insert(key);
        child
    }

    // TODO: write an algorithm which returns an iterator instead of collected result
    pub fn query<'a, I: Debug + Clone + Iterator<Item=QueryKey<'a>>>(&self, key: I, storage: &AtomStorage) -> BindingsSet {
        let mut mapper = CachingMapper::new(VariableAtom::make_unique);
        self.query_internal(key, storage, &mut mapper)
    }

    fn query_internal<'a, I: Debug + Clone + Iterator<Item=QueryKey<'a>>, M: Fn(VariableAtom)->VariableAtom>(&self, mut key: I, storage: &AtomStorage, mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet {
        match key.next() {
            Some(head) => match head {
                QueryKey::Exact(_, _) => self.match_exact_key(head, key, storage, mapper),
                QueryKey::StartExpr(_) => self.match_exact_key(head, key, storage, mapper),
                QueryKey::EndExpr => self.match_exact_key(head, key, storage, mapper),
                QueryKey::Custom(atom) => self.match_custom_key(atom, key, storage, mapper),
            },
            None => BindingsSet::single(),
        }
    }

    fn match_exact_key<'a, I, M>(&self, key: QueryKey<'a>, mut tail: I, storage: &AtomStorage,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom,
    {
        let mut result = BindingsSet::empty();
        // match exact entries if applicable
        if let Some(exact) = key.as_exact_key() {
            if let Some(child) = self.exact.get(&exact) {
                result.extend(child.query_internal(tail.clone(), storage, mapper))
            }
        }
        // match custom entries if applicable
        if let Some(atom) = Self::key_to_atom(&key, &mut tail) {
            for (entry, child) in &self.custom {
                let mut custom_res = Self::match_custom_entry(entry, atom, child, tail.clone(), storage, mapper);
                result.extend(custom_res.drain(..));
            }
        }
        result
    }

    fn key_to_atom<'a, I>(key: &QueryKey<'a>, tail: &mut I) -> Option<&'a Atom>
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>
    {
        match key {
            QueryKey::StartExpr(atom) => {
                // TODO: At the moment we pass expression via
                // AtomToken::StartExpr(Some(atom)) -> QueryKey::StartExpr(atom) -> here,
                // because in case of query (when only reference to Atom is
                // passed) we cannot get _reference_ to atom out of the key,
                // while atom is still there represented as a chain of tokens.
                // Maybe using (N, [Atom; N]) encoding can allow solve this
                // issue if it is an issue at all. Also such encoding should
                // allow us skip expressions in a single index addition operation.
                Self::skip_expression(tail);
                Some(atom)
            },
            QueryKey::EndExpr => None,
            QueryKey::Exact(_, atom) => Some(atom),
            QueryKey::Custom(_) => panic!("Custom key is not expected!"),
        }
    }

    fn skip_expression<'a, I: Iterator<Item=QueryKey<'a>>>(key: &mut I) {
        let mut par = 0;
        let mut is_end = |key: &QueryKey| {
            match key {
                QueryKey::StartExpr(_) => {
                    par = par + 1;
                    false
                },
                QueryKey::EndExpr if par == 0 => true,
                QueryKey::EndExpr if par > 0 => {
                    par = par - 1;
                    false
                },
                _ => false,
            }
        };
        loop {
            match key.next() {
                Some(key) => if is_end(&key) { break },
                None => break,
            }
        }
    }

    fn match_custom_entry<'a, I, M>(entry: &Atom, key: &Atom, child: &AtomTrieNode, tail: I,
        storage: &AtomStorage, mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom
    {
        let mut result = BindingsSet::empty();
        let mut entry = entry.clone();
        // TODO: replacing variables each time could be eliminated
        entry.iter_mut().filter_type::<&mut VariableAtom>().for_each(|var| *var = mapper.replace(var.clone()));
        // TODO: conversion to Iterator and back
        result.extend(match_atoms(&entry, key));
        let tail_result = child.query_internal(tail, storage, mapper);
        //log::trace!("match_custom_entry: entry: {}, key: {}, child: {:?}, tail: {:?}, tail_result: {}", entry, key, child, tail.clone(), tail_result);
        // TODO: we could move BindingsSet into merge instead of passing by reference
        result.merge(&tail_result)
    }

    fn match_custom_key<'a, I, M>(&self, key: &Atom, tail: I, storage: &AtomStorage,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom
    {
        let mut result = BindingsSet::empty();
        for (entry, child) in self.unpack_atoms(storage) {
            let mut tail_result = Self::match_custom_entry(&entry, key, child, tail.clone(), storage, mapper);
            result.extend(tail_result.drain(..));
        }
        result
    }

    fn unpack_atoms<'a>(&'a self, storage: &'a AtomStorage) -> Box<dyn Iterator<Item=(Cow<'a, Atom>, &'a AtomTrieNode)> + 'a>{
        Box::new(AtomTrieNodeIter::new(self, storage)
            .filter_map(|(res, child)| {
                if let IterResult::Atom(atom) = res {
                    Some((atom, child))
                } else {
                    None
                }
            }))
    }
}

impl Display for AtomTrieNode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        enum TrieKeyDisplay<'a> {
            Id(usize),
            Atom(&'a Atom),
            Start,
            End,
        }

        impl Display for TrieKeyDisplay<'_> {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                match self {
                    Self::Id(id) => write!(f, "Id({})", id),
                    Self::Atom(atom) => write!(f, "Atom({})", atom),
                    Self::Start => write!(f, "StartExpr",),
                    Self::End => write!(f, "EndExpr",),
                }
            }
        }

        impl TrieKeyDisplay<'_> {
            fn from_exact(key: &ExactKey) -> Self {
                match key {
                    ExactKey::StartExpr => Self::Start,
                    ExactKey::EndExpr => Self::End,
                    ExactKey::Exact(id) => Self::Id(*id),
                }
            }
        }

        let exact = self.exact.iter()
            .map(|(key, child)| (TrieKeyDisplay::from_exact(key), child));
        let custom = self.custom.iter().map(|p| (&p.0, &p.1))
            .map(|(key, child)| (TrieKeyDisplay::Atom(key), child));
        write_mapping(f, exact.chain(custom))
    }
}

#[derive(Debug, Clone)]
struct AtomTrieNodeIter<'a> {
    node: &'a AtomTrieNode,
    storage: &'a AtomStorage,
    state: AtomTrieNodeIterState<'a>,
}

#[derive(Default, Debug, Clone)]
enum AtomTrieNodeIterState<'a> {
    VisitExact(hash_map::Iter<'a, ExactKey, Box<AtomTrieNode>>),
    VisitCustom(std::slice::Iter<'a, (Atom, Box<AtomTrieNode>)>),
    VisitExpression {
        build_expr: Vec<Atom>,
        cur_it: Box<AtomTrieNodeIter<'a>>,
        next_state: Box<Self>,
    },
    #[default]
    End,
}

impl<'a> AtomTrieNodeIter<'a> {
    fn new(node: &'a AtomTrieNode, storage: &'a AtomStorage) -> Self {
        Self {
            node,
            storage,
            state: AtomTrieNodeIterState::VisitExact(node.exact.iter())
        }
    }
}

enum IterResult<'a> {
    EndExpr,
    Atom(Cow<'a, Atom>),
}

impl<'a> Iterator for AtomTrieNodeIter<'a> {
    type Item = (IterResult<'a>, &'a AtomTrieNode);

    fn next(&mut self) -> Option<Self::Item> {
        const ATOM_NOT_IN_STORAGE: &'static str = "Exact entry contains atom which is not in storage";
        type State<'a> = AtomTrieNodeIterState<'a>;

        loop {
            match &mut self.state {
                State::VisitExact(it) => {
                    match it.next() {
                        None => self.state = State::VisitCustom(self.node.custom.iter()),
                        Some((key, child)) => {
                            match key {
                                ExactKey::Exact(id) => {
                                    let atom = self.storage.get_atom(*id).expect(ATOM_NOT_IN_STORAGE);
                                    return Some((IterResult::Atom(Cow::Borrowed(atom)), child.as_ref()));
                                },
                                ExactKey::EndExpr => {
                                    return Some((IterResult::EndExpr, child.as_ref()));
                                },
                                ExactKey::StartExpr => {
                                    let state = std::mem::take(&mut self.state);
                                    self.state = State::VisitExpression{
                                        build_expr: Vec::new(),
                                        cur_it: Box::new(AtomTrieNodeIter::new(child, self.storage)),
                                        next_state: Box::new(state),
                                    }
                                },
                            }
                        },
                    }
                },
                State::VisitCustom(it) => {
                    match it.next() {
                        Some((atom, child)) => return Some((IterResult::Atom(Cow::Borrowed(atom)), child)),
                        None => self.state = State::End,
                    }
                },
                State::VisitExpression { build_expr, cur_it, next_state } => {
                    match cur_it.next() {
                        Some((IterResult::EndExpr, child)) => {
                            let atom = Atom::expr(build_expr.clone());
                            return Some((IterResult::Atom(Cow::Owned(atom)), child))
                        },
                        Some((IterResult::Atom(atom), child)) => {
                            // FIXME: replace Vec by Option<Vec> ?
                            let mut build_expr = std::mem::take(build_expr);
                            build_expr.push(atom.into_owned());
                            let state = std::mem::take(&mut self.state);
                            self.state = State::VisitExpression {
                                build_expr,
                                cur_it: Box::new(AtomTrieNodeIter::new(child, self.storage)),
                                next_state: Box::new(state)
                            }
                        },
                        None => {
                            let mut expr = std::mem::take(build_expr);
                            expr.pop();
                            let mut next_state = std::mem::take(next_state);
                            if let State::VisitExpression { build_expr, cur_it: _, next_state: _ } = &mut *next_state {
                                *build_expr = expr;
                            }
                            self.state = *next_state;
                        }
                    }
                },
                State::End => return None,
            }
        }
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
    fn atom_storage_display() {
        let mut storage = AtomStorage::new();
        assert!(storage.insert(Atom::sym("S")).is_ok());
        assert!(storage.insert(Atom::var("V")).is_ok());
        assert!(storage.insert(Atom::gnd(Number::Integer(42))).is_ok());
        assert_eq!(format!("{}", storage), "{ 0: S, 1: $V, 2: 42 }");
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

    #[test]
    fn atom_index_iter_single() {
        let mut index = AtomIndex::new();
        index.insert(Atom::sym("A"));
        index.insert(Atom::var("a"));
        index.insert(Atom::gnd(Number::Integer(42)));

        let atoms: Vec<Atom> = index.iter().map(|a| a.into_owned()).collect();
        assert_eq_no_order!(atoms, vec![Atom::sym("A"), Atom::var("a"), Atom::gnd(Number::Integer(42))]);
    }

    #[test]
    fn atom_index_iter_expression() {
        let mut index = AtomIndex::new();
        index.insert(expr!("A" a {Number::Integer(42)} a));

        let atoms: Vec<Atom> = index.iter().map(|a| a.into_owned()).collect();
        assert_eq_no_order!(atoms, vec![expr!("A" a {Number::Integer(42)} a)]);
    }

    #[test]
    fn atom_index_iter_expression_1() {
        let mut index = AtomIndex::new();
        index.insert(expr!("A" "B" "C"));
        index.insert(expr!("A" "D" "C"));

        let atoms: Vec<Atom> = index.iter().map(|a| a.into_owned()).collect();
        assert_eq_no_order!(atoms, vec![expr!("A" "B" "C"), expr!("A" "D" "C")]);
    }

    #[test]
    fn atom_index_iter_expression_2() {
        let mut index = AtomIndex::new();
        index.insert(expr!(()));
        index.insert(expr!(() "A"));
        index.insert(expr!("A" ("B") "C"));
        index.insert(expr!("A" ("D") "C"));

        let atoms: Vec<Atom> = index.iter().map(|a| a.into_owned()).collect();
        assert_eq_no_order!(atoms, vec![expr!(()), expr!(() "A"), expr!("A" ("B") "C"), expr!("A" ("D") "C")]);
    }

    #[test]
    fn atom_trie_node_display() {
        let mut node = AtomTrieNode::new();
        node.insert([InsertKey::Exact(1)].into_iter());
        assert_eq!(format!("{}", node), "{ Id(1): { } }");

        let mut node = AtomTrieNode::new();
        node.insert([InsertKey::Custom(Atom::sym("A"))].into_iter());
        assert_eq!(format!("{}", node), "{ Atom(A): { } }");

        let mut node = AtomTrieNode::new();
        node.insert([InsertKey::StartExpr, InsertKey::Custom(Atom::sym("B")), InsertKey::EndExpr].into_iter());
        assert_eq!(format!("{}", node), "{ StartExpr: { Atom(B): { EndExpr: { } } } }");
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
}
