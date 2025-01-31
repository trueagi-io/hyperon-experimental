use crate::atom::*;
use crate::serial::NullSerializer;
use crate::matcher::*;
use crate::common::CachingMapper;
use crate::common::collections::write_mapping;
use crate::common::vecondemand::VecOnDemand;

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
    #[cfg(test)]
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

    pub fn count(&self) -> usize {
        self.atoms.left_values().count()
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
pub struct AtomIndex<D: DuplicationStrategy = NoDuplication> {
    storage: AtomStorage,
    root: AtomTrieNode<D>,
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

    pub fn remove(&mut self, atom: &Atom) -> bool {
        let key = AtomIter::from_ref(&atom)
            .map(|token| Self::atom_token_to_query_index_key(&self.storage, token));
        self.root.remove(key, &self.storage)
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item=Cow<'_, Atom>> + '_> {
        Box::new(self.root.unpack_atoms(&self.storage)
            .flat_map(|(a, leaf)| { std::iter::repeat_n(a, leaf.leaf_counter()) }))
    }

    pub fn is_empty(&self) -> bool {
        self.root.is_empty()
    }

    pub fn stats(&self) -> AtomIndexStats {
        AtomIndexStats{
            storage_count: self.storage.count(),
            trie_stats: self.root.stats(),
        }
    }
}

pub trait DuplicationStrategyImplementor {
    fn dup_counter_mut(&mut self) -> &mut usize;
}

pub trait DuplicationStrategy: Default {
    fn add_atom(leaf: &mut dyn DuplicationStrategyImplementor);
    fn remove_atom(leaf: &mut dyn DuplicationStrategyImplementor);
}

#[derive(Default, PartialEq, Clone)]
pub struct NoDuplication {}
impl DuplicationStrategy for NoDuplication {
    fn add_atom(leaf: &mut dyn DuplicationStrategyImplementor) {
        let count = leaf.dup_counter_mut();
        *count = 1;
    }
    fn remove_atom(leaf: &mut dyn DuplicationStrategyImplementor) {
        let count = leaf.dup_counter_mut();
        *count = 0;
    }
}

#[derive(Default, PartialEq, Clone)]
pub struct AllowDuplication {}
impl DuplicationStrategy for AllowDuplication {
    fn add_atom(leaf: &mut dyn DuplicationStrategyImplementor) {
        let count = leaf.dup_counter_mut();
        *count += 1;
    }
    fn remove_atom(leaf: &mut dyn DuplicationStrategyImplementor) {
        let count = leaf.dup_counter_mut();
        *count -= 1;
    }
}

pub const ALLOW_DUPLICATION: AllowDuplication = AllowDuplication{};
pub const NO_DUPLICATION: NoDuplication = NoDuplication{};


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
            QueryKey::StartExpr(_) => Some(ExactKey::START_EXPR),
            QueryKey::EndExpr => Some(ExactKey::END_EXPR),
            QueryKey::Exact(Some(id), _) => Some(ExactKey(*id)),
            QueryKey::Exact(None, _) => None,
            QueryKey::Custom(_) => None,
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct ExactKey(usize);

impl ExactKey {
    const START_EXPR: ExactKey = ExactKey(usize::MAX - 1);
    const END_EXPR: ExactKey = ExactKey(usize::MAX);
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AtomTrieNode<D: DuplicationStrategy = NoDuplication> {
    Node(Box<AtomTrieNodeContent<D>>),
    Leaf(usize),
}

impl<D: DuplicationStrategy> DuplicationStrategyImplementor for AtomTrieNode<D> {
    fn dup_counter_mut(&mut self) -> &mut usize {
        self.leaf_counter_mut()
    }
}

impl<D: DuplicationStrategy> Default for AtomTrieNode<D> {
    fn default() -> Self {
        AtomTrieNode::Leaf::<D>(0)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct AtomTrieNodeContent<D: DuplicationStrategy> {
    exact: HashMap<ExactKey, AtomTrieNode<D>>,
    custom: VecOnDemand<(Atom, AtomTrieNode<D>)>,
    _phantom: std::marker::PhantomData<D>,
}

type QueryResult = Box<dyn Iterator<Item=Bindings>>;

impl AtomTrieNode {
    #[cfg(test)]
    pub fn new() -> Self {
        Self::default()
    }
}

impl<D: DuplicationStrategy> AtomTrieNode<D> {
    pub fn with_strategy(_strategy: D) -> Self {
        Self::default()
    }

    pub fn insert<I: Iterator<Item=InsertKey>>(&mut self, mut key: I) {
        match key.next() {
            Some(head) => match head {
                InsertKey::StartExpr => self.insert_exact(ExactKey::START_EXPR, key),
                InsertKey::EndExpr => self.insert_exact(ExactKey::END_EXPR, key),
                InsertKey::Exact(id) => self.insert_exact(ExactKey(id), key),
                InsertKey::Custom(atom) => self.insert_custom(atom, key),
            },
            None => D::add_atom(self),
        }
    }

    fn content_mut(&mut self) -> &mut AtomTrieNodeContent<D> {
        if let AtomTrieNode::Leaf(count) = *self {
            assert_eq!(count, 0);
            *self = AtomTrieNode::Node(Default::default());
        }
        match self {
            AtomTrieNode::Node(content) => content,
            AtomTrieNode::Leaf(_) => panic!("Not expected"),
        }
    }

    fn insert_exact<'a, I: Iterator<Item=InsertKey>>(&mut self, head: ExactKey, tail: I) {
        match self.content_mut().exact.entry(head) {
            hash_map::Entry::Occupied(mut old) => { old.get_mut().insert(tail); },
            hash_map::Entry::Vacant(new) => { new.insert(Self::new_branch(tail)); },
        }
    }

    fn insert_custom<'a, I: Iterator<Item=InsertKey>>(&mut self, atom: Atom, tail: I) {
        let content = self.content_mut();
        for (prev, child) in &mut content.custom {
            if *prev == atom {
                child.insert(tail);
                return;
            }
        }
        content.custom.push((atom, Self::new_branch(tail)));
    }

    fn new_branch<'a, I: Iterator<Item=InsertKey>>(key: I) -> Self {
        let mut child = AtomTrieNode::with_strategy(D::default());
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
            None => {
                BindingsSet::count(self.leaf_counter())
            }
        }
    }

    fn match_exact_key<'a, I, M>(&self, key: QueryKey<'a>, mut tail: I, storage: &AtomStorage,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom,
    {
        let mut result = BindingsSet::empty();
        let content = match self {
            AtomTrieNode::Leaf(_count) => return result,
            AtomTrieNode::Node(content) => content,
        };
        // match exact entries if applicable
        if let Some(exact) = key.as_exact_key() {
            if let Some(child) = content.exact.get(&exact) {
                result.extend(child.query_internal(tail.clone(), storage, mapper))
            }
        }
        // match custom entries if applicable
        if let Some(atom) = Self::key_to_atom(&key, &mut tail) {
            for (entry, child) in &content.custom {
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

    fn match_custom_entry<'a, I, M>(entry: &Atom, key: &Atom, child: &AtomTrieNode<D>, tail: I,
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

    fn unpack_atoms<'a>(&'a self, storage: &'a AtomStorage) -> Box<dyn Iterator<Item=(Cow<'a, Atom>, &'a AtomTrieNode<D>)> + 'a>{
        Box::new(AtomTrieNodeIter::new(self, storage)
            .filter_map(|(res, child)| {
                if let IterResult::Atom(atom) = res {
                    Some((atom, child))
                } else {
                    None
                }
            }))
    }

    pub fn remove<'a, I: Iterator<Item=QueryKey<'a>>>(&mut self, mut key: I, storage: &AtomStorage) -> bool {
        enum Found {
            Exact(ExactKey),
            Custom(usize),
        }

        match key.next() {
            Some(head) => {
                let content = match self {
                    AtomTrieNode::Leaf(_count) => return false,
                    AtomTrieNode::Node(content) => content,
                };
                let entry = match head {
                    QueryKey::StartExpr(_) => content.exact
                        .get_mut(&ExactKey::START_EXPR)
                        .map(|child| (Found::Exact(ExactKey::START_EXPR), child)),
                    QueryKey::EndExpr => content.exact
                        .get_mut(&ExactKey::END_EXPR)
                        .map(|child| (Found::Exact(ExactKey::END_EXPR), child)),
                    QueryKey::Exact(None, _) => None,
                    QueryKey::Exact(Some(id), _) => content.exact
                        .get_mut(&ExactKey(id))
                        .map(|child| (Found::Exact(ExactKey(id)), child)),
                    QueryKey::Custom(key) => content.custom.iter()
                        .position(|(atom, _child)| atom == key)
                        .map(|i| (Found::Custom(i), &mut unsafe{ content.custom.get_unchecked_mut(i) }.1)),
                };
                match entry {
                    Some((child_key, child)) => {
                        let removed = child.remove(key, storage);
                        if child.is_empty() {
                            match child_key {
                                Found::Exact(key) => { content.exact.remove(&key); },
                                // FIXME: replace self.custom by HoleyVec
                                Found::Custom(i) => { content.custom.remove(i); },
                            }
                        }
                        if content.exact.is_empty() && content.custom.is_empty() {
                            *self = AtomTrieNode::Leaf(0);
                        }
                        removed
                    },
                    None => false,
                }
            },
            None => {
                D::remove_atom(self);
                true
            },
        }
    }

    fn leaf_counter(&self) -> usize {
        match self {
            AtomTrieNode::Leaf(count) => *count,
            // This invariant holds only for keys which are properly balanced
            // i.e. for keys constructed from atoms
            AtomTrieNode::Node(_) => panic!("Key is expected to end by leaf node"),
        }
    }

    fn leaf_counter_mut(&mut self) -> &mut usize {
        match self {
            AtomTrieNode::Leaf(count) => count,
            // This invariant holds only for keys which are properly balanced
            // i.e. for keys constructed from atoms
            AtomTrieNode::Node(_) => panic!("Key is expected to end by leaf node"),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(*self, AtomTrieNode::Leaf(0))
    }

    pub fn stats(&self) -> AtomTrieNodeStats {
        let content = match self {
            AtomTrieNode::Leaf(_count) => return AtomTrieNodeStats{ exact: 0, custom: 0, leaf: 1 },
            AtomTrieNode::Node(content) => content,
        };
        let mut exact = content.exact.keys().count();
        let mut custom = content.custom.len();
        let mut leaf = 0usize;
        let children = content.exact.iter().map(|(_, c)| c)
            .chain(content.custom.iter().map(|(_, c)| c));
        for child in children {
            let AtomTrieNodeStats{ exact: e, custom: c, leaf: l} = child.stats();
            exact += e;
            custom += c;
            leaf += l;
        }
        AtomTrieNodeStats{ exact, custom, leaf }
    }
}

#[derive(Debug)]
pub struct AtomTrieNodeStats {
    exact: usize,
    custom: usize,
    leaf: usize,
}

impl AtomTrieNodeStats {
    pub fn exact(&self) -> usize {
        self.exact
    }
    pub fn custom(&self) -> usize {
        self.custom
    }
    pub fn leaf(&self) -> usize {
        self.leaf
    }
}

impl<D: DuplicationStrategy> Display for AtomTrieNode<D> {
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
                    key if *key == ExactKey::START_EXPR => Self::Start,
                    key if *key == ExactKey::END_EXPR => Self::End,
                    ExactKey(id) => Self::Id(*id),
                }
            }
        }

        match self {
            AtomTrieNode::Node(content) => {
                let exact = content.exact.iter()
                    .map(|(key, child)| (TrieKeyDisplay::from_exact(key), child));
                let custom = content.custom.iter().map(|p| (&p.0, &p.1))
                    .map(|(key, child)| (TrieKeyDisplay::Atom(key), child));
                write_mapping(f, exact.chain(custom))
            },
            AtomTrieNode::Leaf(count) => {
                if *count > 1 {
                    write!(f, "{{ x{} }}", count)
                } else {
                    write!(f, "{{}}")
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
struct AtomTrieNodeIter<'a, D: DuplicationStrategy> {
    node: &'a AtomTrieNode<D>,
    storage: &'a AtomStorage,
    state: AtomTrieNodeIterState<'a, D>,
}

#[derive(Default, Debug, Clone)]
enum AtomTrieNodeIterState<'a, D: DuplicationStrategy> {
    VisitExact(hash_map::Iter<'a, ExactKey, AtomTrieNode<D>>),
    VisitCustom(std::slice::Iter<'a, (Atom, AtomTrieNode<D>)>),
    VisitExpression {
        build_expr: Vec<Atom>,
        cur_it: Box<AtomTrieNodeIter<'a, D>>,
        next_state: Box<Self>,
    },
    #[default]
    End,
}

impl<'a, D: DuplicationStrategy> AtomTrieNodeIter<'a, D> {
    fn new(node: &'a AtomTrieNode<D>, storage: &'a AtomStorage) -> Self {
        let state = match node {
            AtomTrieNode::Node(content) => AtomTrieNodeIterState::VisitExact(content.exact.iter()),
            AtomTrieNode::Leaf(_count) => AtomTrieNodeIterState::End,
        };
        Self {
            node,
            storage,
            state,
        }
    }
}

enum IterResult<'a> {
    EndExpr,
    Atom(Cow<'a, Atom>),
}

impl<'a, D: DuplicationStrategy> Iterator for AtomTrieNodeIter<'a, D> {
    type Item = (IterResult<'a>, &'a AtomTrieNode<D>);

    fn next(&mut self) -> Option<Self::Item> {
        const ATOM_NOT_IN_STORAGE: &'static str = "Exact entry contains atom which is not in storage";
        type State<'a, D> = AtomTrieNodeIterState<'a, D>;

        loop {
            match &mut self.state {
                State::VisitExact(it) => {
                    let content = match self.node {
                        AtomTrieNode::Node(content) => content,
                        AtomTrieNode::Leaf(_count) => panic!("Not expected"),
                    };
                    match it.next() {
                        None => self.state = State::VisitCustom(content.custom.iter()),
                        Some((key, child)) => {
                            match key {
                                key if *key == ExactKey::END_EXPR => {
                                    return Some((IterResult::EndExpr, child));
                                },
                                key if *key == ExactKey::START_EXPR => {
                                    let state = std::mem::take(&mut self.state);
                                    self.state = State::VisitExpression{
                                        build_expr: Vec::new(),
                                        cur_it: Box::new(AtomTrieNodeIter::new(child, self.storage)),
                                        next_state: Box::new(state),
                                    }
                                },
                                ExactKey(id) => {
                                    // FIXME: here we can get in unchecked way
                                    let atom = self.storage.get_atom(*id).expect(ATOM_NOT_IN_STORAGE);
                                    return Some((IterResult::Atom(Cow::Borrowed(atom)), child));
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
    fn atom_trie_node_display() {
        let mut node = AtomTrieNode::new();
        node.insert([InsertKey::Exact(1)].into_iter());
        assert_eq!(format!("{}", node), "{ Id(1): {} }");

        let mut node = AtomTrieNode::new();
        node.insert([InsertKey::Custom(Atom::sym("A"))].into_iter());
        assert_eq!(format!("{}", node), "{ Atom(A): {} }");

        let mut node = AtomTrieNode::new();
        node.insert([InsertKey::StartExpr, InsertKey::Custom(Atom::sym("B")), InsertKey::EndExpr].into_iter());
        assert_eq!(format!("{}", node), "{ StartExpr: { Atom(B): { EndExpr: {} } } }");
    }

    #[test]
    fn atom_trie_node_display_duplicate() {
        let mut node = AtomTrieNode::with_strategy(ALLOW_DUPLICATION);
        node.insert([InsertKey::Custom(Atom::sym("A"))].into_iter());
        node.insert([InsertKey::Custom(Atom::sym("A"))].into_iter());
        assert_eq!(format!("{}", node), "{ Atom(A): { x2 } }");

        let mut node = AtomTrieNode::with_strategy(ALLOW_DUPLICATION);
        node.insert([InsertKey::StartExpr, InsertKey::Custom(Atom::sym("B")), InsertKey::EndExpr].into_iter());
        node.insert([InsertKey::StartExpr, InsertKey::Custom(Atom::sym("B")), InsertKey::EndExpr].into_iter());
        assert_eq!(format!("{}", node), "{ StartExpr: { Atom(B): { EndExpr: { x2 } } } }");
    }

    fn get_atoms<D: DuplicationStrategy>(index: &AtomIndex<D>) -> Vec<Atom> {
        index.iter().map(|a| a.into_owned()).collect()
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

    #[test]
    fn atom_trie_node_size() {
        assert_eq!(std::mem::size_of::<AtomTrieNode<AllowDuplication>>(), 2 * std::mem::size_of::<usize>());
    }
}
