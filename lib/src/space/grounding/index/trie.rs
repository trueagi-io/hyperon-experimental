use super::storage::AtomStorage as HashableStorage;

use crate::atom::*;
use crate::matcher::*;
use crate::common::CachingMapper;
use crate::common::holeyvec::HoleyVec;

use std::hash::Hash;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::borrow::Cow;

pub trait DuplicationStrategyImplementor {
    fn dup_counter_mut(&mut self) -> &mut usize;
}

// TODO: modify duplication strategy to be able represent TrieKey::Leaf differently
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


pub enum InsertKey {
    StartExpr,
    EndExpr,
    Atom(Atom),
}

#[derive(Clone, Debug)]
pub enum QueryKey<'a> {
    StartExpr(&'a Atom),
    EndExpr,
    Atom(&'a Atom),
}

impl QueryKey<'_> {
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

}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct TrieKeyStorage {
    hashable: HashableStorage,
    vec: HoleyVec<Atom>,
}

const TK_START_EXPR: TrieKey = TrieKey::new(TrieKeyStore::Hash, TrieKeyMatch::Exact, TK_VALUE_MASK);
const TK_END_EXPR: TrieKey = TrieKey::new(TrieKeyStore::Hash, TrieKeyMatch::Exact, TK_VALUE_MASK - 1);

impl TrieKeyStorage {
    pub fn insert_key(&mut self, key: InsertKey) -> TrieKey {
        match key {
            InsertKey::StartExpr => TK_START_EXPR,
            InsertKey::EndExpr => TK_END_EXPR,
            InsertKey::Atom(atom @ Atom::Variable(_)) => self.add_atom(TrieKeyMatch::Custom, atom),
            InsertKey::Atom(atom @ Atom::Grounded(_)) => {
                match &atom {
                    Atom::Grounded(gnd) =>
                        if gnd.as_grounded().as_match().is_some() {
                            self.add_atom(TrieKeyMatch::Custom, atom)
                        } else {
                            self.add_atom(TrieKeyMatch::Exact, atom)
                        }
                    _ => unreachable!(),
                }
            }
            InsertKey::Atom(atom) => self.add_atom(TrieKeyMatch::Exact, atom),
        }
    }

    fn add_atom(&mut self, match_: TrieKeyMatch, atom: Atom) -> TrieKey {
        self.hashable.insert(atom).map_or_else(|atom| {
            // There is a trade off between keeping non-hashable but exact
            // matchable keys (for instance grounded atom without both matching
            // and serialization) in a collection for exact matching or in a
            // collection for a custom matching. When they are kept in exact
            // matching collection then each occurence of such key in query
            // will go through all exact entries + all custom entries. If they
            // are kept in a custom matching collection (see AtomTrieNode)
            // then when exact key is matched it is matched via all such keys
            // as well while no custom matching with them is possible. Current
            // code keeps such keys in exact matching collection which slows
            // down their matching. User should be advised to implement
            // serialization for the grounded types which don't have custom
            // matching to make them hashable.
            TrieKey::new(TrieKeyStore::Index, match_, self.vec.push(atom))
        }, |hash_id| {
            TrieKey::new(TrieKeyStore::Hash, match_, hash_id)
        })
    }
    
    pub fn query_key<'a>(&self, key: &QueryKey<'a>) -> (TrieKeyMatch, Option<TrieKey>, Option<&'a Atom>) {
        match key {
            QueryKey::StartExpr(atom) => (TrieKeyMatch::Exact, Some(TK_START_EXPR), Some(atom)),
            QueryKey::EndExpr => (TrieKeyMatch::Exact, Some(TK_END_EXPR), None),
            QueryKey::Atom(atom @ Atom::Variable(_)) => self.get_key(TrieKeyMatch::Custom, atom),
            QueryKey::Atom(atom @ Atom::Grounded(gnd)) if gnd.as_grounded().as_match().is_some() =>
                self.get_key(TrieKeyMatch::Custom, atom),
            QueryKey::Atom(atom) => self.get_key(TrieKeyMatch::Exact, atom),
        }
    }

    fn get_key<'a>(&self, match_: TrieKeyMatch, atom: &'a Atom) -> (TrieKeyMatch, Option<TrieKey>, Option<&'a Atom>) {
        let key = self.hashable.get_id(atom)
            .map(|hash_id| TrieKey::new(TrieKeyStore::Hash, match_, hash_id));
        (match_, key, Some(atom))
    }

    pub unsafe fn get_atom_unchecked(&self, key: TrieKey) -> &Atom {
        match key.store() {
            TrieKeyStore::Hash => self.hashable.get_atom(key.value()).unwrap(),
            TrieKeyStore::Index => self.vec.get_unchecked(key.value()),
        }
    }
}

type NodeId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AtomTrie<D: DuplicationStrategy = NoDuplication>{
    keys: TrieKeyStorage,
    nodes: HoleyVec<AtomTrieNode>,
    index: HashMap<(NodeId, TrieKey), NodeId>,
    root: NodeId,
    _phantom: std::marker::PhantomData<D>,
}

impl<D: DuplicationStrategy> Default for AtomTrie<D> {
    fn default() -> Self {
        let mut nodes = HoleyVec::new();
        let root = nodes.push(Default::default());
        Self {
            keys: Default::default(),
            nodes,
            index: HashMap::new(),
            root,
            _phantom: Default::default(),
        }
    }
}

impl<D: DuplicationStrategy> AtomTrie<D> {

    pub fn with_strategy(_strategy: D) -> Self {
        Default::default()
    }

    #[inline]
    pub fn insert<I: Iterator<Item=InsertKey>>(&mut self, key: I) {
        self.insert_internal(self.root, key)
    }

    fn insert_internal<I: Iterator<Item=InsertKey>>(&mut self, node_id: NodeId, mut key: I) {
        match key.next() {
            Some(head) => {
                let head = self.keys.insert_key(head);
                match self.index.get(&(node_id, head)) {
                    Some(child_id) => self.insert_internal(*child_id, key),
                    None => {
                        let child_id = self.new_branch(key);
                        self.nodes[node_id].push(head);
                        self.index.insert((node_id, head), child_id);
                    },
                }
            },
            None => D::add_atom(&mut self.nodes[node_id]),
        }
    }

    fn new_branch<'a, I: Iterator<Item=InsertKey>>(&mut self, key: I) -> NodeId {
        let child_id = self.nodes.push(Default::default());
        self.insert_internal(child_id, key);
        child_id
    }

    #[inline]
    pub fn query<'a, I: Debug + Clone + Iterator<Item=QueryKey<'a>>>(&self, key: I) -> BindingsSet {
        let mut mapper = CachingMapper::new(VariableAtom::make_unique);
        self.query_internal(self.root, key, &mut mapper)
    }

    // TODO: write an algorithm which returns an iterator instead of collected result
    fn query_internal<'a, I, M>(&self, node_id: NodeId, mut key: I,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom
    {
        match key.next() {
            Some(head) => {
                match self.keys.query_key(&head) {
                    (TrieKeyMatch::Exact, head, atom) =>
                        self.match_exact_key(node_id, atom, head, key, mapper),
                    (TrieKeyMatch::Custom, _head, Some(atom)) =>
                        self.match_custom_key(node_id, atom, key, mapper),
                    (TrieKeyMatch::Custom, _head, None) => unreachable!(),
                }
            },
            None => {
                BindingsSet::count(self.nodes[node_id].leaf_counter())
            }
        }
    }

    fn match_exact_key<'a, I, M>(&self, node_id: NodeId,
        atom: Option<&'a Atom>, key: Option<TrieKey>, mut tail: I,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom,
    {
        let mut result = BindingsSet::empty();
        if let AtomTrieNode::Leaf(_count) = self.nodes[node_id] {
            return result;
        }
        
        let mut is_start_expr: bool = false;
        if let Some(exact_key) = key {
            // match exact hashable key
            is_start_expr = exact_key == TK_START_EXPR;
            if let Some(&child_id) = self.index.get(&(node_id, exact_key)) {
                result.extend(self.query_internal(child_id, tail.clone(), mapper))
            }
        } else {
            // match exact nonhashable key (see TrieKeyStorage::add_atom)
            if let Some(query) = atom {
                let it = self.nodes[node_id].iter_match(TrieKeyMatch::Exact)
                    .filter(|&(_index, key)| key != TK_START_EXPR && key != TK_END_EXPR)
                    .map(|(_index, key)| (unsafe{ self.keys.get_atom_unchecked(key) }, key));
                for (entry, key) in it {
                    if entry == query {
                        let child_id = *self.index.get(&(node_id, key)).unwrap();
                        result.extend(self.query_internal(child_id, tail.clone(), mapper));
                    }
                }
            }
        }

        // match custom matching entries with exact key
        if let Some(query) = atom {
            if is_start_expr {
                QueryKey::skip_expression(&mut tail);
            }
            let it = self.nodes[node_id].iter_match(TrieKeyMatch::Custom)
                .map(|(_index, key)| {
                    let entry = unsafe{ self.keys.get_atom_unchecked(key) };
                    let child_id = *self.index.get(&(node_id, key)).unwrap();
                    (entry, child_id)
                });
            for (entry, child_id) in it {
                let mut custom_res = self.match_custom_entry(entry, query, child_id, tail.clone(), mapper);
                result.extend(custom_res.drain(..));
            }
        }
        result
    }

    fn match_custom_entry<'a, I, M>(&self, entry: &Atom, key: &Atom, child_id: NodeId, tail: I,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
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
        if result.is_empty() {
            result
        } else {
            let tail_result = self.query_internal(child_id, tail, mapper);
            // TODO: we could move BindingsSet into merge instead of passing by reference
            result.merge(&tail_result)
        }
    }

    fn match_custom_key<'a, I, M>(&self, node_id: NodeId,
        atom: &Atom, tail: I,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom
    {
        let mut result = BindingsSet::empty();
        for (entry, child_id) in self.unpack_atoms_internal(node_id) {
            let mut tail_result = self.match_custom_entry(&entry, atom, child_id, tail.clone(), mapper);
            result.extend(tail_result.drain(..));
        }
        result
    }

    #[inline]
    pub fn unpack_atoms<'a>(&'a self)
        -> Box<dyn Iterator<Item=Cow<'a, Atom>> + 'a>
    {
        Box::new(self.unpack_atoms_internal(self.root)
            .flat_map(|(atom, child_id)| { 
                std::iter::repeat_n(atom, self.nodes[child_id].leaf_counter())
            }))
    }

    fn unpack_atoms_internal<'a>(&'a self, node_id: NodeId)
        -> Box<dyn Iterator<Item=(Cow<'a, Atom>, NodeId)> + 'a>
    { 
        Box::new(AtomTrieNodeIter::new(self, node_id)
            .filter_map(|(atom, child_id)| {
                if let IterResult::Atom(atom) = atom {
                    Some((atom, child_id))
                } else {
                    None
                }
            }))
    }

    #[inline]
    pub fn remove<'a, I: Iterator<Item=QueryKey<'a>>>(&mut self, key: I) -> bool {
        self.remove_internal(self.root, key)
    }

    pub fn remove_internal<'a, I>(&mut self, node_id: NodeId, mut key: I) -> bool
        where I: Iterator<Item=QueryKey<'a>>
    {
        match key.next() {
            Some(head) => {
                let entry = match self.keys.query_key(&head) {
                    (_match, None, None) => None,
                    (match_, Some(key), _atom) => {
                        match self.index.get(&(node_id, key)) {
                            Some(child_id) => {
                                self.nodes[node_id].iter_match(match_)
                                    .find(|(_i, k)| *k == key)
                                    .map(|(i, k)| (k, i, *child_id))
                            },
                            None => None,
                        }
                    },
                    (match_, None, Some(atom)) => {
                        self.nodes[node_id].iter_match(match_)
                            .find(|(_i, k)| atom == unsafe{ self.keys.get_atom_unchecked(*k) })
                            .map(|(i, k)| (k, i, *self.index.get(&(node_id, k)).unwrap()))
                    },
                };
                match entry {
                    Some((child_key, index, child_id)) => {
                        let removed = self.remove_internal(child_id, key);
                        if removed && self.nodes[child_id].is_empty() {
                            self.index.remove(&(node_id, child_key));
                            self.nodes[node_id].remove_key(child_key, index);
                        }
                        removed
                    },
                    None => false,
                }
            },
            None => {
                D::remove_atom(&mut self.nodes[node_id]);
                true
            },
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.nodes[self.root].is_empty()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TrieKeyStore {
    Hash,
    Index,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TrieKeyMatch {
    Exact,
    Custom,
}

const TK_STORE_MASK: usize = 0b1 << 63;
const TK_MATCH_MASK: usize = 0b1 << 62;
const TK_VALUE_MASK: usize = !(0b11 << 62);

const TK_STORE_HASH: usize = 0b0 << 63;
const TK_STORE_INDEX: usize = 0b1 << 63;
const TK_MATCH_EXACT: usize = 0b0 << 62;
const TK_MATCH_CUSTOM: usize = 0b1 << 62;

#[derive(Hash, Copy, Clone, PartialEq, Eq)]
struct TrieKey(usize);

impl TrieKey {
    #[allow(non_snake_case)]
    #[inline]
    const fn new(store: TrieKeyStore, match_: TrieKeyMatch, value: usize) -> Self {
        let store = match store {
            TrieKeyStore::Hash => TK_STORE_HASH,
            TrieKeyStore::Index => TK_STORE_INDEX,
        };
        let match_ = match match_ {
            TrieKeyMatch::Exact => TK_MATCH_EXACT,
            TrieKeyMatch::Custom => TK_MATCH_CUSTOM,
        };
        assert!(((!TK_VALUE_MASK) & value) == 0);
        Self(store | match_ | value)
    }

    #[inline]
    fn value(&self) -> usize {
        self.0 & TK_VALUE_MASK
    }

    #[inline]
    fn store(&self) -> TrieKeyStore {
        match self.0 & TK_STORE_MASK {
            TK_STORE_HASH => TrieKeyStore::Hash,
            TK_STORE_INDEX => TrieKeyStore::Index,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn match_(&self) -> TrieKeyMatch {
        match self.0 & TK_MATCH_MASK {
            TK_MATCH_EXACT => TrieKeyMatch::Exact,
            TK_MATCH_CUSTOM => TrieKeyMatch::Custom,
            _ => unreachable!(),
        }
    }
}

impl Debug for TrieKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "TrieKey{{ match_: {:?}, store: {:?}, value: {:?} }}",
            self.match_(), self.store(), self.value())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AtomTrieNode {
    Leaf(usize),
    Single(TrieKey),
    Many(Box<AtomTrieNodeKeys>),
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct AtomTrieNodeKeys {
    exact: smallvec::SmallVec<[TrieKey; 2]>,
    custom: smallvec::SmallVec<[TrieKey; 0]>,
}

impl AtomTrieNodeKeys {
    fn push(&mut self, key: TrieKey) {
        match key.match_() {
            TrieKeyMatch::Exact => self.exact.push(key),
            TrieKeyMatch::Custom => self.custom.push(key),
        }
    }

    fn remove(&mut self, key: TrieKey, index: usize) {
        match key.match_() {
            TrieKeyMatch::Exact => self.exact.remove(index),
            TrieKeyMatch::Custom => self.custom.remove(index),
        };
    }

    fn len(&self) -> usize {
        self.exact.len() + self.custom.len()
    }

    fn single(&self) -> TrieKey {
        if self.exact.len() == 1 {
            self.exact[0]
        } else if self.custom.len() == 1 {
            self.custom[0]
        } else {
            panic!("Collection contains more than 1 item");
        }
    }

    fn iter_match(&self, match_: TrieKeyMatch) -> std::slice::Iter<'_, TrieKey> {
        match match_ {
            TrieKeyMatch::Exact => self.exact.iter(),
            TrieKeyMatch::Custom => self.custom.iter(),
        }
    }
}

impl DuplicationStrategyImplementor for AtomTrieNode {
    fn dup_counter_mut(&mut self) -> &mut usize {
        self.leaf_counter_mut()
    }
}

impl Default for AtomTrieNode {
    fn default() -> Self {
        AtomTrieNode::Leaf(0)
    }
}

impl AtomTrieNode {
    pub fn leaf_counter(&self) -> usize {
        match self {
            AtomTrieNode::Leaf(count) => *count,
            // This invariant holds only for keys which are properly balanced
            // i.e. for keys constructed from atoms
            _ => panic!("Key is expected to end by leaf node"),
        }
    }

    fn leaf_counter_mut(&mut self) -> &mut usize {
        match self {
            AtomTrieNode::Leaf(count) => count,
            // This invariant holds only for keys which are properly balanced
            // i.e. for keys constructed from atoms
            _ => panic!("Key is expected to end by leaf node"),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(*self, AtomTrieNode::Leaf(0))
    }

    pub fn push(&mut self, key: TrieKey) {
        match self {
            Self::Leaf(0) => *self = Self::Single(key),
            Self::Leaf(_) => panic!("Unexpected state"),
            Self::Single(prev) => {
                let mut keys: AtomTrieNodeKeys = Default::default();
                keys.push(*prev);
                keys.push(key);
                *self = Self::Many(Box::new(keys))
            },
            Self::Many(keys) => keys.push(key),
        }


    }

    pub fn remove_key(&mut self, key: TrieKey, idx: usize) {
        match self {
            Self::Leaf(_) => panic!("Unexpected state"),
            Self::Single(k) => {
                assert!(idx == 0, "Unexpected state");
                assert!(*k == key);
                *self = Self::default();
            },
            Self::Many(keys) => {
                keys.remove(key, idx);
                if keys.len() == 1 {
                    *self = Self::Single(keys.single());
                }
            }
        }
    }

    pub fn iter_all(&self) -> std::iter::Chain<AtomTrieNodeIndexIter, AtomTrieNodeIndexIter> {
        self.iter_match(TrieKeyMatch::Custom)
            .chain(self.iter_match(TrieKeyMatch::Exact))
    }

    pub fn iter_match(&self, match_: TrieKeyMatch) -> AtomTrieNodeIndexIter<'_> {
        match self {
            Self::Leaf(_) => AtomTrieNodeIndexIter::Empty,
            Self::Single(key) => {
                if key.match_() == match_ {
                    AtomTrieNodeIndexIter::Single(*key)
                } else {
                    AtomTrieNodeIndexIter::Empty
                }
            },
            Self::Many(keys) => AtomTrieNodeIndexIter::Many(keys
                    .iter_match(match_)
                    .copied()
                    .enumerate())
        }
    }
}

#[derive(Clone)]
enum AtomTrieNodeIndexIter<'a> {
    Empty,
    Single(TrieKey),
    Many(std::iter::Enumerate<std::iter::Copied<std::slice::Iter<'a, TrieKey>>>),
}

impl Iterator for AtomTrieNodeIndexIter<'_> {
    type Item = (usize, TrieKey);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Single(key) => {
                let key = *key;
                *self = Self::Empty;
                Some((0, key))
            },
            Self::Many(it) => {
                it.next()
            }
        }
    }
}

#[derive(Clone)]
struct AtomTrieNodeIter<'a, D: DuplicationStrategy> {
    trie: &'a AtomTrie<D>,
    node_id: NodeId,
    state: AtomTrieNodeIterState<'a, D>,
}

#[derive(Default, Clone)]
enum AtomTrieNodeIterState<'a, D: DuplicationStrategy> {
    VisitEntries(std::iter::Chain<AtomTrieNodeIndexIter<'a>, AtomTrieNodeIndexIter<'a>>),
    VisitExpression {
        build_expr: Vec<Atom>,
        cur_it: Box<AtomTrieNodeIter<'a, D>>,
        next_state: Box<Self>,
    },
    #[default]
    End,
}

impl<'a, D: DuplicationStrategy> AtomTrieNodeIter<'a, D> {
    fn new(trie: &'a AtomTrie<D>, node_id: NodeId) -> Self {
        Self {
            trie,
            node_id,
            state: AtomTrieNodeIterState::VisitEntries(trie.nodes[node_id].iter_all()),
        }
    }

    fn single_step(&mut self, key: TrieKey) -> Option<(IterResult<'a>, NodeId)> {
        type State<'a, D> = AtomTrieNodeIterState<'a, D>;

        let child_id = *self.trie.index.get(&(self.node_id, key)).unwrap();
        if key == TK_END_EXPR {
            Some((IterResult::EndExpr, child_id))
        } else if key == TK_START_EXPR {
            let state = std::mem::take(&mut self.state);
            self.state = State::VisitExpression{
                build_expr: Vec::new(),
                cur_it: Box::new(AtomTrieNodeIter::new(self.trie, child_id)),
                next_state: Box::new(state),
            };
            None
        } else {
            let atom = unsafe{ self.trie.keys.get_atom_unchecked(key) };
            Some((IterResult::Atom(Cow::Borrowed(atom)), child_id))
        }
    }
}

#[derive(Debug)]
enum IterResult<'a> {
    EndExpr,
    Atom(Cow<'a, Atom>),
}

impl<'a, D: DuplicationStrategy> Iterator for AtomTrieNodeIter<'a, D> {
    type Item = (IterResult<'a>, NodeId);

    fn next(&mut self) -> Option<Self::Item> {
        type State<'a, D> = AtomTrieNodeIterState<'a, D>;

        loop {
            match &mut self.state {
                State::VisitEntries(it) => {
                    match it.next() {
                        Some((_i, key)) => {
                            match self.single_step(key) {
                                Some(result) => return Some(result),
                                None => {},
                            }
                        }
                        None => self.state = State::End,
                    }
                },
                State::VisitExpression { build_expr, cur_it, next_state } => {
                    match cur_it.next() {
                        Some((IterResult::EndExpr, child_id)) => {
                            let atom = Atom::expr(build_expr.clone());
                            return Some((IterResult::Atom(Cow::Owned(atom)), child_id))
                        },
                        Some((IterResult::Atom(atom), child_id)) => {
                            // FIXME: replace Vec by Option<Vec> ?
                            let mut build_expr = std::mem::take(build_expr);
                            build_expr.push(atom.into_owned());
                            let state = std::mem::take(&mut self.state);
                            self.state = State::VisitExpression {
                                build_expr,
                                cur_it: Box::new(AtomTrieNodeIter::new(self.trie, child_id)),
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

    #[test]
    fn atom_trie_node_size() {
        assert_eq!(std::mem::size_of::<AtomTrieNode>(), 2 * std::mem::size_of::<usize>());
    }

    #[test]
    fn atom_trie_custom_exact_key_size() {
        assert_eq!(std::mem::size_of::<TrieKey>(), std::mem::size_of::<usize>());
    }
}
