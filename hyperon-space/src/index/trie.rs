use super::storage::AtomStorage as HashableStorage;

use hyperon_atom::*;
use hyperon_atom::matcher::*;
use hyperon_common::CachingMapper;
use hyperon_common::holeyvec::HoleyVec;

use std::hash::Hash;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Display};
use std::borrow::Cow;

/// Implementer of the duplication strategy.
pub trait DuplicationStrategyImplementor {
    /// Return reference to the items counter.
    fn dup_counter_mut(&mut self) -> &mut usize;
}

/// Duplication strategy type.
// TODO: modify duplication strategy to be able represent TrieKey::Leaf differently
pub trait DuplicationStrategy: Default {
    fn add_atom(leaf: &mut dyn DuplicationStrategyImplementor);
    fn remove_atom(leaf: &mut dyn DuplicationStrategyImplementor);
}

/// Duplication strategy which forbids duplication.
#[derive(Debug, Default, PartialEq, Clone)]
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

impl Display for NoDuplication {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "NoDuplication")
    }
}

/// Duplication strategy which allows duplication.
#[derive(Debug, Default, PartialEq, Clone)]
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

impl Display for AllowDuplication {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "AllowDuplication")
    }
}

/// [AllowDuplication] strategy instance.
pub const ALLOW_DUPLICATION: AllowDuplication = AllowDuplication{};
/// [NoDuplication] strategy instance.
pub const NO_DUPLICATION: NoDuplication = NoDuplication{};


/// Key for atoms insertion.
#[derive(Debug)]
pub enum InsertKey {
    /// Start of the expression token.
    StartExpr(usize),
    /// Movable atom token.
    Atom(Atom),
}

/// Key for atoms querying.
#[derive(Debug)]
pub enum QueryKey<'a> {
    /// Start expression token with expression to be queried.
    StartExpr(&'a Atom, usize),
    /// Borrowed atom token.
    Atom(&'a Atom),
}

impl QueryKey<'_> {
    /// Consumes key until the end of the current expression. It assumes start
    /// token of the expression is already consumed.
    fn skip_expression<'a, I: Iterator<Item=QueryKey<'a>>>(key: &mut I, mut count: usize) {
        while count > 0 {
            match key.next() {
                Some(QueryKey::StartExpr(_, size)) => count = count + size - 1,
                Some(_) => count -= 1,
                None => break,
            }
        }
    }

}

/// Storage for the atoms from the [AtomTrie] which maps atoms to the [TrieKey] and
/// back. It consists of two storages. One for storing hashable atoms which
/// allows atoms extraction. Second for storing non-hashable atoms which doesn't
/// allow atoms extraction. It also manually maps two special keys: expression
/// start and expression end.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct TrieKeyStorage {
    hashable: HashableStorage,
    vec: HoleyVec<Atom>,
}

impl TrieKeyStorage {
    /// Add key into the storage and return corresponding [TrieKey].
    pub fn insert_key(&mut self, key: InsertKey) -> TrieKey {
        match key {
            InsertKey::StartExpr(size) => TrieKey::start_expr(size),
            InsertKey::Atom(atom @ Atom::Variable(_)) => self.add_atom(AtomMatchMode::Unification, atom),
            InsertKey::Atom(atom @ Atom::Grounded(_)) => {
                match &atom {
                    Atom::Grounded(gnd) =>
                        if gnd.as_grounded().as_match().is_some() {
                            self.add_atom(AtomMatchMode::Unification, atom)
                        } else {
                            self.add_atom(AtomMatchMode::Equality, atom)
                        }
                    _ => unreachable!(),
                }
            }
            InsertKey::Atom(atom) => self.add_atom(AtomMatchMode::Equality, atom),
        }
    }

    fn add_atom(&mut self, match_mode: AtomMatchMode, atom: Atom) -> TrieKey {
        self.hashable.insert(atom).map_or_else(|atom| {
            // There is a trade off between keeping non-hashable but equality
            // matchable keys (for instance grounded atom without both matching
            // and serialization) in a collection for equality matching or in a
            // collection for a unification matching. When they are kept in equality
            // matching collection then each occurrence of such key in query
            // will go through all equality entries + all unifiable entries. If they
            // are kept in a unification matching collection (see TrieNode)
            // then when equality key is matched it is matched via all such keys
            // as well while no unification matching with them is possible. Current
            // code keeps such keys in equality matching collection which slows
            // down their matching. User should be advised to implement
            // serialization for the grounded types which don't have unification
            // matching to make them hashable.
            TrieKey::new(TrieKeyStore::Index, match_mode, self.vec.push(atom))
        }, |hash_id| {
            TrieKey::new(TrieKeyStore::Hash, match_mode, hash_id)
        })
    }
    
    /// Check if key is in the storage and return matching mode, [TrieKey] if it is
    /// possible and reference to the atom.
    pub fn query_key<'a>(&self, key: &QueryKey<'a>) -> (AtomMatchMode, Option<TrieKey>, Option<&'a Atom>) {
        match key {
            QueryKey::StartExpr(atom, size) => (AtomMatchMode::Equality, Some(TrieKey::start_expr(*size)), Some(atom)),
            QueryKey::Atom(atom @ Atom::Variable(_)) => self.get_key(AtomMatchMode::Unification, atom),
            QueryKey::Atom(atom @ Atom::Grounded(gnd)) if gnd.as_grounded().as_match().is_some() =>
                self.get_key(AtomMatchMode::Unification, atom),
            QueryKey::Atom(atom) => self.get_key(AtomMatchMode::Equality, atom),
        }
    }

    fn get_key<'a>(&self, match_mode: AtomMatchMode, atom: &'a Atom) -> (AtomMatchMode, Option<TrieKey>, Option<&'a Atom>) {
        let key = self.hashable.get_id(atom)
            .map(|hash_id| TrieKey::new(TrieKeyStore::Hash, match_mode, hash_id));
        (match_mode, key, Some(atom))
    }

    /// Returns atom from storage by [TrieKey].
    pub unsafe fn get_atom_unchecked(&self, key: TrieKey) -> &Atom {
        match key.store() {
            TrieKeyStore::Hash => self.hashable.get_atom(key.value()).unwrap(),
            TrieKeyStore::Index => self.vec.get_unchecked(key.value()),
        }
    }

    /// Remove [TrieKey] from the store
    pub fn remove_key(&mut self, key: TrieKey) {
        // TODO: Ability to remove hashable keys is to be added. To implement
        // removing hashable keys we need to track how many nodes contains this
        // key. When key is not hashable it is not reused and each instance is
        // kept in exacly one node.
        if key.store() == TrieKeyStore::Index {
            self.vec.remove(key.value());
        }
    }
}

/// Index of the trie node inside collection of the nodes.
type NodeId = usize;

/// Trie to keep and query atoms parameterized by [DuplicationStrategy].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AtomTrie<D: DuplicationStrategy = NoDuplication>{
    keys: TrieKeyStorage,
    nodes: HoleyVec<TrieNode>,
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

    /// New instance of the [AtomTrie] using provided instance of the [DuplicationStrategy].
    pub fn with_strategy(_strategy: D) -> Self {
        Default::default()
    }

    /// Insert list of [InsertKey] into the trie.
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

    /// Query trie using list of the [QueryKey] instances.
    #[inline]
    pub fn query<'a, I: Debug + Clone + Iterator<Item=QueryKey<'a>>>(&self, key: I) -> BindingsSet {
        let mut mapper = CachingMapper::new(|v: &VariableAtom| v.clone().make_unique());
        self.query_internal(self.root, key, &mut mapper)
    }

    // TODO: write an algorithm which returns an iterator instead of collected result
    fn query_internal<'a, I, M>(&self, node_id: NodeId, mut key: I,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(&VariableAtom)->VariableAtom
    {
        match key.next() {
            Some(head) => {
                match self.keys.query_key(&head) {
                    (AtomMatchMode::Equality, head, atom) =>
                        self.match_key_by_equality(node_id, atom, head, key, mapper),
                    (AtomMatchMode::Unification, _head, Some(atom)) =>
                        self.match_key_by_unification(node_id, atom, key, mapper),
                    (AtomMatchMode::Unification, _head, None) => unreachable!(),
                }
            },
            None => {
                BindingsSet::count(self.nodes[node_id].leaf_counter())
            }
        }
    }

    fn match_key_by_equality<'a, I, M>(&self, node_id: NodeId,
        atom: Option<&'a Atom>, key: Option<TrieKey>, mut tail: I,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(&VariableAtom)->VariableAtom,
    {
        let mut result = BindingsSet::empty();
        if let TrieNode::Leaf(_count) = self.nodes[node_id] {
            return result;
        }
        
        if let Some(equality_key) = key {
            if let Some(&child_id) = self.index.get(&(node_id, equality_key)) {
                result.extend(self.query_internal(child_id, tail.clone(), mapper))
            }
        } else {
            // match equality nonhashable key (see TrieKeyStorage::add_atom)
            if let Some(query) = atom {
                let it = self.nodes[node_id].iter_match(AtomMatchMode::Equality)
                    // FIXME: should we check !is_hashable here?
                    .filter(|&(_index, key)| !key.is_start_expr())
                    .map(|(_index, key)| (unsafe{ self.keys.get_atom_unchecked(key) }, key));
                for (entry, key) in it {
                    if entry == query {
                        let child_id = *self.index.get(&(node_id, key)).unwrap();
                        result.extend(self.query_internal(child_id, tail.clone(), mapper));
                    }
                }
            }
        }

        // match unification matching entries with equality key
        if let Some(query) = atom {
            if let Some(expr_size) = key.and_then(|k| k.as_start_expr()) {
                QueryKey::skip_expression(&mut tail, expr_size);
            }
            let it = self.nodes[node_id].iter_match(AtomMatchMode::Unification)
                .map(|(_index, key)| {
                    let entry = unsafe{ self.keys.get_atom_unchecked(key) };
                    let child_id = *self.index.get(&(node_id, key)).unwrap();
                    (entry, child_id)
                });
            for (entry, child_id) in it {
                let mut unify_res = self.unify_entry(entry, query, child_id, tail.clone(), mapper);
                result.extend(unify_res.drain(..));
            }
        }
        result
    }

    fn unify_entry<'a, I, M>(&self, entry: &Atom, key: &Atom, child_id: NodeId, tail: I,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(&VariableAtom)->VariableAtom
    {
        let mut result = BindingsSet::empty();
        let mut entry = entry.clone();
        // TODO: replacing variables each time could be eliminated
        entry.iter_mut().filter_type::<&mut VariableAtom>().for_each(|var| *var = mapper.replace(var));
        // TODO: conversion to Iterator and back
        result.extend(match_atoms(&entry, key));
        if result.is_empty() {
            result
        } else {
            let tail_result = self.query_internal(child_id, tail, mapper);
            // TODO: we could move BindingsSet into merge instead of passing by reference
            result.merge(&tail_result).into_iter().filter(|b| !b.has_loops()).collect()
        }
    }

    fn match_key_by_unification<'a, I, M>(&self, node_id: NodeId,
        atom: &Atom, tail: I,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(&VariableAtom)->VariableAtom
    {
        let mut result = BindingsSet::empty();
        for (entry, child_id) in self.unpack_atoms_internal(node_id) {
            let mut tail_result = self.unify_entry(&entry, atom, child_id, tail.clone(), mapper);
            result.extend(tail_result.drain(..));
        }
        result
    }

    /// Get iterator over atoms of the trie.
    #[inline]
    pub fn unpack_atoms<'a>(&'a self) -> Box<dyn Iterator<Item=Cow<'a, Atom>> + 'a>
    {
        Box::new(self.unpack_atoms_internal(self.root)
            .flat_map(|(atom, child_id)| { 
                std::iter::repeat_n(atom, self.nodes[child_id].leaf_counter())
            }))
    }

    fn unpack_atoms_internal<'a>(&'a self, node_id: NodeId)
        -> Box<dyn Iterator<Item=(Cow<'a, Atom>, NodeId)> + 'a>
    { 
        Box::new(TrieNodeAtomIter::new(self, node_id))
    }

    /// Remove specific list of [QueryKey] from the trie. Each key is matched
    /// by equality.
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
                    (match_mode, Some(key), _atom) => {
                        match self.index.get(&(node_id, key)) {
                            Some(child_id) => {
                                self.nodes[node_id].iter_match(match_mode)
                                    .find(|(_i, k)| *k == key)
                                    .map(|(i, k)| (k, i, *child_id))
                            },
                            None => None,
                        }
                    },
                    (match_mode, None, Some(atom)) => {
                        self.nodes[node_id].iter_match(match_mode)
                            .find(|(_i, k)| atom == unsafe{ self.keys.get_atom_unchecked(*k) })
                            .map(|(i, k)| (k, i, *self.index.get(&(node_id, k)).unwrap()))
                    },
                };
                match entry {
                    Some((child_key, index, child_id)) => {
                        let removed = self.remove_internal(child_id, key);
                        if removed && self.nodes[child_id].is_leaf() {
                            self.index.remove(&(node_id, child_key));
                            self.nodes[node_id].remove_key(child_key, index);
                            self.keys.remove_key(child_key);
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

    /// Return `true` if trie is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.nodes[self.root].is_leaf()
    }
}

impl<D: DuplicationStrategy> Display for AtomTrie<D> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        self.fmt_node(0, self.root, f)
    }
}

impl<D: DuplicationStrategy> AtomTrie<D> {
    fn fmt_node(&self, level: usize, node_id: NodeId, f: &mut Formatter) -> std::fmt::Result {
        let node = &self.nodes[node_id];
        if let TrieNode::Leaf(count) = node {
            write!(f, "{}{}\n", " ".repeat(level), count)
        } else {
            node.iter_all().fold(Ok(()), |rc, (_, key)| {
                let child_id = self.index.get(&(node_id, key)).unwrap();
                rc.and_then(|_| self.fmt_key(level, *child_id, key, f))
                    .and_then(|_| {
                        self.fmt_node(level + 1, *child_id, f)
                    })
            })
        }
    }

    fn fmt_key(&self, level: usize, node_id: NodeId, key: TrieKey, f: &mut Formatter) -> std::fmt::Result {
        match key.as_start_expr() {
            Some(size) => write!(f, "{}({} [{}] <{:?}>\n", " ".repeat(level), size, node_id, key),
            None => {
                let atom = unsafe{ self.keys.get_atom_unchecked(key) };
                write!(f, "{}{} [{}] <{:?}>\n", " ".repeat(level), atom, node_id, key)
            },
        }
    }
}

/// Which storage keeps the value of the key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TrieKeyStore {
    /// Value is kept in the storage for hashable atoms.
    Hash,
    /// Value is kept in the storage for nonhashable atoms.
    Index,
}

/// How atom represented by the key should be matched.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AtomMatchMode {
    /// Atom is matched using equality.
    Equality,
    /// Atom is matched using unification.
    Unification,
}

const BITS_PER_ID: u32 = usize::BITS - 2;

const TK_STORE_MASK: usize = 0b10 << BITS_PER_ID;
const TK_MATCH_MASK: usize = 0b01 << BITS_PER_ID;
const TK_VALUE_MASK: usize = !(0b11 << BITS_PER_ID);

const TK_STORE_HASH: usize = 0b00 << BITS_PER_ID;
const TK_STORE_INDEX: usize = 0b10 << BITS_PER_ID;
const TK_MATCH_EXACT: usize = 0b00 << BITS_PER_ID;
const TK_MATCH_CUSTOM: usize = 0b01 << BITS_PER_ID;

const TK_MAX_EXPRESSION_SIZE: usize = 2048;

/// Compact representation of the atom from the trie. It represents each
/// atom using single [usize] value. It keeps value of the key, key matching
/// mode: equality or unification, key storage mode: hashable or indexed.
#[derive(Hash, Copy, Clone, PartialEq, Eq)]
struct TrieKey(usize);

impl TrieKey {
    #[allow(non_snake_case)]
    #[inline]
    const fn new(store: TrieKeyStore, match_mode: AtomMatchMode, mut value: usize) -> Self {
        let store = match store {
            TrieKeyStore::Hash => TK_STORE_HASH,
            TrieKeyStore::Index => TK_STORE_INDEX,
        };
        let match_mode = match match_mode {
            AtomMatchMode::Equality => TK_MATCH_EXACT,
            AtomMatchMode::Unification => TK_MATCH_CUSTOM,
        };
        value += TK_MAX_EXPRESSION_SIZE;
        assert!(((!TK_VALUE_MASK) & value) == 0);
        Self(store | match_mode | value)
    }

    #[allow(non_snake_case)]
    #[inline]
    const fn start_expr(size: usize) -> Self {
        assert!(size < TK_MAX_EXPRESSION_SIZE);
        Self(TK_STORE_HASH | TK_MATCH_EXACT | size)
    }

    #[inline]
    fn value(&self) -> usize {
        self.0 & TK_VALUE_MASK - TK_MAX_EXPRESSION_SIZE
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
    fn match_mode(&self) -> AtomMatchMode {
        match self.0 & TK_MATCH_MASK {
            TK_MATCH_EXACT => AtomMatchMode::Equality,
            TK_MATCH_CUSTOM => AtomMatchMode::Unification,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn is_start_expr(&self) -> bool {
        self.0 < TK_MAX_EXPRESSION_SIZE
            && self.store() == TrieKeyStore::Hash
            && self.match_mode() == AtomMatchMode::Equality
    }

    #[inline]
    fn as_start_expr(&self) -> Option<usize> {
        if self.is_start_expr() {
            Some(self.0 & TK_VALUE_MASK)
        } else {
            None
        }
    }
}

impl Debug for TrieKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_start_expr() {
            write!(f, "TrieKey{{ expr: {:?} }}", self.as_start_expr().unwrap())
        } else {
            write!(f, "TrieKey{{ match_mode: {:?}, store: {:?}, value: {:?} }}",
                self.match_mode(), self.store(), self.value())
        }
    }
}

/// [AtomTrie] node representation. Node doesn't keep a map between keys and
/// successors. It keeps just a list of keys. The map is kept in the [AtomTrie]
/// itself. It is much more compact memory representation. List of keys in the
/// node is required to iterate over successors.
#[derive(Debug, Clone, PartialEq, Eq)]
enum TrieNode {
    /// Leaf node which contains the counter of atoms.
    Leaf(usize),
    /// Single node which contains the only successor.
    Single(TrieKey),
    /// Collection of the successors.
    Many(Box<TrieNodeKeys>),
}

/// [TrieNode] successors collection which keeps separately keys matched
/// by equality and keys matched by unification.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct TrieNodeKeys {
    equal: smallvec::SmallVec<[TrieKey; 2]>,
    unify: smallvec::SmallVec<[TrieKey; 0]>,
}

impl TrieNodeKeys {
    fn push(&mut self, key: TrieKey) {
        match key.match_mode() {
            AtomMatchMode::Equality => self.equal.push(key),
            AtomMatchMode::Unification => self.unify.push(key),
        }
    }

    fn remove(&mut self, key: TrieKey, index: usize) {
        match key.match_mode() {
            AtomMatchMode::Equality => self.equal.remove(index),
            AtomMatchMode::Unification => self.unify.remove(index),
        };
    }

    fn len(&self) -> usize {
        self.equal.len() + self.unify.len()
    }

    fn single(&self) -> TrieKey {
        if self.equal.len() == 1 {
            self.equal[0]
        } else if self.unify.len() == 1 {
            self.unify[0]
        } else {
            panic!("Collection contains more than 1 item");
        }
    }

    fn iter_match(&self, match_mode: AtomMatchMode) -> std::slice::Iter<'_, TrieKey> {
        match match_mode {
            AtomMatchMode::Equality => self.equal.iter(),
            AtomMatchMode::Unification => self.unify.iter(),
        }
    }
}

impl DuplicationStrategyImplementor for TrieNode {
    fn dup_counter_mut(&mut self) -> &mut usize {
        self.leaf_counter_mut()
    }
}

impl Default for TrieNode {
    fn default() -> Self {
        TrieNode::Leaf(0)
    }
}

impl TrieNode {
    /// Return items counter from leaf node.
    pub fn leaf_counter(&self) -> usize {
        match self {
            TrieNode::Leaf(count) => *count,
            // This invariant holds only for keys which are properly balanced
            // i.e. for keys constructed from atoms
            _ => panic!("Key is expected to end by leaf node"),
        }
    }

    /// Return mutable items counter from leaf node.
    fn leaf_counter_mut(&mut self) -> &mut usize {
        match self {
            TrieNode::Leaf(count) => count,
            // This invariant holds only for keys which are properly balanced
            // i.e. for keys constructed from atoms
            _ => panic!("Key is expected to end by leaf node"),
        }
    }

    /// Return `true` if node is a leaf.
    pub fn is_leaf(&self) -> bool {
        matches!(*self, TrieNode::Leaf(0))
    }

    /// Push new key into a list of successors.
    pub fn push(&mut self, key: TrieKey) {
        match self {
            Self::Leaf(0) => *self = Self::Single(key),
            Self::Leaf(_) => panic!("Unexpected state"),
            Self::Single(prev) => {
                let mut keys: TrieNodeKeys = Default::default();
                keys.push(*prev);
                keys.push(key);
                *self = Self::Many(Box::new(keys))
            },
            Self::Many(keys) => keys.push(key),
        }


    }

    /// Remove key from the node.
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

    /// Iterate over all of keys in the list
    pub fn iter_all(&self) -> std::iter::Chain<TrieNodeIndexIter, TrieNodeIndexIter> {
        self.iter_match(AtomMatchMode::Unification)
            .chain(self.iter_match(AtomMatchMode::Equality))
    }

    /// Iterate over keys which has specified matching mode.
    pub fn iter_match(&self, match_mode: AtomMatchMode) -> TrieNodeIndexIter<'_> {
        match self {
            Self::Leaf(_) => TrieNodeIndexIter::Empty,
            Self::Single(key) => {
                if key.match_mode() == match_mode {
                    TrieNodeIndexIter::Single(*key)
                } else {
                    TrieNodeIndexIter::Empty
                }
            },
            Self::Many(keys) => TrieNodeIndexIter::Many(keys
                    .iter_match(match_mode)
                    .copied()
                    .enumerate())
        }
    }
}

/// Iterator over [TrieNode] keys and corresponding indexes.
#[derive(Debug, Clone)]
enum TrieNodeIndexIter<'a> {
    Empty,
    Single(TrieKey),
    Many(std::iter::Enumerate<std::iter::Copied<std::slice::Iter<'a, TrieKey>>>),
}

impl Iterator for TrieNodeIndexIter<'_> {
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

type TrieNodeIter<'a> = std::iter::Chain<TrieNodeIndexIter<'a>, TrieNodeIndexIter<'a>>;

#[derive(Debug)]
struct Expression {
    count: usize,
    children: Vec<Atom>,
}

// FIXME: split state on expr and single?
#[derive(Debug)]
struct TrieNodeAtomIterState<'a> {
    node: NodeId,
    iter: TrieNodeIter<'a>,
    path: Vec<(NodeId, TrieNodeIter<'a>)>,
    expr: Vec<Expression>,     
}

struct TrieNodeAtomIter<'a, D: DuplicationStrategy> {
    trie: &'a AtomTrie<D>,
    // FIXME: insert content of the struct
    state: TrieNodeAtomIterState<'a>,
}

impl<'a, D: DuplicationStrategy> TrieNodeAtomIter<'a, D> {
    fn new(trie: &'a AtomTrie<D>, node_id: NodeId) -> Self {
        let state = TrieNodeAtomIterState {
            node: node_id,
            iter: trie.nodes[node_id].iter_all(),
            path: Vec::new(),
            expr: Vec::new(),
        };
        Self {
            trie,
            state,
        }
    }

    fn push(&mut self, subnode_id: NodeId, key: TrieKey)  {
        match key.as_start_expr() {
            None => {
                let atom = unsafe{ self.trie.keys.get_atom_unchecked(key) };
                let expr = self.state.expr.last_mut().unwrap();
                expr.children.push(atom.clone());
                expr.count -= 1;
            },
            Some(expr_size) => {
                self.state.expr.push(Expression {
                    count: expr_size,
                    children: Vec::new(),
                });
            },
        }

        loop {
            if self.state.expr.len() == 1
                || self.state.expr.last_mut().unwrap().count > 0 {
                    break
            }
            let expr = self.state.expr.pop().unwrap();
            let last = self.state.expr.last_mut().unwrap();
            last.children.push(Atom::expr(expr.children));
            last.count -= 1;
        }
        
        let prev = std::mem::replace(&mut self.state.iter, self.trie.nodes[subnode_id].iter_all());
        self.state.path.push((self.state.node, prev));
        self.state.node = subnode_id;
    }

    fn pop(&mut self) -> bool {
        match self.state.path.pop() {
            Some(prev) => {
                loop {
                    let last = self.state.expr.last_mut().unwrap();
                    let children = &mut last.children;
                    if let Some(Atom::Expression(_)) = children.last() {
                        last.count += 1;
                        let atom = children.pop().unwrap();
                        let vec = ExpressionAtom::try_from(atom).unwrap().into_children();
                        self.state.expr.push(Expression {
                            count: 0,
                            children: vec
                        });
                    } else {
                        break
                    }
                }

                let expr = self.state.expr.last_mut().unwrap();
                match expr.children.pop() {
                    Some(_) => {
                        expr.count += 1;
                    },
                    None => {
                        self.state.expr.pop();
                    },
                }

                (self.state.node, self.state.iter) = prev;
                true
            },
            None => false,
        }
    }

}

impl<'a, D: DuplicationStrategy> Iterator for TrieNodeAtomIter<'a, D> {
    type Item = (Cow<'a, Atom>, NodeId);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.state.expr.len() == 1 {
                let expr = self.state.expr.last_mut().unwrap();
                if expr.count == 0 {
                    let atom = Atom::expr(expr.children.clone());
                    let node = self.state.node;
                    self.pop();
                    return Some((Cow::Owned(atom), node))
                }
            }

            match self.state.iter.next() {
                None => {
                    if !self.pop() {
                        return None
                    }
                },
                Some((_, key)) => {
                    let sub_node_id = *self.trie.index.get(&(self.state.node, key)).unwrap();
                    if self.state.expr.is_empty() && !key.is_start_expr() {
                        let atom = unsafe{ self.trie.keys.get_atom_unchecked(key) };
                        return Some((Cow::Borrowed(atom), sub_node_id))
                    } else {
                        self.push(sub_node_id, key);
                    }
                },
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn atom_trie_node_size() {
        assert_eq!(std::mem::size_of::<TrieNode>(), 2 * std::mem::size_of::<usize>());
    }

    #[test]
    fn atom_trie_trie_key_size() {
        assert_eq!(std::mem::size_of::<TrieKey>(), std::mem::size_of::<usize>());
    }
}
