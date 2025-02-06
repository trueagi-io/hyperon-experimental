use super::storage::AtomStorage;

use crate::atom::*;
use crate::matcher::*;
use crate::common::CachingMapper;
use crate::common::collections::write_mapping;
use crate::common::holeyvec::HoleyVec;

use std::hash::Hash;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::borrow::Cow;

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


pub enum InsertKey {
    StartExpr,
    EndExpr,
    Exact(usize),
    Custom(Atom),
}

#[derive(Clone, Debug)]
pub enum QueryKey<'a> {
    StartExpr(&'a Atom),
    EndExpr,
    Exact(Option<usize>, &'a Atom),
    Custom(&'a Atom),
}

impl<'a> QueryKey<'a> {
    fn as_exact_key(&self) -> Option<CustomExact> {
        match self {
            QueryKey::StartExpr(_) => Some(CustomExact::StartExpr()),
            QueryKey::EndExpr => Some(CustomExact::EndExpr()),
            QueryKey::Exact(Some(id), _) => Some(CustomExact::Exact(*id)),
            QueryKey::Exact(None, _) => None,
            QueryKey::Custom(_) => None,
        }
    }
}

type NodeId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AtomTrie<D: DuplicationStrategy = NoDuplication>{
    nodes: HoleyVec<AtomTrieNode>,
    index: HashMap<(NodeId, CustomExact), NodeId>,
    custom: HoleyVec<Atom>,
    root: NodeId,
    _phantom: std::marker::PhantomData<D>,
}

impl<D: DuplicationStrategy> Default for AtomTrie<D> {
    fn default() -> Self {
        let mut nodes = HoleyVec::new();
        let root = nodes.push(Default::default());
        Self {
            nodes,
            index: HashMap::new(),
            custom: HoleyVec::new(),
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
            // FIXME: can be improved by converting InsertKey into CustomExact
            Some(head) => match head {
                InsertKey::StartExpr => self.insert_exact(node_id, CustomExact::StartExpr(), key),
                InsertKey::EndExpr => self.insert_exact(node_id, CustomExact::EndExpr(), key),
                InsertKey::Exact(id) => self.insert_exact(node_id, CustomExact::Exact(id), key),
                InsertKey::Custom(atom) => self.insert_custom(node_id, atom, key),
            },
            None => D::add_atom(self.node_mut(node_id)),
        }
    }

    fn insert_exact<'a, I: Iterator<Item=InsertKey>>(&mut self, node_id: NodeId, head: CustomExact, tail: I) {
        match self.index.get(&(node_id, head)) {
            Some(child_id) => self.insert_internal(*child_id, tail),
            None => {
                self.nodes[node_id].push(head);
                let child_id = self.new_branch(tail);
                self.index.insert((node_id, head), child_id);
            },
        }
    }

    fn insert_custom<'a, I: Iterator<Item=InsertKey>>(&mut self, node_id: NodeId, atom: Atom, tail: I) {
        match self.find_custom(node_id, &atom) {
            Some(child_id) => self.insert_internal(child_id, tail),
            None => {
                let id = self.custom.push(atom);
                let key = CustomExact::Custom(id);
                self.nodes[node_id].push(key);
                let child_id = self.new_branch(tail);
                self.index.insert((node_id, key), child_id);
            },
        }
    }

    #[inline]
    fn custom_key(&self, key: CustomExact) -> &Atom {
        assert!(key.is_custom());
        unsafe{ self.custom.get_unchecked(key.value()) }
    }

    #[inline]
    fn custom_entry(&self, node_id: NodeId, key: CustomExact) -> (&Atom, NodeId) {
        let atom = self.custom_key(key);
        let child_id = self.child_unchecked(node_id, key);
        (atom, child_id)
    }

    #[inline]
    fn child_unchecked(&self, node_id: NodeId, key: CustomExact) -> NodeId {
        *self.index.get(&(node_id, key)).unwrap()
    }

    fn new_branch<'a, I: Iterator<Item=InsertKey>>(&mut self, key: I) -> NodeId {
        let child_id = self.nodes.push(Default::default());
        self.insert_internal(child_id, key);
        child_id
    }

    fn find_custom(&self, node_id: NodeId, atom: &Atom) -> Option<NodeId> {
        match self.node(node_id) {
            AtomTrieNode::Single(key) if key.is_custom() => {
                if self.custom_key(*key) == atom {
                    Some(self.child_unchecked(node_id, *key))
                } else {
                    None
                }
            },
            AtomTrieNode::Many(list) => {
                let result = list.iter().find(|&key| key.is_custom() && self.custom_key(*key) == atom);
                result.map(|&key| self.child_unchecked(node_id, key))
            },
            AtomTrieNode::Single(_) | AtomTrieNode::Leaf(_) => None,
        }
    }

    #[inline]
    pub fn query<'a, I: Debug + Clone + Iterator<Item=QueryKey<'a>>>(&self, key: I, storage: &AtomStorage) -> BindingsSet {
        let mut mapper = CachingMapper::new(VariableAtom::make_unique);
        self.query_internal(self.root, key, storage, &mut mapper)
    }

    // TODO: write an algorithm which returns an iterator instead of collected result
    fn query_internal<'a, I, M>(&self, node_id: NodeId, mut key: I, storage: &AtomStorage,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom
    {
        match key.next() {
            Some(head) => match head {
                // FIXME: can be improved by converting InsertKey into CustomExact
                QueryKey::Exact(_, _) => self.match_exact_key(node_id, head, key, storage, mapper),
                QueryKey::StartExpr(_) => self.match_exact_key(node_id, head, key, storage, mapper),
                QueryKey::EndExpr => self.match_exact_key(node_id, head, key, storage, mapper),
                QueryKey::Custom(atom) => self.match_custom_key(node_id, atom, key, storage, mapper),
            },
            None => {
                BindingsSet::count(self.nodes[node_id].leaf_counter())
            }
        }
    }

    #[inline]
    fn node(&self, node_id: NodeId) -> &AtomTrieNode {
        &self.nodes[node_id]
    }

    #[inline]
    fn node_mut(&mut self, node_id: NodeId) -> &mut AtomTrieNode {
        &mut self.nodes[node_id]
    }

    fn match_exact_key<'a, I, M>(&self, node_id: NodeId, key: QueryKey<'a>, mut tail: I, storage: &AtomStorage,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom,
    {
        let mut result = BindingsSet::empty();
        let node = self.node(node_id);
        if let AtomTrieNode::Leaf(_count) = node {
            return result;
        }
        
        // match exact entries if applicable
        if let Some(exact_key) = key.as_exact_key() {
            if let Some(&child_id) = self.index.get(&(node_id, exact_key)) {
                result.extend(self.query_internal(child_id, tail.clone(), storage, mapper))
            }
        }
        // match custom entries if applicable
        if let Some(atom) = Self::exact_key_to_atom(&key, &mut tail) {
            self.visit_custom(node_id, |(entry, child_id)| {
                let mut custom_res = self.match_custom_entry(entry, atom, child_id, tail.clone(), storage, mapper);
                result.extend(custom_res.drain(..));
            })
        }
        result
    }

    fn visit_custom<F>(&self, node_id: NodeId, mut visitor: F)
        where F: FnMut((&Atom, NodeId)),
    {
        match self.node(node_id) {
            AtomTrieNode::Single(key) if key.is_custom() =>
                visitor(self.custom_entry(node_id, *key)),
            AtomTrieNode::Many(list) =>
                list.iter().filter(|&key| key.is_custom())
                    .for_each(|&key| visitor(self.custom_entry(node_id, key))),
            AtomTrieNode::Single(_) | AtomTrieNode::Leaf(_) => {},
        }
    }

    fn exact_key_to_atom<'a, I>(key: &QueryKey<'a>, tail: &mut I) -> Option<&'a Atom>
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
            QueryKey::Custom(_) => panic!("Custom key should not be passed"),
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

    fn match_custom_entry<'a, I, M>(&self, entry: &Atom, key: &Atom, child_id: NodeId, tail: I,
        storage: &AtomStorage,
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
        let tail_result = self.query_internal(child_id, tail, storage, mapper);
        //log::trace!("match_custom_entry: entry: {}, key: {}, child: {:?}, tail: {:?}, tail_result: {}", entry, key, child, tail.clone(), tail_result);
        // TODO: we could move BindingsSet into merge instead of passing by reference
        result.merge(&tail_result)
    }

    fn match_custom_key<'a, I, M>(&self, node_id: NodeId, key: &Atom, tail: I, storage: &AtomStorage,
        mapper: &mut CachingMapper<VariableAtom, VariableAtom, M>) -> BindingsSet
        where
            I: Debug + Clone + Iterator<Item=QueryKey<'a>>,
            M: Fn(VariableAtom)->VariableAtom
    {
        let mut result = BindingsSet::empty();
        for (entry, child_id) in self.unpack_atoms_internal(node_id, storage) {
            let mut tail_result = self.match_custom_entry(&entry, key, child_id, tail.clone(), storage, mapper);
            result.extend(tail_result.drain(..));
        }
        result
    }

    #[inline]
    pub fn unpack_atoms<'a>(&'a self, storage: &'a AtomStorage)
        -> Box<dyn Iterator<Item=Cow<'a, Atom>> + 'a>
    {
        Box::new(self.unpack_atoms_internal(self.root, storage)
            .flat_map(|(atom, child_id)| { 
                std::iter::repeat_n(atom, self.nodes[child_id].leaf_counter())
            }))
    }

    fn unpack_atoms_internal<'a>(&'a self, node_id: NodeId, storage: &'a AtomStorage)
        -> Box<dyn Iterator<Item=(Cow<'a, Atom>, NodeId)> + 'a>
    { 
        Box::new(AtomTrieNodeIter::new(self, node_id, storage)
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
        enum Found {
            Exact(CustomExact),
            Custom(CustomExact, usize),
        }

        match key.next() {
            Some(head) => {
                let entry = match head {
                    QueryKey::StartExpr(_) => self.index
                        .get(&(node_id, CustomExact::StartExpr())).cloned()
                        .map(|child_id| (Found::Exact(CustomExact::StartExpr()), child_id)),
                    QueryKey::EndExpr => self.index
                        .get(&(node_id, CustomExact::EndExpr())).cloned()
                        .map(|child_id| (Found::Exact(CustomExact::EndExpr()), child_id)),
                    QueryKey::Exact(None, _) => None,
                    QueryKey::Exact(Some(id), _) => self.index
                        .get(&(node_id, CustomExact::Exact(id))).cloned()
                        .map(|child_id| (Found::Exact(CustomExact::Exact(id)), child_id)),
                    QueryKey::Custom(atom) => self.node_iter(node_id)
                        .enumerate()
                        .filter(|(_idx, key)| key.is_custom())
                        .find(|(_idx, key)| atom == &self.custom[key.value()])
                        .map(|(idx, key)| (Found::Custom(key, idx), *self.index.get(&(node_id, key)).unwrap())),
                };
                match entry {
                    Some((child_key, child_id)) => {
                        let removed = self.remove_internal(child_id, key);
                        if self.nodes[child_id].is_empty() {
                            match child_key {
                                Found::Exact(key) => {
                                    self.index.remove(&(node_id, key));
                                    self.nodes[node_id].remove_key(key);
                                },
                                Found::Custom(key, index) => {
                                    self.index.remove(&(node_id, key));
                                    self.nodes[node_id].remove_index(index);
                                },
                            }
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

    fn node_iter(&self, node_id: NodeId) -> Box<dyn '_ + Iterator<Item=CustomExact>> {
        match &self.nodes[node_id] {
            AtomTrieNode::Leaf(_) => Box::new(std::iter::empty()),
            AtomTrieNode::Single(key) => Box::new(std::iter::once(*key)),
            AtomTrieNode::Many(vec) => Box::new(vec.iter().copied()),
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.nodes[self.root].is_empty()
    }
}

impl<D: DuplicationStrategy> Display for AtomTrie<D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!();
    }
}

const CE_KIND_MASK: usize = 0b11 << 62;
const CE_VALUE_MASK: usize = !(0b11 << 62);

const CE_CUSTOM: usize = 0b00 << 62;
const CE_EXACT: usize = 0b01 << 62;
const CE_START_EXPR: usize = 0b10 << 62;
const CE_END_EXPR: usize = 0b11 << 62;

#[derive(Hash, Copy, Clone, PartialEq, Eq)]
struct CustomExact(usize);

impl CustomExact {
    #[allow(non_snake_case)]
    #[inline]
    fn Custom(value: usize) -> Self {
        assert!(CE_KIND_MASK & value == 0);
        Self(CE_CUSTOM | value)
    }
    #[allow(non_snake_case)]
    #[inline]
    fn Exact(value: usize) -> Self {
        assert!(CE_KIND_MASK & value == 0);
        Self(CE_EXACT | value)
    }

    #[allow(non_snake_case)]
    #[inline]
    fn StartExpr() -> Self {
        Self(CE_START_EXPR)
    }

    #[allow(non_snake_case)]
    #[inline]
    fn EndExpr() -> Self {
        Self(CE_END_EXPR)
    }

    #[inline]
    fn value(&self) -> usize {
        self.0 & CE_VALUE_MASK
    }

    #[inline]
    fn is_custom(&self) -> bool {
        self.is_kind(CE_CUSTOM)
    }

    #[inline]
    fn is_kind(&self, kind: usize) -> bool {
        self.get_kind() == kind
    }

    #[inline]
    fn get_kind(&self) -> usize {
        self.0 & CE_KIND_MASK
    }
}

impl Debug for CustomExact {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let kind = self.get_kind();
        match kind {
            CE_CUSTOM => write!(f, "CustomExact::Exact({})", self.value()),
            CE_EXACT => write!(f, "CustomExact::Custom({})", self.value()),
            CE_START_EXPR => write!(f, "CustomExact::Custom({})", self.value()),
            CE_END_EXPR => write!(f, "CustomExact::Custom({})", self.value()),
            _ => panic!("Unexpected kind: {}", kind >> 62),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AtomTrieNode {
    Leaf(usize),
    Single(CustomExact),
    Many(Box<Vec<CustomExact>>),
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

pub type QueryResult = Box<dyn Iterator<Item=Bindings>>;

impl AtomTrieNode {
    #[cfg(test)]
    pub fn new() -> Self {
        Self::default()
    }

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

    pub fn push(&mut self, key: CustomExact) {
        match self {
            Self::Leaf(0) => *self = Self::Single(key),
            Self::Leaf(_) => panic!("Unexpected state"),
            Self::Single(prev) => *self = Self::Many(Box::new(vec![*prev, key])),
            Self::Many(vec) => vec.push(key),
        }


    }

    pub fn remove_key(&mut self, to_remove: CustomExact) {
        match self {
            Self::Leaf(_) => panic!("Unexpected state"),
            Self::Single(key) => {
                assert!(*key == to_remove, "Unexpected state");
                *self = Self::default();
            },
            Self::Many(vec) => {
                let idx = vec.iter().position(|key| *key == to_remove)
                    .expect("Unexpected state");
                vec.remove(idx);
                if vec.len() == 1 {
                    *self = Self::Single(vec[0]);
                }
            }
        }
    }

    pub fn remove_index(&mut self, idx: usize) {
        match self {
            Self::Leaf(_) => panic!("Unexpected state"),
            Self::Single(_key) => {
                assert!(idx == 0, "Unexpected state");
                *self = Self::default();
            },
            Self::Many(vec) => {
                vec.remove(idx);
                if vec.len() == 1 {
                    *self = Self::Single(vec[0]);
                }
            }
        }
    }
}

impl Display for AtomTrieNode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        todo!()
        //enum TrieKeyDisplay<'a> {
            //Id(usize),
            //Atom(&'a Atom),
            //Start,
            //End,
        //}

        //impl Display for TrieKeyDisplay<'_> {
            //fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                //match self {
                    //Self::Id(id) => write!(f, "Id({})", id),
                    //Self::Atom(atom) => write!(f, "Atom({})", atom),
                    //Self::Start => write!(f, "StartExpr",),
                    //Self::End => write!(f, "EndExpr",),
                //}
            //}
        //}

        //impl TrieKeyDisplay<'_> {
            //fn from_exact(key: &ExactKey) -> Self {
                //match key {
                    //key if *key == ExactKey::START_EXPR => Self::Start,
                    //key if *key == ExactKey::END_EXPR => Self::End,
                    //ExactKey(id) => Self::Id(*id),
                //}
            //}
        //}

        //match self {
            //AtomTrieNode::Node(content) => {
                ////let exact = content.exact.iter()
                    ////.map(|(key, child)| (TrieKeyDisplay::from_exact(key), child));
                //let custom = content.custom.iter().map(|p| (&p.0, &p.1))
                    //.map(|(key, child)| (TrieKeyDisplay::Atom(key), child));
                //write_mapping(f, custom[>exact.chain(custom)<])
            //},
            //AtomTrieNode::Leaf(count) => {
                //if *count > 1 {
                    //write!(f, "{{ x{} }}", count)
                //} else {
                    //write!(f, "{{}}")
                //}
            //},
        //}
    }
}

#[derive(Debug, Clone)]
struct AtomTrieNodeIter<'a, D: DuplicationStrategy> {
    trie: &'a AtomTrie<D>,
    node_id: NodeId,
    // FIXME: return only ids and unpack atoms in AtomTrie code if needed
    // it is not required for remove for instance
    storage: &'a AtomStorage,
    state: AtomTrieNodeIterState<'a, D>,
}

#[derive(Default, Debug, Clone)]
enum AtomTrieNodeIterState<'a, D: DuplicationStrategy> {
    VisitEntries(std::slice::Iter<'a, CustomExact>),
    VisitSingle(CustomExact),
    VisitExpression {
        build_expr: Vec<Atom>,
        cur_it: Box<AtomTrieNodeIter<'a, D>>,
        next_state: Box<Self>,
    },
    #[default]
    End,
}

impl<'a, D: DuplicationStrategy> AtomTrieNodeIter<'a, D> {
    fn new(trie: &'a AtomTrie<D>, node_id: NodeId, storage: &'a AtomStorage) -> Self {
        let state = match trie.node(node_id) {
            AtomTrieNode::Many(entries) => AtomTrieNodeIterState::VisitEntries(entries.iter()),
            AtomTrieNode::Single(entry) => AtomTrieNodeIterState::VisitSingle(*entry),
            AtomTrieNode::Leaf(_count) => AtomTrieNodeIterState::End,
        };
        Self {
            trie,
            node_id,
            storage,
            state,
        }
    }

    fn single_step(&mut self, key: CustomExact) -> Option<(IterResult<'a>, NodeId)> {
        const ATOM_NOT_IN_STORAGE: &'static str = "Exact entry contains atom which is not in storage";
        type State<'a, D> = AtomTrieNodeIterState<'a, D>;

        if matches!(self.state, State::VisitSingle(_)) {
            self.state = State::End;
        }

        let child_id = self.trie.child_unchecked(self.node_id, key);
        let kind = key.get_kind();
        match kind {
            CE_END_EXPR => return Some((IterResult::EndExpr, child_id)),
            CE_START_EXPR => {
                let state = std::mem::take(&mut self.state);
                self.state = State::VisitExpression{
                    build_expr: Vec::new(),
                    cur_it: Box::new(AtomTrieNodeIter::new(self.trie, child_id, self.storage)),
                    next_state: Box::new(state),
                };
                return None;
            },
            CE_EXACT => {
                let id = key.value();
                // FIXME: here we can get in unchecked way
                let atom = self.storage.get_atom(id).expect(ATOM_NOT_IN_STORAGE);
                return Some((IterResult::Atom(Cow::Borrowed(atom)), child_id));
            },
            CE_CUSTOM => {
                let id = key.value();
                let atom = &self.trie.custom[id];
                return Some((IterResult::Atom(Cow::Borrowed(atom)), child_id));
            },
            // FIXME: print kind correctly
            _ => panic!("Unexpected kind: {}", kind),
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
                        Some(key) => {
                            match self.single_step(*key) {
                                Some(result) => return Some(result),
                                None => {},
                            }
                        }
                        None => self.state = State::End,
                    }
                },
                State::VisitSingle(key) => {
                    let key = *key;
                    match self.single_step(key) {
                        Some(result) => return Some(result),
                        None => {},
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
                                cur_it: Box::new(AtomTrieNodeIter::new(self.trie, child_id, self.storage)),
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

    //#[test]
    //fn atom_trie_node_display() {
        //let mut node = AtomTrieNode::new();
        //node.insert([InsertKey::Exact(1)].into_iter());
        //assert_eq!(format!("{}", node), "{ Id(1): {} }");

        //let mut node = AtomTrieNode::new();
        //node.insert([InsertKey::Custom(Atom::sym("A"))].into_iter());
        //assert_eq!(format!("{}", node), "{ Atom(A): {} }");

        //let mut node = AtomTrieNode::new();
        //node.insert([InsertKey::StartExpr, InsertKey::Custom(Atom::sym("B")), InsertKey::EndExpr].into_iter());
        //assert_eq!(format!("{}", node), "{ StartExpr: { Atom(B): { EndExpr: {} } } }");
    //}

    //#[test]
    //fn atom_trie_node_display_duplicate() {
        //let mut node = AtomTrieNode::with_strategy(ALLOW_DUPLICATION);
        //node.insert([InsertKey::Custom(Atom::sym("A"))].into_iter());
        //node.insert([InsertKey::Custom(Atom::sym("A"))].into_iter());
        //assert_eq!(format!("{}", node), "{ Atom(A): { x2 } }");

        //let mut node = AtomTrieNode::with_strategy(ALLOW_DUPLICATION);
        //node.insert([InsertKey::StartExpr, InsertKey::Custom(Atom::sym("B")), InsertKey::EndExpr].into_iter());
        //node.insert([InsertKey::StartExpr, InsertKey::Custom(Atom::sym("B")), InsertKey::EndExpr].into_iter());
        //assert_eq!(format!("{}", node), "{ StartExpr: { Atom(B): { EndExpr: { x2 } } } }");
    //}

    #[test]
    fn atom_trie_node_size() {
        assert_eq!(std::mem::size_of::<AtomTrieNode>(), 2 * std::mem::size_of::<usize>());
    }

    #[test]
    fn atom_trie_custom_exact_key_size() {
        assert_eq!(std::mem::size_of::<CustomExact>(), std::mem::size_of::<usize>());
    }
}
