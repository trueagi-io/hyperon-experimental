use super::storage::AtomStorage;

use crate::atom::*;
use crate::matcher::*;
use crate::common::CachingMapper;
use crate::common::collections::write_mapping;
use crate::common::vecondemand::VecOnDemand;

use std::hash::Hash;
use std::collections::HashMap;
use std::collections::hash_map;
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
pub struct ExactKey(usize);

impl ExactKey {
    const START_EXPR: ExactKey = ExactKey(usize::MAX - 1);
    const END_EXPR: ExactKey = ExactKey(usize::MAX);
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AtomTrie<D: DuplicationStrategy = NoDuplication>(AtomTrieNode<D>);

impl<D: DuplicationStrategy> AtomTrie<D> {

    #[cfg(test)]
    #[inline]
    pub fn with_strategy(_strategy: D) -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert<I: Iterator<Item=InsertKey>>(&mut self, key: I) {
        self.0.insert(key)
    }

    #[inline]
    pub fn query<'a, I: Debug + Clone + Iterator<Item=QueryKey<'a>>>(&self, key: I, storage: &AtomStorage) -> BindingsSet {
        self.0.query(key, storage)
    }

    #[inline]
    pub fn unpack_atoms<'a>(&'a self, storage: &'a AtomStorage) -> Box<dyn Iterator<Item=Cow<'a, Atom>> + 'a>{
        Box::new(self.0.unpack_atoms(storage)
            .flat_map(|(a, leaf)| { std::iter::repeat_n(a, leaf.leaf_counter()) }))
    }

    #[inline]
    pub fn remove<'a, I: Iterator<Item=QueryKey<'a>>>(&mut self, key: I, storage: &AtomStorage) -> bool {
        self.0.remove(key, storage)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn stats(&self) -> AtomTrieNodeStats {
        self.0.stats()
    }
}

impl<D: DuplicationStrategy> Display for AtomTrie<D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
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

pub type QueryResult = Box<dyn Iterator<Item=Bindings>>;

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

    pub fn unpack_atoms<'a>(&'a self, storage: &'a AtomStorage) -> Box<dyn Iterator<Item=(Cow<'a, Atom>, &'a AtomTrieNode<D>)> + 'a>{
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

    pub fn leaf_counter(&self) -> usize {
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

    #[test]
    fn atom_trie_node_size() {
        assert_eq!(std::mem::size_of::<AtomTrieNode<AllowDuplication>>(), 2 * std::mem::size_of::<usize>());
    }
}
