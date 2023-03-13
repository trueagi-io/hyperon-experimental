use std::fmt::{Debug, Display};
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::marker::PhantomData;
use crate::common::shared::Shared;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum NodeKey<T> {
    Exact(T),
    Wildcard,
    LeftPar,
    RightPar,
}

impl<T: PartialEq> NodeKey<T> {
    fn is_parenthesis(&self) -> bool {
        *self == NodeKey::RightPar || *self == NodeKey::LeftPar
    }
}

impl<T: Display> Display for NodeKey<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            NodeKey::Exact(val) => write!(f, "Exact({})", val),
            NodeKey::Wildcard => write!(f, "*"),
            NodeKey::LeftPar => write!(f, "LeftPar"),
            NodeKey::RightPar => write!(f, "RightPar"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
struct _NodeKey<T> {
    key: NodeKey<T>,
    par_size: Option<usize>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct TrieKey<T>(VecDeque<_NodeKey<T>>);

impl<T> TrieKey<T> {
    pub fn from_list<V: Into<VecDeque<NodeKey<T>>>>(keys: V) -> Self {
        let panic = |err| { panic!("{}", err) };
        let keys = Self::precalculate_par_sizes(keys.into()).unwrap_or_else(panic);
        Self(keys)
    }

    fn precalculate_par_sizes(bare_keys: VecDeque<NodeKey<T>>) -> Result<VecDeque<_NodeKey<T>>, String> {
        let mut left_par_stack = Vec::new();
        let mut keys_with_sizes = VecDeque::new();
        for (pos, bare_key) in bare_keys.into_iter().enumerate() {
            keys_with_sizes.push_back(_NodeKey{ key: bare_key, par_size: None });
            match keys_with_sizes.back().unwrap().key {
                NodeKey::LeftPar => left_par_stack.push(pos),
                NodeKey::RightPar => {
                    let error = || { format!(concat!(
                            "Unbalanced key: NodeKey::RightPar without ",
                            "NodeKey::LeftPar at position {}"), pos) };
                    let start = left_par_stack.pop().ok_or_else(error)?;
                    keys_with_sizes[start].par_size = Some(pos - start);
                },
                _ => {},
            }
        }
        if left_par_stack.is_empty() {
            Ok(keys_with_sizes)
        } else {
            Err(format!(concat!("Unbalanced key: NodeKey::LeftPar without ",
                        "NodeKey::Right at positions {:?}"), left_par_stack))
        }
    }

    fn pop_head(&mut self) -> Option<_NodeKey<T>> {
        self.0.pop_front()
    }

    fn pop_head_unchecked(&mut self) -> _NodeKey<T> {
        self.pop_head().expect("Unexpected end of key")
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T: Clone> TrieKey<T> {
    fn skip_tokens(&self, size: usize) -> Self {
        Self(self.0.iter().cloned().skip(size).collect())
    }
}

impl<T: Display> Display for TrieKey<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TrieKey(")
            .and_then(|_| self.0.iter().take(1).fold(Ok(()),
                |res, key| res.and_then(|_| write!(f, "{}", key.key))))
            .and_then(|_| self.0.iter().skip(1).fold(Ok(()),
                |res, key| res.and_then(|_| write!(f, ", {}", key.key))))
            .and_then(|_| write!(f, ")"))
    }
}

pub type MultiTrie<K, V> = MultiTrieNode<K, V>;

#[derive(Clone, Debug)]
pub struct MultiTrieNode<K, V> {
    children: HashMap<NodeKey<K>, Shared<Self>>,
    skip_pars: HashMap<*mut Self, Shared<Self>>,
    values: HashSet<V>,
}

impl<K, V> MultiTrieNode<K, V>
where
    K: Clone + Debug + Eq + Hash + ?Sized,
    V: Clone + Debug + Eq + Hash + ?Sized,
{

    pub fn new() -> Self {
        Self{
            children: HashMap::new(),
            skip_pars: HashMap::new(),
            values: HashSet::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.children.is_empty() && self.values.is_empty()
    }

    fn get_or_insert_child(&mut self, key: NodeKey<K>) -> Shared<Self> {
        self.children.entry(key).or_insert(Shared::new(Self::new())).clone()
    }

    fn next<F, R>(&self, mut key: TrieKey<K>, map: F, result: &mut Vec<R>)
        where F: Fn(Option<NodeKey<K>>, &Shared<Self>, TrieKey<K>) -> R,
    {
        let head = key.pop_head_unchecked();
        match head.key {
            NodeKey::Exact(_) => {
                self.children.get(&head.key).map(|child| result.push(map(Some(head.key), child, key.clone())));
                self.children.get(&NodeKey::Wildcard).map(|child| result.push(map(Some(NodeKey::Wildcard), child, key)));
            },
            NodeKey::RightPar => {
                self.children.get(&head.key).map(|child| result.push(map(Some(head.key), child, key)));
            },
            NodeKey::LeftPar => {
                self.children.get(&NodeKey::Wildcard)
                    .map(|child| result.push(map(Some(NodeKey::Wildcard), child, key.skip_tokens(head.par_size.unwrap()))));
                self.children.get(&NodeKey::LeftPar)
                    .map(|child| result.push(map(Some(NodeKey::LeftPar), child, key)));
            },
            NodeKey::Wildcard => {
                self.children.iter()
                    .filter(|(key, _child)| !key.is_parenthesis())
                    .for_each(|(head, child)| result.push(map(Some(head.clone()), child, key.clone())));
                self.skip_pars.values()
                    .for_each(|child| result.push(map(None, child, key.clone())));
            },
        };
    }

    pub fn remove(&mut self, key: TrieKey<K>, value: &V) -> bool {
        log::debug!("MultiTrieNode::remove(): key: {:?}, value: {:?}", key, value);
        if key.is_empty() {
            self.values.remove(value)
        } else {
            let mut children = Vec::new();
            self.next(key, |child_key, child_node, key| (child_key, child_node.clone(), key), &mut children);
            children.into_iter()
                .map(|(child_key, child_node, key)| {
                    let removed = child_node.borrow_mut().remove(key, value);
                    if removed && child_node.borrow().is_empty() {
                        match child_key {
                            Some(key) => { self.children.remove(&key); },
                            None => { self.skip_pars.remove(&child_node.as_ptr()); },
                        }
                    }
                    removed
                })
            .fold(false, |a, b| a || b)
        }
    }
    
    pub fn add(&mut self, mut key: TrieKey<K>, value: V) {
        log::debug!("MultiTrie::add(): key: {:?}, value: {:?}", key, value);
        if key.is_empty() {
            self.values.insert(value);
        } else {
            let mut nodes: Vec<Shared<Self>> = vec![];
            let mut pars: Vec<(usize, usize)> = Vec::new();
            let mut pos = 0;
            
            let head = key.pop_head_unchecked();
            if let _NodeKey{ key: NodeKey::LeftPar, par_size: Some(size) } = head {
                pars.push((pos, pos + size + 1));
            }
            let mut cur = self.get_or_insert_child(head.key);
            nodes.push(cur.clone());
            pos = pos + 1;

            loop {
                match key.pop_head() {
                    None => {
                        cur.borrow_mut().values.insert(value);
                        break;
                    },
                    Some(head) => {
                        if let _NodeKey{ key: NodeKey::LeftPar, par_size: Some(size) } = head {
                            pars.push((pos, pos + size + 1));
                        }
                        let node = cur.borrow_mut().get_or_insert_child(head.key);
                        cur = node;
                        nodes.push(cur.clone());
                    },
                }
                pos = pos + 1
            }
            for (start, end) in pars {
                let end_node = nodes[end - 1].clone();
                if start > 0 {
                    nodes[start - 1].borrow_mut().skip_pars.insert(end_node.as_ptr(), end_node);
                } else {
                    self.skip_pars.insert(end_node.as_ptr(), end_node);
                }
            }
        }
    }

    pub fn get(&self, key: TrieKey<K>) -> impl Iterator<Item=&V> + '_ {
        MultiValueIter::new(self, key).flat_map(|node| node.values.iter())
    }

    #[cfg(test)]
    fn size(&self) -> usize {
        let mut counted = HashSet::new();
        fn size_recursive<K, V>(node: &MultiTrieNode<K, V>, counted: &mut HashSet<*const MultiTrieNode<K, V>>) -> usize {
            let ptr = node as *const MultiTrieNode<K, V>;
            if !counted.contains(&ptr) {
                counted.insert(ptr);
                node.children.values().fold(1, |size, node| {
                    size + size_recursive(node.borrow().as_ref(), counted)
                })
            } else {
                0
            }
        }
        size_recursive(self, &mut counted)
    }
}

struct MultiValueIter<'a, K, V> {
    to_be_explored: Vec<(*mut MultiTrieNode<K, V>, TrieKey<K>)>,
    _root_node_ref: PhantomData<&'a MultiTrieNode<K, V>>,
}

impl<'a, K, V> MultiValueIter<'a, K, V>
where
    K: Clone + Debug + Eq + Hash + ?Sized,
    V: Clone + Debug + Eq + Hash + ?Sized,
{
    fn new(node: &'a MultiTrieNode<K, V>, key: TrieKey<K>) -> Self {
        let mut to_be_explored = Vec::new();
        node.next(key, Self::map_children, &mut to_be_explored);
        Self{ to_be_explored, _root_node_ref: PhantomData }
    }

    fn map_children(_child_key: Option<NodeKey<K>>,
        child: &Shared<MultiTrieNode<K, V>>, key: TrieKey<K>) -> (*mut MultiTrieNode<K, V>, TrieKey<K>) {
        (child.as_ptr(), key)
    }
}

impl<'a, K, V> Iterator for MultiValueIter<'a, K, V>
where
    K: Clone + Debug + Eq + Hash + ?Sized,
    V: Clone + Debug + Eq + Hash + ?Sized,
{
    type Item = &'a MultiTrieNode<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, key)) = self.to_be_explored.pop() {
            let node = unsafe{ &*node };
            match key.is_empty() {
                true => return Some(node),
                false => node.next(key, MultiValueIter::map_children, &mut self.to_be_explored),
            }
        }
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    trait IntoSorted<T: Ord> {
        fn to_sorted(self) -> Vec<T>;
    }

    impl<'a, T: 'a + Ord + Clone, I: Iterator<Item=&'a T>> IntoSorted<T> for I {
        fn to_sorted(self) -> Vec<T> {
            let mut vec: Vec<T> = self.cloned().collect();
            vec.sort();
            vec
        }
    }

    macro_rules! triekey {
        ($($x:tt)*) => { TrieKey::from_list(triekeyslice!($($x)*)) }
    }

    macro_rules! triekeyslice {
        () => { vec![] };
        (*) => { vec![ NodeKey::Wildcard ] };
        ($x:literal) => { vec![ NodeKey::Exact($x) ] };
        ([]) => { vec![ vec![ NodeKey::LeftPar ], vec![ NodeKey::RightPar ] ].concat() };
        ([$($x:tt),*]) => { {
            vec![ vec![ NodeKey::LeftPar ], $( triekeyslice!($x) ),*, vec![ NodeKey::RightPar ] ].concat()
        } };
        ($($x:tt),*) => { vec![ $( triekeyslice!($x) ),* ].concat() };
    }

    #[test]
    fn triekey_macro() {
        assert_eq!(triekey!() as TrieKey<u32>, TrieKey::from_list([ ]));
        assert_eq!(triekey!(*) as TrieKey<u32>, TrieKey::from_list([NodeKey::Wildcard]));
        assert_eq!(triekey!(0), TrieKey::from_list([NodeKey::Exact(0)]));
        assert_eq!(triekey!([]) as TrieKey<u32>, TrieKey::from_list([
                NodeKey::LeftPar,NodeKey::RightPar]));
        assert_eq!(triekey!([0, *]), TrieKey::from_list([
                NodeKey::LeftPar, NodeKey::Exact(0),
                NodeKey::Wildcard, NodeKey::RightPar]));
        assert_eq!(triekey!([[0, *]]), TrieKey::from_list([
                NodeKey::LeftPar, NodeKey::LeftPar,
                NodeKey::Exact(0), NodeKey::Wildcard,
                NodeKey::RightPar, NodeKey::RightPar]));
        assert_eq!(triekey!(0, *, [*]), TrieKey::from_list([
                NodeKey::Exact(0), NodeKey::Wildcard,
                NodeKey::LeftPar, NodeKey::Wildcard,
                NodeKey::RightPar]));
    }

    #[test]
    fn multi_trie_add_basic() {
        let mut trie = MultiTrie::new();

        trie.add(triekey!("A"), "exact_a");
        trie.add(triekey!(*), "wild");
        trie.add(triekey!(["A", "B"]), "pars_a_b");
        trie.add(triekey!("A", "B"), "a_b");

        assert_eq!(trie.get(triekey!("A")).to_sorted(), vec!["exact_a", "wild"]);
        assert_eq!(trie.get(triekey!("B")).to_sorted(), vec!["wild"]);
        assert_eq!(trie.get(triekey!(*)).to_sorted(), vec!["exact_a", "pars_a_b", "wild"]);
        assert_eq!(trie.get(triekey!(["A", "B"])).to_sorted(), vec!["pars_a_b", "wild"]);
        assert_eq!(trie.get(triekey!(["A", "C"])).to_sorted(), vec!["wild"]);
        assert_eq!(trie.get(triekey!(["A", *])).to_sorted(), vec!["pars_a_b", "wild"]);
        assert_eq!(trie.get(triekey!("A", "B")).to_sorted(), vec!["a_b"]);
        assert_eq!(trie.get(triekey!("A", "C")).to_sorted(), vec![] as Vec<&str>);
        assert_eq!(trie.get(triekey!("A", *)).to_sorted(), vec!["a_b"]);
    }

    #[test]
    fn multi_trie_add_pars() {
        let mut trie = MultiTrie::new();

        trie.add(triekey!(["A", "B"]), "pars_a_b");
        trie.add(triekey!([*, "C"]), "pars_x_c");

        assert_eq!(trie.get(triekey!([*, "B"])).to_sorted(), vec!["pars_a_b"]);
        assert_eq!(trie.get(triekey!(["A", "C"])).to_sorted(), vec!["pars_x_c"]);
    }

    #[test]
    fn multi_trie_add_subpars_twice() {
        let mut trie: MultiTrie<&'static str, &'static str> = MultiTrie::new();

        trie.add(triekey!([]), "empty_pars");
        trie.add(triekey!([]).clone(), "empty_pars");

        assert_eq!(trie.get(triekey!([])).to_sorted(), vec!["empty_pars"]);
        assert_eq!(trie.get(triekey!(*)).to_sorted(), vec!["empty_pars"]);
    }

    #[test]
    fn multi_trie_twice_result_because_of_subpars() {
        let mut trie = MultiTrie::new();

        trie.add(triekey!(["A"]), "pars_a");
        trie.add(triekey!(["B"]), "pars_b");

        assert_eq!(trie.get(triekey!([*])).to_sorted(), vec!["pars_a", "pars_b"]);
    }

    #[test]
    fn multi_trie_remove_basic() {
        let mut trie = MultiTrie::new();
        let empty_trie_size = trie.size();
        trie.add(triekey!("A"), "exact_a");
        trie.add(triekey!(*), "wild");
        trie.add(triekey!(["A", "B"]), "pars_a_b");
        trie.add(triekey!("A", "B"), "a_b");

        trie.remove(triekey!("A"), &"exact_a");
        trie.remove(triekey!(*), &"wild");
        trie.remove(triekey!(["A", "B"]), &"pars_a_b");
        trie.remove(triekey!("A", "B"), &"a_b");

        assert!(trie.get(triekey!("A")).to_sorted().is_empty());
        assert!(trie.get(triekey!(*)).to_sorted().is_empty());
        assert!(trie.get(triekey!(["A", "B"])).to_sorted().is_empty());
        assert!(trie.get(triekey!("A", "B")).to_sorted().is_empty());
        assert_eq!(trie.size(), empty_trie_size);
    }

    #[test]
    fn trie_key_display() {
        assert_eq!(format!("{}", triekey!("A")), "TrieKey(Exact(A))");
        assert_eq!(format!("{}", triekey!(*) as TrieKey<u32>), "TrieKey(*)");
        assert_eq!(format!("{}", triekey!(["A", "B", *])), "TrieKey(LeftPar, Exact(A), Exact(B), *, RightPar)");
    }

    #[test]
    fn multi_trie_clone() {
        let mut trie = MultiTrie::new();
        let key = triekey!(0, *, [*]);
        trie.add(key.clone(), "test");

        let copy = trie.clone();

        assert_eq!(copy.get(key).to_sorted(), vec!["test"]);
    }

    #[test]
    fn multi_trie_add_key_with_many_subpars() {
        fn with_subpars(nvars: usize) -> TrieKey<NodeKey<usize>> {
            let mut keys = Vec::new();
            keys.push(NodeKey::LeftPar);
            for _i in 0..nvars {
                keys.push(NodeKey::LeftPar);
                keys.push(NodeKey::RightPar);
            }
            keys.push(NodeKey::RightPar);
            TrieKey::from_list(keys)
        }
        let mut trie = MultiTrie::new();

        trie.add(with_subpars(4), 0);
        assert_eq!(trie.size(), 5*2 + 1);

        trie.add(with_subpars(8), 0);
        assert_eq!(trie.size(), 20);
    }
}
