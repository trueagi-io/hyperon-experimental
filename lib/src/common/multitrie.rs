use std::fmt::Debug;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use crate::common::shared::Shared;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum NodeKey<T> {
    Exact(T),
    Wildcard,
    Expression(usize),
    ExpressionBegin,
    ExpressionEnd,
}

impl<T: PartialEq> NodeKey<T> {
    fn is_expr_begin_or_end(&self) -> bool {
        *self == NodeKey::ExpressionEnd || *self == NodeKey::ExpressionBegin
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct TrieKey<T>(VecDeque<NodeKey<T>>);

impl<T> TrieKey<T> {
    pub fn from_list<V: Into<VecDeque<NodeKey<T>>>>(keys: V) -> Self {
        Self(keys.into())
    }

    fn pop_head(&mut self) -> Option<NodeKey<T>> {
        self.0.pop_front()
    }

    fn pop_head_unchecked(&mut self) -> NodeKey<T> {
        self.pop_head().expect("Unexpected end of key")
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T: Clone> TrieKey<T> {
    fn skip_expr(&self, expr_len: usize) -> Self {
        let no_expr_tail = self.0.iter().cloned().skip(expr_len).collect();
        Self(no_expr_tail)
    }
}

pub type MultiTrie<K, V> = MultiTrieNode<K, V>;

#[derive(Clone, Debug)]
pub struct MultiTrieNode<K, V> {
    children: HashMap<NodeKey<K>, Shared<Self>>,
    expression_wildcards: HashMap<*mut Self, Shared<Self>>,
    values: HashSet<V>,
}

macro_rules! multi_trie_explorer {
    ( $ValueExplorer:ident, $UnexploredPath:ident, {$( $mut_:tt )?}, $raw_mut:tt ) => {

        struct $UnexploredPath<K, V> {
            node: * $raw_mut MultiTrieNode<K, V>,
            key: TrieKey<K>,
        }

        impl<K, V> $UnexploredPath<K, V> {
            fn new(node: * $raw_mut MultiTrieNode<K, V>, key: TrieKey<K>) -> Self {
                Self{ node, key }
            }
        }

        struct $ValueExplorer<'a, K, V, ExploringStrategy>
            where ExploringStrategy: Fn(&'a $( $mut_ )? MultiTrieNode<K, V>,
                  TrieKey<K>, &mut dyn FnMut($UnexploredPath<K, V>))
        {
            to_be_explored: Vec<$UnexploredPath<K, V>>,
            strategy: ExploringStrategy,
            _marker: std::marker::PhantomData<&'a $( $mut_ )? MultiTrieNode<K, V>>,
        }

        impl<'a, K, V, ExploringStrategy> $ValueExplorer<'a, K, V, ExploringStrategy>
            where ExploringStrategy: Fn(&'a $( $mut_ )? MultiTrieNode<K, V>,
                  TrieKey<K>, &mut dyn FnMut($UnexploredPath<K, V>))
        {
            fn new(node: &'a $( $mut_ )? MultiTrieNode<K, V>, key: TrieKey<K>, strategy: ExploringStrategy) -> Self {
                let to_be_explored = vec![$UnexploredPath::new(node, key)];
                Self{ to_be_explored, strategy, _marker: std::marker::PhantomData }
            }

            fn explore(&mut self, node: * $raw_mut MultiTrieNode<K, V>, key: TrieKey<K>) {
                let node = unsafe{ & $( $mut_ )? *node};
                let to_be_explored = &mut self.to_be_explored;
                (self.strategy)(node, key, &mut |key| to_be_explored.push(key));
            }
        }

        impl<'a, K, V, ExploringStrategy> Iterator for $ValueExplorer<'a, K, V, ExploringStrategy>
            where ExploringStrategy: Fn(&'a $( $mut_ )? MultiTrieNode<K, V>,
                  TrieKey<K>, &mut dyn FnMut($UnexploredPath<K, V>))
        {
            type Item = &'a $( $mut_ )? MultiTrieNode<K, V>;

            fn next(&mut self) -> Option<Self::Item> {
                while let Some($UnexploredPath{node, key}) = self.to_be_explored.pop() {
                    match key.is_empty() {
                        true => {
                            let node = unsafe{ & $( $mut_ )? *node };
                            return Some(node);
                        },
                        false => self.explore(node, key),
                    }
                }
                None
            }
        }
    }
}

multi_trie_explorer!(ValueMutExplorer, UnexploredPathMut, { mut }, mut);
multi_trie_explorer!(ValueExplorer, UnexploredPath, { /* no mut */ }, const);

impl<K, V> MultiTrieNode<K, V>
where
    K: Clone + Debug + Eq + Hash + ?Sized,
    V: Clone + Debug + Eq + Hash + ?Sized,
{

    pub fn new() -> Self {
        Self{
            children: HashMap::new(),
            expression_wildcards: HashMap::new(),
            values: HashSet::new(),
        }
    }

    fn get_or_insert_child(&mut self, key: NodeKey<K>) -> Shared<Self> {
        self.children.entry(key).or_insert(Shared::new(Self::new())).clone()
    }

    fn get_child(&self, key: &NodeKey<K>) -> Option<*const Self> {
        self.children.get(key).map(|child| Shared::as_ptr(&child) as *const Self)
    }

    fn get_child_mut(&mut self, key: &NodeKey<K>) -> Option<*mut Self> {
        self.children.get_mut(key).map(|child| Shared::as_ptr(child))
    }

    fn remove_exploring_strategy(&mut self, mut key: TrieKey<K>, callback: &mut dyn FnMut(UnexploredPathMut<K, V>)) {
        let head = key.pop_head_unchecked();
        match head {
            NodeKey::Exact(_) => {
                self.get_child_mut(&head).map(|child| callback(UnexploredPathMut::new(child, key.clone())));
                self.get_child_mut(&NodeKey::Wildcard).map(|child| callback(UnexploredPathMut::new(child, key)));
            },
            NodeKey::ExpressionEnd => {
                self.get_child_mut(&head).map(|child| callback(UnexploredPathMut::new(child, key)));
            },
            NodeKey::Expression(expr_len) => {
                self.get_child_mut(&NodeKey::Wildcard).map(|child| callback(UnexploredPathMut::new(child, key.skip_expr(expr_len))));
                self.get_child_mut(&NodeKey::ExpressionBegin).map(|child| callback(UnexploredPathMut::new(child, key)));
            },
            NodeKey::Wildcard => {
                self.children.iter_mut()
                    .filter(|(key, _child)| !key.is_expr_begin_or_end())
                    .map(|(_key, child)| child)
                    .for_each(|child| callback(UnexploredPathMut::new(child.as_ptr(), key.clone())));
                self.expression_wildcards.values_mut()
                    .for_each(|child| callback(UnexploredPathMut::new(child.as_ptr(), key.clone())));
            },
            NodeKey::ExpressionBegin => panic!(concat!(
                    "NodeKey::ExpressionBegin used only for indexing never for searching.",
                    "Should not be included into a key created from atom.")),
        }
    }
    
    fn get_exploring_strategy(&self, mut key: TrieKey<K>, callback: &mut dyn FnMut(UnexploredPath<K, V>)) {
        let head = key.pop_head_unchecked();
        match head {
            NodeKey::Exact(_) => {
                self.get_child(&head).map(|child| callback(UnexploredPath::new(child, key.clone())));
                self.get_child(&NodeKey::Wildcard).map(|child| callback(UnexploredPath::new(child, key)));
            },
            NodeKey::ExpressionEnd => {
                self.get_child(&head).map(|child| callback(UnexploredPath::new(child, key)));
            }
            NodeKey::Expression(expr_len) => {
                self.get_child(&NodeKey::Wildcard).map(|child| callback(UnexploredPath::new(child, key.skip_expr(expr_len))));
                self.get_child(&NodeKey::ExpressionBegin).map(|child| callback(UnexploredPath::new(child, key)));
            },
            NodeKey::Wildcard => {
                self.children.iter()
                    .filter(|(key, _child)| !key.is_expr_begin_or_end())
                    .map(|(_key, child)| child)
                    .for_each(|child| callback(UnexploredPath::new(child.as_ptr(), key.clone())));
                self.expression_wildcards.values()
                    .for_each(|child| callback(UnexploredPath::new(child.as_ptr(), key.clone())));
            },
            NodeKey::ExpressionBegin => panic!(concat!(
                    "NodeKey::ExpressionBegin used only for indexing never for searching.",
                    "Should not be included into a key created from atom.")),
        }
    }

    pub fn add(&mut self, mut key: TrieKey<K>, value: V) {
        log::debug!("MultiTrie::add(): key: {:?}, value: {:?}", key, value);
        if key.is_empty() {
            self.values.insert(value);
        } else {
            let mut nodes: Vec<Shared<Self>> = vec![];
            let mut exprs: Vec<(usize, usize)> = Vec::new();
            let mut pos = 0;
            
            let mut head = key.pop_head_unchecked();
            if let NodeKey::Expression(size) = head {
                exprs.push((pos, pos + size + 1));
                head = NodeKey::ExpressionBegin;
            }
            let mut cur = self.get_or_insert_child(head);
            nodes.push(cur.clone());
            pos = pos + 1;

            loop {
                match key.pop_head() {
                    None => {
                        cur.borrow_mut().values.insert(value);
                        break;
                    },
                    Some(mut head) => {
                        if let NodeKey::Expression(size) = head {
                            exprs.push((pos, pos + size + 1));
                            head = NodeKey::ExpressionBegin;
                        }
                        let node = cur.borrow_mut().get_or_insert_child(head);
                        cur = node;
                        nodes.push(cur.clone());
                    },
                }
                pos = pos + 1
            }
            for (start, end) in exprs {
                let end_node = nodes[end - 1].clone();
                if start > 0 {
                    nodes[start - 1].borrow_mut().expression_wildcards.insert(end_node.as_ptr(), end_node);
                } else {
                    self.expression_wildcards.insert(end_node.as_ptr(), end_node);
                }
            }
        }
    }

    // TODO: at the moment the method doesn't remove the key from the index. 
    // It removes only value.  It can be fixed by using links to parent in the
    // MultiTrieNode nodes and cleaning up the map entries which point to the empty
    // nodes only.
    pub fn remove(&mut self, key: TrieKey<K>, value: &V) -> bool {
        log::debug!("MultiTrieNode::remove(): key: {:?}, value: {:?}", key, value);
        ValueMutExplorer::new(self, key, MultiTrieNode::remove_exploring_strategy)
            .map(|node| node.remove_value(value)).fold(false, |a, b| a | b)
    }

    #[inline]
    fn remove_value(&mut self, value: &V) -> bool {
        self.values.remove(value)
    }

    pub fn get(&self, key: TrieKey<K>) -> impl Iterator<Item=&V> {
        ValueExplorer::new(self, key, MultiTrieNode::get_exploring_strategy)
            .flat_map(|node| node.values.iter())
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

    #[test]
    fn multi_trie_add_basic() {
        let mut trie = MultiTrie::new();

        let exact_a = TrieKey::from_list([NodeKey::Exact("A")]);
        let exact_b = TrieKey::from_list([NodeKey::Exact("B")]);
        let wild = TrieKey::from_list([NodeKey::Wildcard]);
        let expr_a_b = TrieKey::from_list([NodeKey::Expression(3)
            , NodeKey::Exact("A"), NodeKey::Exact("B")
            , NodeKey::ExpressionEnd]);
        let expr_a_c = TrieKey::from_list([NodeKey::Expression(3)
            , NodeKey::Exact("A"), NodeKey::Exact("C")
            , NodeKey::ExpressionEnd]);

        trie.add(exact_a.clone(), "exact_a");
        trie.add(wild.clone(), "wild");
        trie.add(expr_a_b.clone(), "expr_a_b");

        assert_eq!(trie.get(exact_a).to_sorted(), vec!["exact_a", "wild"]);
        assert_eq!(trie.get(exact_b).to_sorted(), vec!["wild"]);

        assert_eq!(trie.get(wild).to_sorted(), vec!["exact_a", "expr_a_b", "wild"]);

        assert_eq!(trie.get(expr_a_b).to_sorted(), vec!["expr_a_b", "wild"]);
        assert_eq!(trie.get(expr_a_c).to_sorted(), vec!["wild"]);
    }

    #[test]
    fn multi_trie_add_expr() {
        let mut trie = MultiTrie::new();

        let expr_a_b = TrieKey::from_list([NodeKey::Expression(5)
            , NodeKey::Expression(2) , NodeKey::Exact("A"), NodeKey::ExpressionEnd
            , NodeKey::Exact("B") , NodeKey::ExpressionEnd]);
        let expr_x_b = TrieKey::from_list([NodeKey::Expression(3)
            , NodeKey::Wildcard, NodeKey::Exact("B") , NodeKey::ExpressionEnd]);
        let expr_x_c = TrieKey::from_list([NodeKey::Expression(3)
            , NodeKey::Wildcard, NodeKey::Exact("C") , NodeKey::ExpressionEnd]);
        let expr_a_c = TrieKey::from_list([NodeKey::Expression(5)
            , NodeKey::Expression(2) , NodeKey::Exact("A"), NodeKey::ExpressionEnd
            , NodeKey::Exact("C") , NodeKey::ExpressionEnd]);

        trie.add(expr_a_b, "expr_a_b");
        trie.add(expr_x_c, "expr_x_c");

        assert_eq!(trie.get(expr_x_b).to_sorted(), vec!["expr_a_b"]);
        assert_eq!(trie.get(expr_a_c).to_sorted(), vec!["expr_x_c"]);
    }

    #[test]
    fn multi_trie_add_subexpr_twice() {
        let mut trie: MultiTrie<&'static str, &'static str> = MultiTrie::new();
        let empty_expr = TrieKey::from_list([NodeKey::Expression(1), NodeKey::ExpressionEnd]);
        let wildcard: TrieKey<&'static str> = TrieKey::from_list([NodeKey::Wildcard]);

        trie.add(empty_expr.clone(), "empty_expr");
        trie.add(empty_expr.clone(), "empty_expr");

        assert_eq!(trie.get(empty_expr).to_sorted(), vec!["empty_expr"]);
        assert_eq!(trie.get(wildcard).to_sorted(), vec!["empty_expr"]);
    }

    #[test]
    fn multi_trie_twice_result_because_of_subexpr() {
        let mut trie = MultiTrie::new();
        let expr_a = TrieKey::from_list([NodeKey::Expression(2), NodeKey::Exact("A"), NodeKey::ExpressionEnd]);
        let expr_b = TrieKey::from_list([NodeKey::Expression(2), NodeKey::Exact("B"), NodeKey::ExpressionEnd]);
        let expr_x = TrieKey::from_list([NodeKey::Expression(2), NodeKey::Wildcard, NodeKey::ExpressionEnd]);

        trie.add(expr_a.clone(), "expr_a");
        trie.add(expr_b.clone(), "expr_b");

        println!("trie: {:?}", trie);

        assert_eq!(trie.get(expr_x).to_sorted(), vec!["expr_a", "expr_b"]);
    }

    #[test]
    fn multi_trie_remove_basic() {
        let mut trie = MultiTrie::new();

        let exact_a = TrieKey::from_list([NodeKey::Exact("A")]);
        let wild = TrieKey::from_list([NodeKey::Wildcard]);
        let expr_a_b = TrieKey::from_list([NodeKey::Expression(3)
            , NodeKey::Exact("A"), NodeKey::Exact("B")
            , NodeKey::ExpressionEnd]);

        trie.add(exact_a.clone(), "exact_a");
        trie.add(wild.clone(), "wild");
        trie.add(expr_a_b.clone(), "expr_a_b");

        trie.remove(exact_a.clone(), &"exact_a");
        trie.remove(wild.clone(), &"wild");
        trie.remove(expr_a_b.clone(), &"expr_a_b");

        assert!(trie.get(exact_a).to_sorted().is_empty());
        assert!(trie.get(wild).to_sorted().is_empty());
        assert!(trie.get(expr_a_b).to_sorted().is_empty());
    }

    #[test]
    fn trie_key_debug() {
        let exact_a: TrieKey<&str> = TrieKey::from_list([NodeKey::Exact("A")]);
        let wild: TrieKey<&str> = TrieKey::from_list([NodeKey::Wildcard]);
        let expr_a_b: TrieKey<&str> = TrieKey::from_list([NodeKey::Expression(3)
            , NodeKey::Exact("A"), NodeKey::Exact("B")
            , NodeKey::ExpressionEnd]);

        assert_eq!(format!("{:?}", exact_a), "TrieKey([Exact(\"A\")])");
        assert_eq!(format!("{:?}", wild), "TrieKey([Wildcard])");
        assert_eq!(format!("{:?}", expr_a_b), "TrieKey([Expression(3), Exact(\"A\"), Exact(\"B\"), ExpressionEnd])");
    }

    #[test]
    fn multi_trie_clone() {
        let mut trie = MultiTrie::new();
        let key = TrieKey::from_list([NodeKey::Exact(0), NodeKey::Wildcard,
            NodeKey::Expression(2), NodeKey::Wildcard, NodeKey::ExpressionEnd]);
        trie.add(key.clone(), "test");

        let copy = trie.clone();

        assert_eq!(copy.get(key).to_sorted(), vec!["test"]);
    }

    #[test]
    fn multi_trie_add_key_with_many_subexpr() {
        fn with_subexpr(nvars: usize) -> TrieKey<NodeKey<usize>> {
            let mut keys = Vec::new();
            keys.push(NodeKey::Expression(nvars * 2 + 1));
            for _i in 0..nvars {
                keys.push(NodeKey::Expression(1));
                keys.push(NodeKey::ExpressionEnd);
            }
            keys.push(NodeKey::ExpressionEnd);
            TrieKey::from_list(keys)
        }
        let mut trie = MultiTrie::new();

        trie.add(with_subexpr(4), 0);
        assert_eq!(trie.size(), 5*2 + 1);

        trie.add(with_subexpr(8), 0);
        assert_eq!(trie.size(), 20);
    }
}
