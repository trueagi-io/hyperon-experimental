//! Multi-value trie with double side matching.
//!
//! Key of the trie is a sequence of tokens. The most common token type is
//! an [TrieToken::Exact] token. It contains a value and matches with another [TrieToken::Exact] token
//! having the equal value. For example key `[ Exact(A), Exact(B) ]` matches with
//! the equal key but doesn't match with `[ Exact(A), Exact(C) ]`.
//! But [TrieKey] containing [TrieToken::Exact] token is not the only kind of keys supported.
//! [MultiTrie] also supports wildcards and sub-expressions.
//!
//! Subexpression is a part of the key between a pair of left and right parentheses.
//! [TrieToken::LeftPar] and [TrieToken::RightPar] are also kinds of tokens.
//! Parentheses mark the edges of the key subsequence which can be matched as a whole.
//! Exact token doesn't match with a part of a subexpression unless it is also
//! a part of a subexpression and located on the same position as a matched token.
//! Subexpressions can be nested and in such case parentheses should be correctly balanced.
//! For example `[ Exact(A), LeftPar, Exact(B), RightPar ]` is a key which contains
//! a subexpression. It can be matched with the same key but doesn't match with
//! `[ Exact(A), Exact(B) ]`.
//!
//! The [TrieToken::Wildcard] is a last kind of token which matches the exact value, the whole
//! sub-expression or another wildcard. It can recognize anything but when dealing
//! with subexpression it matches with the whole subexpression only. It cannot
//! be matched with a part of the subexpression or with the left or right parenthesis.
//! For example `[ Exact(A), * ]` key matches both `[ Exact(A), Exact(B) ]` and
//! `[ Exact(A), LeftPar, Exact(B), RightPar ]` keys.
//!
//! Wildcard can be used not only for getting value but also as a key for
//! keeping value. Thus a singly key in the [MultiTrie] can match many different keys
//! for retrieve value. In the example above we could put two
//! values with `[ Exact(A), Exact(B) ]` and `[ Exact(A), LeftPar, Exact(B), RightPar ]`
//! keys into the trie and then get both of them using `[ Exact(A), * ]` key.
//! Or vice versa put a single value using `[ Exact(A), * ]` key and extract
//! it using two keys above. This is what is called double-side matching.
//!
//! Because of double-side matching [MultiTrie] cannot guarantie that single key
//! corresponds to the single value. Thus [MultiTrie::get] call returns the
//! iterator through the values which keys are matched with given key.

use std::fmt::{Debug, Display};
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::marker::PhantomData;
use crate::common::shared::Shared;

/// Single token of [TrieKey]. Each kind of token has its own recognition rules.
#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum TrieToken<T> {
    /// Exact token recognizes another instance of [TrieToken::Exact] which
    /// has the equal value inside.
    Exact(T),
    /// Whildcard token recognizes [TrieToken::Exact] with any value inside,
    /// another [TrieToken::Wildcard] or the whole sub-expression from
    /// [TrieToken::LeftPar] to [TrieToken::RightPar].
    Wildcard,
    /// LeftPar designates beginning of the sub-expression. It recognizes
    /// another [TrieToken::LeftPar] or [TrieToken::Wildcard].
    LeftPar,
    /// RightPar designates end of the sub-expression. It recognizes another
    /// [TrieToken::RightPar] only.
    RightPar,
}

impl<T: PartialEq> TrieToken<T> {
    fn is_parenthesis(&self) -> bool {
        *self == TrieToken::RightPar || *self == TrieToken::LeftPar
    }
}

impl<T: Display> Display for TrieToken<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TrieToken::Exact(val) => write!(f, "Exact({})", val),
            TrieToken::Wildcard => write!(f, "*"),
            TrieToken::LeftPar => write!(f, "LeftPar"),
            TrieToken::RightPar => write!(f, "RightPar"),
        }
    }
}

/// Trie key is a sequence of [TrieToken].
#[derive(PartialEq, Clone, Debug)]
pub struct TrieKey<T> {
    tokens: VecDeque<TrieToken<T>>,
    expr_size: VecDeque<usize>,
}

impl<T> TrieKey<T> {
    fn precalculate_expr_size(tokens: &VecDeque<TrieToken<T>>) -> Result<VecDeque<usize>, String> {
        fn unbalanced_right(pos: usize) -> String {
            format!(concat!("Unbalanced key: TrieToken::RightPar without ",
                    "TrieToken::LeftPar at position {}"), pos)
        }
        fn unbalanced_left(left_par_stack: Vec<usize>) -> String {
            format!(concat!("Unbalanced key: TrieToken::LeftPar without ",
                    "TrieToken::Right at positions {:?}"), left_par_stack)
        }

        let mut left_par_stack = Vec::new();
        let mut expr_size = VecDeque::with_capacity(tokens.len());
        for (pos, token) in tokens.iter().enumerate() {
            expr_size.push_back(0);
            match token {
                TrieToken::LeftPar => left_par_stack.push(pos),
                TrieToken::RightPar => {
                    let left_pos = left_par_stack.pop().ok_or_else(|| unbalanced_right(pos))?;
                    expr_size[left_pos] = pos - left_pos;
                },
                _ => {},
            }
        }
        if left_par_stack.is_empty() {
            Ok(expr_size)
        } else {
            Err(unbalanced_left(left_par_stack))
        }
    }

    fn pop_head(&mut self) -> Option<(TrieToken<T>, usize)> {
        match (self.tokens.pop_front(), self.expr_size.pop_front()) {
            (Some(token), Some(size)) => Some((token, size)),
            _ => None,
        }
    }

    fn pop_head_unchecked(&mut self) -> (TrieToken<T>, usize) {
        self.pop_head().expect("Unexpected end of key")
    }

    fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
}

impl<T, V: Into<VecDeque<TrieToken<T>>>> From<V> for TrieKey<T> {
    fn from(tokens: V) -> Self {
        let panic = |err| { panic!("{}", err) };
        let tokens = tokens.into();
        let expr_size = Self::precalculate_expr_size(&tokens).unwrap_or_else(panic);
        Self{ tokens, expr_size }
    }
}

impl<T: Clone> TrieKey<T> {
    fn skip_tokens(&self, size: usize) -> Self {
        Self{
            tokens: self.tokens.iter().skip(size).cloned().collect(),
            expr_size: self.expr_size.iter().skip(size).cloned().collect(),
        }
    }
}

impl<T: Display> Display for TrieKey<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[ ")
            .and_then(|_| self.tokens.iter().take(1).fold(Ok(()),
                |res, token| res.and_then(|_| write!(f, "{}", token))))
            .and_then(|_| self.tokens.iter().skip(1).fold(Ok(()),
                |res, token| res.and_then(|_| write!(f, ", {}", token))))
            .and_then(|_| write!(f, " ]"))
    }
}

pub type MultiTrie<K, V> = MultiTrieNode<K, V>;

#[derive(Clone, Debug)]
pub struct MultiTrieNode<K, V> {
    children: HashMap<TrieToken<K>, Shared<Self>>,
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

    fn get_or_insert_child(&mut self, token: TrieToken<K>) -> Shared<Self> {
        self.children.entry(token).or_insert(Shared::new(Self::new())).clone()
    }

    fn next<F, R>(&self, mut key: TrieKey<K>, map: F, result: &mut Vec<R>)
        where F: Fn(Option<TrieToken<K>>, &Shared<Self>, TrieKey<K>) -> R,
    {
        let (token, size) = key.pop_head_unchecked();
        match token {
            TrieToken::Exact(_) => {
                self.children.get(&token).map(|child| result.push(map(Some(token), child, key.clone())));
                self.children.get(&TrieToken::Wildcard).map(|child| result.push(map(Some(TrieToken::Wildcard), child, key)));
            },
            TrieToken::RightPar => {
                self.children.get(&token).map(|child| result.push(map(Some(token), child, key)));
            },
            TrieToken::LeftPar => {
                self.children.get(&TrieToken::Wildcard)
                    .map(|child| result.push(map(Some(TrieToken::Wildcard), child, key.skip_tokens(size))));
                self.children.get(&TrieToken::LeftPar)
                    .map(|child| result.push(map(Some(TrieToken::LeftPar), child, key)));
            },
            TrieToken::Wildcard => {
                self.children.iter()
                    .filter(|(token, _child)| !token.is_parenthesis())
                    .for_each(|(token, child)| result.push(map(Some(token.clone()), child, key.clone())));
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
            self.next(key, |token, child_node, key| (token, child_node.clone(), key), &mut children);
            children.into_iter()
                .map(|(token, child_node, key)| {
                    let removed = child_node.borrow_mut().remove(key, value);
                    if removed && child_node.borrow().is_empty() {
                        match token {
                            Some(token) => { self.children.remove(&token); },
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
            
            let (token, size) = key.pop_head_unchecked();
            if token == TrieToken::LeftPar {
                pars.push((pos, pos + size + 1));
            }
            let mut cur = self.get_or_insert_child(token);
            nodes.push(cur.clone());
            pos = pos + 1;

            loop {
                match key.pop_head() {
                    None => {
                        cur.borrow_mut().values.insert(value);
                        break;
                    },
                    Some((token, size)) => {
                        if token == TrieToken::LeftPar {
                            pars.push((pos, pos + size + 1));
                        }
                        let node = cur.borrow_mut().get_or_insert_child(token);
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

    fn map_children(_token: Option<TrieToken<K>>,
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
        ($($x:tt)*) => { TrieKey::from(trietokens!($($x)*)) }
    }

    macro_rules! trietokens {
        () => { vec![] };
        (*) => { vec![ TrieToken::Wildcard ] };
        ($x:literal) => { vec![ TrieToken::Exact($x) ] };
        ([]) => { vec![ vec![ TrieToken::LeftPar ], vec![ TrieToken::RightPar ] ].concat() };
        ([$($x:tt),*]) => { {
            vec![ vec![ TrieToken::LeftPar ], $( trietokens!($x) ),*, vec![ TrieToken::RightPar ] ].concat()
        } };
        ($($x:tt),*) => { vec![ $( trietokens!($x) ),* ].concat() };
    }

    #[test]
    fn triekey_macro() {
        assert_eq!(triekey!() as TrieKey<u32>, TrieKey::from([ ]));
        assert_eq!(triekey!(*) as TrieKey<u32>, TrieKey::from([TrieToken::Wildcard]));
        assert_eq!(triekey!(0), TrieKey::from([TrieToken::Exact(0)]));
        assert_eq!(triekey!([]) as TrieKey<u32>, TrieKey::from([
                TrieToken::LeftPar,TrieToken::RightPar]));
        assert_eq!(triekey!([0, *]), TrieKey::from([
                TrieToken::LeftPar, TrieToken::Exact(0),
                TrieToken::Wildcard, TrieToken::RightPar]));
        assert_eq!(triekey!([[0, *]]), TrieKey::from([
                TrieToken::LeftPar, TrieToken::LeftPar,
                TrieToken::Exact(0), TrieToken::Wildcard,
                TrieToken::RightPar, TrieToken::RightPar]));
        assert_eq!(triekey!(0, *, [*]), TrieKey::from([
                TrieToken::Exact(0), TrieToken::Wildcard,
                TrieToken::LeftPar, TrieToken::Wildcard,
                TrieToken::RightPar]));
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
        assert_eq!(format!("{}", triekey!("A")), "[ Exact(A) ]");
        assert_eq!(format!("{}", triekey!(*) as TrieKey<u32>), "[ * ]");
        assert_eq!(format!("{}", triekey!(["A", "B", *])), "[ LeftPar, Exact(A), Exact(B), *, RightPar ]");
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
        fn with_subpars(nvars: usize) -> TrieKey<TrieToken<usize>> {
            let mut tokens = Vec::new();
            tokens.push(TrieToken::LeftPar);
            for _i in 0..nvars {
                tokens.push(TrieToken::LeftPar);
                tokens.push(TrieToken::RightPar);
            }
            tokens.push(TrieToken::RightPar);
            TrieKey::from(tokens)
        }
        let mut trie = MultiTrie::new();

        trie.add(with_subpars(4), 0);
        assert_eq!(trie.size(), 5*2 + 1);

        trie.add(with_subpars(8), 0);
        assert_eq!(trie.size(), 20);
    }
}
