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
//! keeping value. Thus a single key in the [MultiTrie] can match many different keys
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

    fn iter(&self) -> TrieKeyIter<'_, T> {
        TrieKeyIter{ key: self, pos: 0 }
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

#[derive(Clone)]
struct TrieKeyIter<'a, T> {
    key: &'a TrieKey<T>,
    pos: usize,
}

impl<'a, T> TrieKeyIter<'a, T> {
    fn is_end(&self) -> bool {
        self.pos >= self.key.tokens.len()
    }

    fn skip_tokens(mut self) -> Self {
        assert!(self.pos > 0 && self.key.expr_size[self.pos - 1] > 0);
        self.pos += self.key.expr_size[self.pos - 1];
        self
    }
}

impl<'a, T> Iterator for TrieKeyIter<'a, T> {
    type Item = &'a TrieToken<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.key.tokens.get(self.pos);
        if token.is_some() {
            self.pos = self.pos + 1;
        }
        token
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

/// Multi-value trie with double side matching. See [crate::common::multitrie]
/// for the algorithm description.
#[derive(Clone, Debug)]
pub struct MultiTrie<K, V>(MultiTrieNode<K, V>);

impl<K, V> MultiTrie<K, V>
where
    K: Debug + Clone + Eq + Hash,
    V: Debug + Eq + Hash,
{
    /// Constructs new empty [MultiTrie] instance.
    pub fn new() -> Self {
        Self(MultiTrieNode::new())
    }

    /// Insert the given `value` by the given `key`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::common::multitrie::*;
    ///
    /// fn collect<'a, T, I>(it: I) -> Vec<T> where T: Clone + 'a, I: Iterator<Item=&'a T>, {
    ///     it.cloned().collect()
    /// }
    ///
    /// let mut trie = MultiTrie::new();
    ///
    /// let ab = TrieKey::from([TrieToken::Exact("A"), TrieToken::Exact("B")]);
    /// let ac = TrieKey::from([TrieToken::Exact("A"), TrieToken::Exact("C")]);
    ///
    /// trie.insert(ab.clone(), "AB");
    /// trie.insert(ac.clone(), "AC");
    ///
    /// assert_eq!(collect(trie.get(&ab)), vec!["AB"]);
    /// assert_eq!(collect(trie.get(&ac)), vec!["AC"]);
    /// ```
    pub fn insert(&mut self, key: TrieKey<K>, value: V) {
        log::debug!("MultiTrie::insert(): key: {:?}, value: {:?}", key, value);
        self.0.insert(key, value)
    }

    /// Get values from the trie by the given `key`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::common::multitrie::*;
    ///
    /// fn collect<'a, T, I>(it: I) -> Vec<T> where T: Clone + 'a, I: Iterator<Item=&'a T>, {
    ///     it.cloned().collect()
    /// }
    ///
    /// let mut trie = MultiTrie::new();
    ///
    /// let ax = TrieKey::from([TrieToken::Exact("A"), TrieToken::Wildcard]);
    /// let ab = TrieKey::from([TrieToken::Exact("A"), TrieToken::Exact("B")]);
    /// let ae = TrieKey::from([TrieToken::Exact("A"), TrieToken::LeftPar,
    ///                         TrieToken::Exact("B"), TrieToken::RightPar]);
    ///
    /// trie.insert(ax.clone(), "A*");
    ///
    /// assert_eq!(collect(trie.get(&ax)), vec!["A*"]);
    /// assert_eq!(collect(trie.get(&ab)), vec!["A*"]);
    /// assert_eq!(collect(trie.get(&ae)), vec!["A*"]);
    /// ```
    pub fn get<'a>(&'a self, key: &'a TrieKey<K>) -> impl Iterator<Item=&'a V> + 'a {
        self.0.get(key)
    }

    /// Remove the given `value` by the given `key`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::common::multitrie::*;
    ///
    /// fn collect<'a, T, I>(it: I) -> Vec<T> where T: Clone + 'a, I: Iterator<Item=&'a T>, {
    ///     it.cloned().collect()
    /// }
    ///
    /// let mut trie = MultiTrie::new();
    ///
    /// let ab = TrieKey::from([TrieToken::Exact("A"), TrieToken::Exact("B")]);
    /// let ac = TrieKey::from([TrieToken::Exact("A"), TrieToken::Exact("C")]);
    /// let ax = TrieKey::from([TrieToken::Exact("A"), TrieToken::Wildcard]);
    ///
    /// trie.insert(ab.clone(), "AB");
    /// trie.insert(ac.clone(), "AC");
    ///
    /// assert_eq!(collect(trie.get(&ab)), vec!["AB"]);
    /// assert!(trie.remove(&ax, &"AB"));
    /// assert!(!trie.remove(&ax, &"AB"));
    /// assert_eq!(trie.get(&ab).count(), 0);
    /// assert_eq!(collect(trie.get(&ac)), vec!["AC"]);
    /// ```
    pub fn remove(&mut self, key: &TrieKey<K>, value: &V) -> bool {
        log::debug!("MultiTrie::remove(): key: {:?}, value: {:?}", key, value);
        self.0.remove(key, value)
    }

    #[cfg(test)]
    fn size(&self) -> usize {
        self.0.size()
    }
}

#[derive(Clone, Debug)]
struct MultiTrieNode<K, V> {
    children: HashMap<TrieToken<K>, Shared<Self>>,
    end_of_expr: HashMap<*mut Self, Shared<Self>>,
    values: HashSet<V>,
}

impl<K, V> MultiTrieNode<K, V>
where
    K: Clone + Eq + Hash,
    V: Eq + Hash,
{

    fn new() -> Self {
        Self{
            children: HashMap::new(),
            end_of_expr: HashMap::new(),
            values: HashSet::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.children.is_empty() && self.values.is_empty()
    }

    fn get_or_insert_child(&mut self, token: TrieToken<K>) -> Shared<Self> {
        self.children.entry(token).or_insert(Shared::new(Self::new())).clone()
    }

    fn next<'a, 'b: 'a>(&'a self, mut key: TrieKeyIter<'b, K>) ->
        impl Iterator<Item=(Option<&'a TrieToken<K>>, &'a Shared<Self>, TrieKeyIter<'b, K>)>
    {
        let mut result = Vec::new();
        match key.next() {
            Some(token) => match token {
                TrieToken::Exact(_) => {
                    self.children.get(token).map(|child| result.push((Some(token), child, key.clone())));
                    self.children.get(&TrieToken::Wildcard).map(|child| result.push((Some(&TrieToken::Wildcard), child, key)));
                },
                TrieToken::RightPar => {
                    self.children.get(token).map(|child| result.push((Some(token), child, key)));
                },
                TrieToken::LeftPar => {
                    self.children.get(&TrieToken::LeftPar)
                        .map(|child| result.push((Some(&TrieToken::LeftPar), child, key.clone())));
                    self.children.get(&TrieToken::Wildcard)
                        .map(|child| result.push((Some(&TrieToken::Wildcard), child, key.skip_tokens())));
                },
                TrieToken::Wildcard => {
                    self.children.iter()
                        .filter(|(token, _child)| !token.is_parenthesis())
                        .for_each(|(token, child)| result.push((Some(token), child, key.clone())));
                    self.end_of_expr.values()
                        .for_each(|child| result.push((None, child, key.clone())));
                },
            },
            None => {},
        }
        result.into_iter()
    }

    fn remove(&mut self, key: &TrieKey<K>, value: &V) -> bool {
        self.remove_internal(key.iter(), value)
    }

    fn remove_internal(&mut self, key: TrieKeyIter<K>, value: &V) -> bool {
        if key.is_end() {
            self.values.remove(value)
        } else {
            let children: Vec<(Option<TrieToken<K>>, Shared<Self>, TrieKeyIter<K>)> = self.next(key)
                .map(|(token, child_node, key)| (token.cloned(), child_node.clone(), key))
                .collect();
            children.into_iter().map(|(token, child_node, key)| {
                let removed = child_node.borrow_mut().remove_internal(key, value);
                if removed && child_node.borrow().is_empty(){
                    match token {
                        Some(token) => { self.children.remove(&token); },
                        None => { self.end_of_expr.remove(&child_node.as_ptr()); },
                    }
                }
                removed
            })
            .fold(false, |a, b| a || b)
        }
    }
    
    fn insert(&mut self, mut key: TrieKey<K>, value: V) {
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
                    nodes[start - 1].borrow_mut().end_of_expr.insert(end_node.as_ptr(), end_node);
                } else {
                    self.end_of_expr.insert(end_node.as_ptr(), end_node);
                }
            }
        }
    }

    fn get<'a>(&'a self, key: &'a TrieKey<K>) -> impl Iterator<Item=&'a V> + 'a {
        MultiValueIter::new(self, key.iter()).flat_map(|node| node.values.iter())
    }

    #[cfg(test)]
    fn size(&self) -> usize {
        let mut visited = HashSet::new();
        fn size_recursive<K, V>(node: &MultiTrieNode<K, V>, visited: &mut HashSet<*const MultiTrieNode<K, V>>) -> usize {
            let ptr = node as *const MultiTrieNode<K, V>;
            if !visited.contains(&ptr) {
                visited.insert(ptr);
                node.children.values().fold(1, |size, node| {
                    size + size_recursive(node.borrow().as_ref(), visited)
                })
            } else {
                0
            }
        }
        size_recursive(self, &mut visited)
    }
}

struct MultiValueIter<'a, K, V> {
    to_be_explored: Vec<(*mut MultiTrieNode<K, V>, TrieKeyIter<'a, K>)>,
    _root_node_ref: PhantomData<&'a MultiTrieNode<K, V>>,
}

impl<'a, K, V> MultiValueIter<'a, K, V>
where
    K: Clone + Eq + Hash,
    V: Eq + Hash,
{
    fn new(node: &'a MultiTrieNode<K, V>, key: TrieKeyIter<'a, K>) -> Self {
        let to_be_explored = node.next(key).map(Self::to_unexplored_path).collect();
        Self{ to_be_explored, _root_node_ref: PhantomData }
    }

    fn to_unexplored_path((_token, child, key): (Option<&TrieToken<K>>, &Shared<MultiTrieNode<K, V>>, TrieKeyIter<'a, K>)) -> (*mut MultiTrieNode<K, V>, TrieKeyIter<'a, K>) {
        (child.as_ptr(), key)
    }
}

impl<'a, K, V> Iterator for MultiValueIter<'a, K, V>
where
    K: Clone + Eq + Hash,
    V: Eq + Hash,
{
    type Item = &'a MultiTrieNode<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, key)) = self.to_be_explored.pop() {
            let node = unsafe{ &*node };
            match key.is_end() {
                true => return Some(node),
                false => node.next(key)
                    .map(MultiValueIter::to_unexplored_path)
                    .for_each(|x| self.to_be_explored.push(x)),
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

        trie.insert(triekey!("A"), "exact_a");
        trie.insert(triekey!(*), "wild");
        trie.insert(triekey!(["A", "B"]), "pars_a_b");
        trie.insert(triekey!("A", "B"), "a_b");

        assert_eq!(trie.get(&triekey!("A")).to_sorted(), vec!["exact_a", "wild"]);
        assert_eq!(trie.get(&triekey!("B")).to_sorted(), vec!["wild"]);
        assert_eq!(trie.get(&triekey!(*)).to_sorted(), vec!["exact_a", "pars_a_b", "wild"]);
        assert_eq!(trie.get(&triekey!(["A", "B"])).to_sorted(), vec!["pars_a_b", "wild"]);
        assert_eq!(trie.get(&triekey!(["A", "C"])).to_sorted(), vec!["wild"]);
        assert_eq!(trie.get(&triekey!(["A", *])).to_sorted(), vec!["pars_a_b", "wild"]);
        assert_eq!(trie.get(&triekey!("A", "B")).to_sorted(), vec!["a_b"]);
        assert_eq!(trie.get(&triekey!("A", "C")).to_sorted(), vec![] as Vec<&str>);
        assert_eq!(trie.get(&triekey!("A", *)).to_sorted(), vec!["a_b"]);
    }

    #[test]
    fn multi_trie_add_pars() {
        let mut trie = MultiTrie::new();

        trie.insert(triekey!(["A", "B"]), "pars_a_b");
        trie.insert(triekey!([*, "C"]), "pars_x_c");

        assert_eq!(trie.get(&triekey!([*, "B"])).to_sorted(), vec!["pars_a_b"]);
        assert_eq!(trie.get(&triekey!(["A", "C"])).to_sorted(), vec!["pars_x_c"]);
    }

    #[test]
    fn multi_trie_add_subpars_twice() {
        let mut trie: MultiTrie<&'static str, &'static str> = MultiTrie::new();

        trie.insert(triekey!([]), "empty_pars");
        trie.insert(triekey!([]).clone(), "empty_pars");

        assert_eq!(trie.get(&triekey!([])).to_sorted(), vec!["empty_pars"]);
        assert_eq!(trie.get(&triekey!(*)).to_sorted(), vec!["empty_pars"]);
    }

    #[test]
    fn multi_trie_twice_result_because_of_subpars() {
        let mut trie = MultiTrie::new();

        trie.insert(triekey!(["A"]), "pars_a");
        trie.insert(triekey!(["B"]), "pars_b");

        assert_eq!(trie.get(&triekey!([*])).to_sorted(), vec!["pars_a", "pars_b"]);
    }

    #[test]
    fn multi_trie_remove_basic() {
        let mut trie = MultiTrie::new();
        let empty_trie_size = trie.size();
        trie.insert(triekey!("A"), "exact_a");
        trie.insert(triekey!(*), "wild");
        trie.insert(triekey!(["A", "B"]), "pars_a_b");
        trie.insert(triekey!("A", "B"), "a_b");

        trie.remove(&triekey!("A"), &"exact_a");
        trie.remove(&triekey!(*), &"wild");
        trie.remove(&triekey!(["A", "B"]), &"pars_a_b");
        trie.remove(&triekey!("A", "B"), &"a_b");

        assert!(trie.get(&triekey!("A")).to_sorted().is_empty());
        assert!(trie.get(&triekey!(*)).to_sorted().is_empty());
        assert!(trie.get(&triekey!(["A", "B"])).to_sorted().is_empty());
        assert!(trie.get(&triekey!("A", "B")).to_sorted().is_empty());
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
        trie.insert(key.clone(), "test");

        let copy = trie.clone();

        assert_eq!(copy.get(&key).to_sorted(), vec!["test"]);
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

        trie.insert(with_subpars(4), 0);
        assert_eq!(trie.size(), 5*2 + 1);

        trie.insert(with_subpars(8), 0);
        assert_eq!(trie.size(), 20);
    }
}
