// Macros to simplify bindings writing

#[macro_export]
macro_rules! bind {
    ($($k:ident: $v:expr),*) => {
        Bindings::from( vec![$( (VariableAtom::new(stringify!($k)), $v), )*])
    };
}

use super::*;

use std::collections::HashMap;
use delegate::delegate;

#[derive(Clone, PartialEq, Eq)]
pub struct Bindings(HashMap<VariableAtom, Atom>);

impl Bindings {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    delegate! {
        to self.0 {
            pub fn get(&self, k: &VariableAtom) -> Option<&Atom>;
            pub fn drain(&mut self) -> std::collections::hash_map::Drain<'_, VariableAtom, Atom>;
            pub fn insert(&mut self, k: VariableAtom, v: Atom) -> Option<Atom>;
            pub fn iter(&self) -> std::collections::hash_map::Iter<'_, VariableAtom, Atom>;
            pub fn remove(&mut self, k: &VariableAtom) -> Option<Atom>;
            pub fn is_empty(&self) -> bool;
        }
    }

    pub fn check_and_insert_binding(&mut self, var: &VariableAtom, value: &Atom) -> bool{
        let compatible = match self.get(var){
            Some(prev) => prev == value,
            None => true,
        };
        if compatible {
            self.insert(var.clone(), value.clone());
        }
        compatible
    }

    pub fn merge(prev: &Bindings, next: &Bindings) -> Option<Bindings> {
        if !prev.iter().all(|(k, v)| !next.0.contains_key(k)
                || next.0[k] == *v
                || matches!(next.0[k], Atom::Variable(_))) {
            log::trace!("Bindings::merge: {} ^ {} = None", prev, next);
            None
        } else {
            let mut res = Bindings::new();
            prev.iter().for_each(|(k, v)| { res.insert(k.clone(), v.clone()); });
            next.iter().filter(|(k, _)| !prev.0.contains_key(k))
                .for_each(|(k, v)| { res.insert(k.clone(), v.clone()); });
            log::trace!("Bindings::merge: {} ^ {} = {}", prev, next, res);
            Some(res) 
        }
    }

    pub fn product(prev: &Vec<Bindings>, next: &Vec<Bindings>) -> Vec<Bindings> {
        prev.iter().flat_map(|p| -> Vec<Option<Bindings>> {
            next.iter().map(|n| Self::merge(p, n)).collect()
        }).filter(Option::is_some).map(Option::unwrap).collect()
    }
}

impl From<Vec<(VariableAtom, Atom)>> for Bindings {
    fn from(mut pairs: Vec<(VariableAtom, Atom)>) -> Self {
        Bindings(pairs.drain(0..).collect())
    }
}

impl<'a> IntoIterator for &'a Bindings {
    type Item = (&'a VariableAtom, &'a Atom);
    type IntoIter = std::collections::hash_map::Iter<'a, VariableAtom, Atom>;

    #[inline]
    fn into_iter(self) -> std::collections::hash_map::Iter<'a, VariableAtom, Atom> {
        self.0.iter()
    }
}

impl Display for Bindings {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")
            .and_then(|_| self.0.iter().take(1).fold(Ok(()),
                |res, (k, v)| res.and_then(|_| write!(f, "{}: {}", k, v))))
            .and_then(|_| self.0.iter().skip(1).fold(Ok(()),
                |res, (k, v)| res.and_then(|_| write!(f, ", {}: {}", k, v))))
            .and_then(|_| write!(f, "}}"))
    }
}

impl Debug for Bindings {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchResult {
    pub candidate_bindings: Bindings,
    pub pattern_bindings: Bindings,
}

impl MatchResult {
    pub fn new() -> Self {
        MatchResult {
            candidate_bindings: Bindings::new(),
            pattern_bindings: Bindings::new()
        }
    }

    pub fn merge(prev: &MatchResult, next: &MatchResult) -> Option<MatchResult> {
        let candidate_bindings = Bindings::merge(&prev.candidate_bindings, &next.candidate_bindings);
        let pattern_bindings = Bindings::merge(&prev.pattern_bindings, &next.pattern_bindings);
        if let (Some(candidate_bindings), Some(pattern_bindings)) = (candidate_bindings, pattern_bindings) {
            Some(MatchResult::from((candidate_bindings, pattern_bindings)))
        } else {
            None
        }
    }
}

impl From<(Bindings, Bindings)> for MatchResult {
    fn from((candidate_bindings, pattern_bindings): (Bindings, Bindings)) -> Self {
        Self { candidate_bindings, pattern_bindings }
    }
}

pub type MatchResultIter = Box<dyn Iterator<Item=matcher::MatchResult>>;

pub trait WithMatch {
    fn match_(&self, pattern: &Atom) -> MatchResultIter;
}

impl WithMatch for Atom {
    fn match_(&self, pattern: &Atom) -> MatchResultIter {
        match (self, pattern) {
            (Atom::Symbol(a), Atom::Symbol(b)) if a == b => Box::new(std::iter::once(MatchResult::new())),
            (Atom::Grounded(a), Atom::Grounded(_)) => a.match_(pattern),
            (Atom::Variable(_), Atom::Variable(v)) => {
                // We stick to prioritize pattern bindings in this case
                // because otherwise the $X in (= (...) $X) will not be matched with
                // (= (if True $then) $then)
                log::trace!("match_(): bind a pattern's variable: {} = {}", v, self);
                Box::new(std::iter::once(MatchResult::from((Bindings::new(), Bindings::from(vec![(v.clone(), self.clone())])))))
            }
            (Atom::Variable(v), b) => {
                log::trace!("match_(): bind a candidate's variable: {} = {}", v, b);
                Box::new(std::iter::once(MatchResult::from((Bindings::from(vec![(v.clone(), b.clone())]), Bindings::new()))))
            }
            (a, Atom::Variable(v)) => {
                log::trace!("match_(): bind a pattern's variable: {} = {}", v, a);
                Box::new(std::iter::once(MatchResult::from((Bindings::new(), Bindings::from(vec![(v.clone(), a.clone())])))))
            },
            (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b }))
                if a.len() == b.len() => {
                a.iter().zip(b.iter()).fold(Box::new(std::iter::once(MatchResult::new())),
                    |acc, (a, b)| {
                        match_result_product_iter(acc, a.match_(b))
                    })
            },
            _ => Box::new(std::iter::empty()),
        }
    }
}

pub fn match_result_product_iter(prev: MatchResultIter, next: MatchResultIter) -> MatchResultIter {
    let next : Vec<MatchResult> = next.collect();
    log::trace!("match_result_product_iter, next: {:?}", next);
    Box::new(prev.flat_map(move |p| -> Vec<Option<MatchResult>> {
        next.iter().map(|n| MatchResult::merge(&p, n)).collect()
    }).filter(Option::is_some).map(Option::unwrap))
}
    
#[derive(Debug, PartialEq, Eq)]
pub struct UnificationPair {
    pub candidate: Atom,
    pub pattern: Atom,
}

impl From<(Atom, Atom)> for UnificationPair {
    fn from((candidate, pattern): (Atom, Atom)) -> Self {
        Self { candidate, pattern }
    }
}

pub type Unifications = Vec<matcher::UnificationPair>;

#[derive(Debug, PartialEq, Eq)]
pub struct UnifyResult {
    pub candidate_bindings: Bindings,
    pub pattern_bindings: Bindings,
    pub unifications: Unifications,
}

impl UnifyResult {
    fn new() -> Self {
        UnifyResult {
            candidate_bindings: Bindings::new(),
            pattern_bindings: Bindings::new(),
            unifications: Vec::new(),
        }
    }
}

fn unify_atoms_recursively(candidate: &Atom, pattern: &Atom, res: &mut UnifyResult, depth: u32) -> bool {
    match (candidate, pattern) {
        (Atom::Symbol(a), Atom::Symbol(b)) => a == b,
        (Atom::Grounded(a), Atom::Grounded(b)) => a == b,
        (Atom::Variable(_), Atom::Variable(v)) => {
            // We stick to prioritize pattern bindings in this case
            // because otherwise the $X in (= (...) $X) will not be matched with
            // (= (if True $then) $then)
            log::trace!("check_and_insert_binding for pattern({:?}, {}, {})", res.pattern_bindings, v, candidate);
            res.pattern_bindings.check_and_insert_binding(v, candidate)
        }
        (Atom::Variable(v), b) => {
            log::trace!("check_and_insert_binding for candidate({:?}, {}, {})", res.candidate_bindings, v, b);
            res.candidate_bindings.check_and_insert_binding(v, b)
        }
        (a, Atom::Variable(v)) => {
            log::trace!("check_and_insert_binding for pattern({:?}, {}, {})", res.pattern_bindings, v, a);
            res.pattern_bindings.check_and_insert_binding(v, a)
        },
        (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b })) => {
            if a.len() != b.len() {
                if depth == 1 {
                    false
                } else {
                    res.unifications.push((candidate.clone(), pattern.clone()).into());
                    true
                }
            } else {
                a.iter().zip(b.iter()).fold(true,
                    |succ, pair| succ && unify_atoms_recursively(pair.0, pair.1, res, depth + 1))
            }
        },
        (Atom::Expression(_), _) | (_, Atom::Expression(_)) => {
            res.unifications.push((candidate.clone(), pattern.clone()).into());
            true
        }
        _ => false,
    }
}

pub fn unify_atoms(candidate: &Atom, pattern: &Atom) -> Option<UnifyResult> {
    log::trace!("unify_atoms: candidate: {}, pattern: {}", candidate, pattern);
    let mut res = UnifyResult::new();
    if unify_atoms_recursively(candidate, pattern, &mut res, 0) {
        Some(res)
    } else {
        None
    }
}

pub fn apply_bindings_to_atom(atom: &Atom, bindings: &Bindings) -> Atom {
    if bindings.0.is_empty() {
        atom.clone()
    } else {
        let result = apply_bindings_to_atom_recurse(atom, bindings);
        log::trace!("applied bindings to {} result: {}", atom, result);
        return result;
    }
}

fn apply_bindings_to_atom_recurse(atom: &Atom, bindings: &Bindings) -> Atom {
    match atom {
        Atom::Symbol(_)|Atom::Grounded(_) => atom.clone(),
        Atom::Variable(v) => {
            if let Some(binding) = bindings.get(v) {
                binding.clone()
            } else {
                Atom::Variable(v.clone())
            }
        },
        Atom::Expression(ExpressionAtom{ children }) => {
            let children = children.iter()
                .map(|a| apply_bindings_to_atom_recurse(a, bindings))
                .collect::<Vec<Atom>>();
            Atom::expr(children)
        },
    }
}

fn atom_contains_variable(atom: &Atom, var: &VariableAtom) -> bool {
    match atom {
        Atom::Expression(ExpressionAtom{ children }) =>
            children.iter().any(|sub| atom_contains_variable(sub, var)),
        Atom::Variable(v) => v == var,
        _ => false,
    }
}

pub fn apply_bindings_to_bindings(from: &Bindings, to: &Bindings) -> Result<Bindings, ()> {
    log::trace!("apply_bindings_to_bindings: from: {}, to: {}", from, to);
    let mut res = Bindings::new();
    for (key, value) in to {
        let applied = apply_bindings_to_atom(value, from);
        // Check that variable is not expressed via itself, if so it is
        // a task for unification not for matching
        if !matches!(applied, Atom::Variable(_)) && atom_contains_variable(&applied, key) {
            log::trace!("apply_bindings_to_bindings: rejecting binding, variable is expressed via itself: key: {}, applied: {}", key, applied);
            return Err(())
        }
        if !res.check_and_insert_binding(key, &applied) {
            log::trace!("apply_bindings_to_bindings: rejecting binding, new value is not equal to previous one: ({}, {}) into {}", res, key, value);
            return Err(())
        }
    }
    return Ok(res);
}

pub fn atoms_are_equivalent(first: &Atom, second: &Atom) -> bool {
    atoms_are_equivalent_with_bindings(first, second, &mut Bindings::new(), &mut Bindings::new())
}

fn atoms_are_equivalent_with_bindings(first: &Atom, second: &Atom,
        direct_bindings: &mut Bindings, reverse_bindings: &mut Bindings) -> bool {
    match (first, second) {
        (Atom::Variable(f), Atom::Variable(s)) =>
            direct_bindings.check_and_insert_binding(f, second) &&
                reverse_bindings.check_and_insert_binding(s, first),
        (Atom::Symbol(first), Atom::Symbol(second)) => first == second,
        (Atom::Grounded(first), Atom::Grounded(second)) => first == second,
        (Atom::Expression(first), Atom::Expression(second)) =>
            first.children().len() == second.children().len() &&
            first.children().iter().zip(second.children().iter())
                .all(|(first, second)| atoms_are_equivalent_with_bindings(
                        first, second, direct_bindings, reverse_bindings)),
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_match_variables_in_data() {
        assert_eq!(
            expr!("+" a ("*" b c)).match_(&expr!("+" "A" ("*" "B" "C"))).collect::<Vec<MatchResult>>(),
            vec![MatchResult::from((bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }, bind!{}))]);
    }

    #[test]
    fn test_match_different_value_for_variable_in_data() {
        assert_eq!(
            expr!("+" a ("*" a c)).match_(&expr!("+" "A" ("*" "B" "C"))).collect::<Vec<MatchResult>>(),
            vec![]);
    }

    #[test]
    fn test_bindings_merge_different_values() {
        assert_eq!(Bindings::merge(&bind!{ a: expr!("A") },
            &bind!{ a: expr!("C"), b: expr!("B") }), None);
        assert_eq!(Bindings::merge(&bind!{ a: expr!("C"), b: expr!("B") },
            &bind!{ a: expr!("A") }), None);
    }

    #[test]
    fn test_bindings_merge() {
        assert_eq!(Bindings::merge(&bind!{ a: expr!("A") },
            &bind!{ a: expr!("A"), b: expr!("B") }),
            Some(bind!{ a: expr!("A"), b: expr!("B") }));
        assert_eq!(Bindings::merge(&bind!{ a: expr!("A"), b: expr!("B") },
            &bind!{ a: expr!("A") }),
            Some(bind!{ a: expr!("A"), b: expr!("B") }));
    }

    #[test]
    fn test_variable_name_conflict() {
        assert_eq!(expr!("a" (W)).match_(&expr!("a" W)).collect::<Vec<MatchResult>>(),
            vec![MatchResult::from((bind!{}, bind!{ W: expr!((W)) }))]);
    }

    #[test]
    fn test_are_equivalent() {
        assert!(atoms_are_equivalent(&expr!(a "b" {"c"}), &expr!(x "b" {"c"})));
        assert!(atoms_are_equivalent(&expr!(a b), &expr!(c d)));
        assert!(!atoms_are_equivalent(&expr!(a "b" {"c"}), &expr!(a "x" {"c"})));
        assert!(!atoms_are_equivalent(&expr!(a "b" {"c"}), &expr!(a "b" {"x"})));
        assert!(!atoms_are_equivalent(&expr!(a a), &expr!(c d)));
        assert!(!atoms_are_equivalent(&expr!(a b), &expr!(b b)));
    }
}
