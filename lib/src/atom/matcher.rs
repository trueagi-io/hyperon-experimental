// Macros to simplify bindings writing

#[macro_export]
macro_rules! bind {
    ($($k:ident: $v:expr),*) => {
        Bindings::from( vec![$( (VariableAtom::new(stringify!($k)), $v), )*])
    };
}

use super::*;

use std::collections::{HashMap, BTreeMap};
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
            log::trace!("Bindings::merge: {} ^ {} -> None", prev, next);
            None
        } else {
            let mut res = Bindings::new();
            prev.iter().for_each(|(k, v)| { res.insert(k.clone(), v.clone()); });
            next.iter().filter(|(k, _)| !prev.0.contains_key(k))
                .for_each(|(k, v)| { res.insert(k.clone(), v.clone()); });
            log::trace!("Bindings::merge: {} ^ {} -> {}", prev, next, res);
            Some(res)
        }
    }

    pub fn product(prev: &Vec<Bindings>, next: &Vec<Bindings>) -> Vec<Bindings> {
        prev.iter().flat_map(|p| -> Vec<Option<Bindings>> {
            next.iter().map(|n| Self::merge(p, n)).collect()
        }).filter(Option::is_some).map(Option::unwrap).collect()
    }

    pub fn filter<F>(&mut self, mut pred: F)
        where F: FnMut(&VariableAtom, &Atom) -> bool {
        self.0 = self.0.drain().filter(|(k, v)| pred(k, v)).collect();
    }

    fn into_variable_matcher(self) -> VariableMatcher {
        let mut var_matcher = VariableMatcher::new();
        for (k, v) in self.0 {
            var_matcher.add_var_binding(Var::Pattern(k), &v);
        }
        var_matcher
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

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Var {
    Data(VariableAtom),
    Pattern(VariableAtom),
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Data(var) => write!(f, "_{}", var),
            Self::Pattern(var) => write!(f, "{}", var),
        }
    }
}

// FIXME: rename to AtomMatcher?
#[derive(Clone)]
struct VariableMatcher {
    next_var_set: u32,
    vars: BTreeMap<Var, u32>,
    values: HashMap<u32, Atom>,
}

impl VariableMatcher {
    fn new() -> Self {
        Self{ next_var_set: 0, vars: BTreeMap::new(), values: HashMap::new() }
    }

    fn get(&self, var: &Var) -> Option<Atom> {
        self.vars.get(var).and_then(|set|
            match self.values.get(set) {
                Some(value) => self.resolve_atom_vars(value, var),
                None => panic!("get() should be called after assigning value or unique variable to each variable set"),
            })
    }

    fn resolve_atom_vars(&self, atom: &Atom, root: &Var) -> Option<Atom> {
        match atom {
            // FIXME: excessive cloning here
            Atom::Variable(var) if Var::Pattern(var.clone()) == *root => None,
            Atom::Variable(var) => {
                match self.get(&Var::Pattern(var.clone())) {
                    Some(atom) => Some(atom.clone()),
                    None => match self.get(&Var::Data(var.clone())) {
                        Some(atom) => Some(atom.clone()),
                        None => Some(atom.clone()),
                    },
                }
            }
            Atom::Expression(expr) => {
                let children = expr.children().iter()
                    .fold(Some(Vec::new()), |vec, child| {
                        match (vec, self.resolve_atom_vars(child, root)) {
                            (Some(mut vec), Some(child)) => {
                                vec.push(child);
                                Some(vec)
                            },
                            _ => None,
                        }
                    });
                if children.is_some() {
                    Some(Atom::Expression(ExpressionAtom::new(children.unwrap())))
                } else {
                    None
                }
            }
            _ => Some(atom.clone()),
        }
    }

    fn add_vars_equality(self, data_var: &VariableAtom,
            pattern_var: &VariableAtom) -> Option<Self> {
        self.add_any_vars_equality(Var::Data(data_var.clone()),
            Var::Pattern(pattern_var.clone()))
    }

    fn add_any_vars_equality(mut self, a: Var, b: Var) -> Option<Self> {
        match (self.vars.get(&a).copied(), self.vars.get(&b).copied()) {
            (Some(a_var_set), Some(b_var_set))  =>
                if a_var_set != b_var_set {
                    self.merge_var_sets(a_var_set, b_var_set)
                } else {
                    true
                }
            (Some(var_set), None) => {
                self.vars.insert(b, var_set);
                true
            },
            (None, Some(var_set)) => {
                self.vars.insert(a, var_set);
                true
            },
            (None, None) => {
                let var_set = self.get_next_var_set();
                self.vars.insert(a, var_set);
                self.vars.insert(b, var_set);
                true
            },
        }.then(|| self)
    }

    fn merge_var_sets(&mut self, a_var_set: u32, b_var_set: u32) -> bool {
        fn move_set(vars: &mut BTreeMap<Var, u32>, from: u32, to: u32) {
            vars.iter_mut().for_each(|(_var, set)| {
                if *set == from {
                    *set = to;
                }
            });
        }
        match (self.values.get(&a_var_set), self.values.get(&b_var_set)) {
            (Some(a_val), Some(b_val)) if a_val != b_val => false,
            (Some(_), None) => {
                move_set(&mut self.vars, b_var_set, a_var_set);
                true
            }
            _ => {
                move_set(&mut self.vars, a_var_set, b_var_set);
                true
            },
        }
    }

    fn get_next_var_set(&mut self) -> u32 {
        let next_var_set = self.next_var_set;
        self.next_var_set = self.next_var_set + 1;
        next_var_set
    }

    fn with_data_binding(mut self, data_var: &VariableAtom, value: &Atom) -> Option<Self> {
        self.add_var_binding(Var::Data(data_var.clone()), value).then(|| self)
    }

    fn with_pattern_binding(mut self, pattern_var: &VariableAtom, value: &Atom) -> Option<Self> {
        self.add_var_binding(Var::Pattern(pattern_var.clone()), value).then(|| self)
    }

    fn add_var_binding(&mut self, var: Var, value: &Atom) -> bool {
        let value_is_from_pattern = matches!(var, Var::Data(_));
        match self.vars.get(&var) {
            Some(var_set) =>
                match self.values.get(var_set) {
                    Some(current) => {
                        if current == value {
                            true
                        } else {
                            let sub_match = if value_is_from_pattern {
                                match_atoms_recursively(current, value)
                            } else {
                                match_atoms_recursively(value, current)
                            };
                            let sub_match: Vec<VariableMatcher> = sub_match.collect();
                            assert!(sub_match.len() <= 1, concat!(
                                    "Case when sub_match returns more than ",
                                    "one matcher because match_() is overloaded ",
                                    "inside grounded atom is not implemented yet"));
                            if sub_match.len() == 1 {
                                match VariableMatcher::merge(self, &sub_match[0]) {
                                    Some(sub_match) => { *self = sub_match; true },
                                    None => false,
                                }
                            } else {
                                false
                            }
                        }
                    },
                    None => {
                        self.values.insert(*var_set, value.clone());
                        true
                    },
                },
            None => {
                let var_set = self.get_next_var_set();
                self.vars.insert(var, var_set);
                self.values.insert(var_set, value.clone());
                true
            },
        }
    }

    fn add_var_no_value(mut self, var: Var) -> Self {
        if !self.vars.contains_key(&var) {
            let var_set = self.get_next_var_set();
            self.vars.insert(var, var_set);
        }
        self
    }

    fn merge(a: &VariableMatcher, b: &VariableMatcher) -> Option<VariableMatcher> {
        let mut var_sets: HashMap<u32, Var> = HashMap::new();
        let result = b.vars.iter().fold(Some(a.clone()),
            |result, (var, set)| match result {
                Some(mut result) => {
                    if let Some(first_var) = var_sets.get(&set) {
                        result.add_any_vars_equality(first_var.clone(), var.clone())
                    } else {
                        var_sets.insert(*set, var.clone());
                        if let Some(value) = b.values.get(set) {
                            result.add_var_binding(var.clone(), value).then(|| result)
                        } else {
                            Some(result.add_var_no_value(var.clone()))
                        }
                    }
                },
                None => None,
            });
        log::trace!("VariableMatcher::merge: {} ^ {} -> {:?}", a, b, result);
        result
    }

    fn vars_by_set(&self) -> HashMap<&u32, Vec<&Var>> {
        let mut var_sets: HashMap<&u32, Vec<&Var>> = HashMap::new();
        self.vars.iter().for_each(|(var, set)| {
            match var_sets.get_mut(set) {
                Some(vec) => vec.push(var),
                None => { var_sets.insert(set, vec![var]); },
            };
        });
        var_sets
    }

    fn into_bindings(mut self) -> Option<Bindings> {
        let mut bindings = Bindings::new();
        for (_var, set) in &self.vars {
            match self.values.get(set) {
                Some(_value) => {},
                None => {
                    let var = VariableAtom::new(format!("u{}", set)).make_unique();
                    self.values.insert(*set, Atom::Variable(var));
                },
            }
        }
        for var in self.vars.keys().filter(|var| matches!(var, Var::Pattern(_))) {
            match (var, self.get(var)) {
                (Var::Pattern(var), Some(value)) => {
                    bindings.insert(var.clone(), value.clone());
                },
                (Var::Pattern(_var), None) => return None,
                _ => {},
            }
        }
        Some(bindings)
    }
}

impl Display for VariableMatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vars_by_set = self.vars_by_set();
        // FIXME: rewrite in procedural style
        write!(f, "{{")
            .and_then(|_| vars_by_set.iter().take(1).fold(Ok(()),
                |res, (set, vars)| vars.iter().fold(res,
                    |res, var| res.and_then(|_| write!(f, "{} = ", var)))
                .and_then(|_| write!(f, "{}", self.values.get(set)
                        .map_or(&sym!("?"), std::convert::identity)))))
            .and_then(|_| vars_by_set.iter().skip(1).fold(Ok(()),
                |res, (set, vars)| write!(f, ", ")
                .and_then(|_| vars.iter().fold(res,
                        |res, var| res.and_then(|_| write!(f, "{} = ", var)))
                    .and_then(|_| write!(f, "{}", self.values.get(set)
                            .map_or(&sym!("?"), std::convert::identity))))))
            .and_then(|_| write!(f, "}}"))
    }
}

impl Debug for VariableMatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub type MatchResultIter = Box<dyn Iterator<Item=matcher::Bindings>>;

pub trait WithMatch {
    fn match_(&self, pattern: &Atom) -> MatchResultIter;
}

impl WithMatch for Atom {
    fn match_(&self, pattern: &Atom) -> MatchResultIter {
        Box::new(match_atoms_recursively(self, pattern)
            .map(VariableMatcher::into_bindings)
            .filter(Option::is_some).map(Option::unwrap))
    }
}

type VarMatchIter = Box<dyn Iterator<Item=VariableMatcher>>;

fn match_atoms_recursively(data: &Atom, pattern: &Atom) -> VarMatchIter {
    log::trace!("match_atoms_recursively: {} ~ {}", data, pattern);
    fn empty() -> VarMatchIter { Box::new(std::iter::empty()) }
    fn once(b: VariableMatcher) -> VarMatchIter { Box::new(std::iter::once(b)) }

    match (data, pattern) {
        (Atom::Symbol(a), Atom::Symbol(b)) if a == b => once(VariableMatcher::new()),
        (Atom::Grounded(a), Atom::Grounded(_)) => Box::new(a.match_(pattern).map(Bindings::into_variable_matcher)),
        (Atom::Variable(dv), Atom::Variable(pv)) => {
            VariableMatcher::new().add_vars_equality(dv, pv).map_or(empty(), once)
        }
        (Atom::Variable(v), b) => {
            VariableMatcher::new().with_data_binding(v, b).map_or(empty(), once)
        }
        (a, Atom::Variable(v)) => {
            VariableMatcher::new().with_pattern_binding(v, a).map_or(empty(), once)
        },
        (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b }))
                if a.len() == b.len() => {
            // FIXME: how to log it properly?
            a.iter().zip(b.iter()).fold(once(VariableMatcher::new()),
                |acc, (a, b)| {
                    variable_matcher_product(acc, match_atoms_recursively(a, b))
                })
        },
        _ => empty(),
    }
}

fn variable_matcher_product(prev: VarMatchIter, next: VarMatchIter) -> VarMatchIter {
    let next : Vec<VariableMatcher> = next.collect();
    Box::new(prev.flat_map(move |p| -> Vec<Option<VariableMatcher>> {
        next.iter().map(|n| VariableMatcher::merge(&p, n)).collect()
    }).filter(Option::is_some).map(Option::unwrap))
}

pub fn match_result_product(prev: MatchResultIter, next: MatchResultIter) -> MatchResultIter {
    let next : Vec<Bindings> = next.collect();
    log::trace!("match_result_product_iter, next: {:?}", next);
    Box::new(prev.flat_map(move |p| -> Vec<Option<Bindings>> {
        next.iter().map(|n| Bindings::merge(&p, n)).collect()
    }).filter(Option::is_some).map(Option::unwrap))
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnificationPair {
    pub data: Atom,
    pub pattern: Atom,
}

impl From<(Atom, Atom)> for UnificationPair {
    fn from((data, pattern): (Atom, Atom)) -> Self {
        Self { data, pattern }
    }
}

pub type Unifications = Vec<matcher::UnificationPair>;

#[derive(Debug, PartialEq, Eq)]
pub struct UnifyResult {
    pub data_bindings: Bindings,
    pub pattern_bindings: Bindings,
    pub unifications: Unifications,
}

impl UnifyResult {
    fn new() -> Self {
        UnifyResult {
            data_bindings: Bindings::new(),
            pattern_bindings: Bindings::new(),
            unifications: Vec::new(),
        }
    }
}

fn unify_atoms_recursively(data: &Atom, pattern: &Atom, res: &mut UnifyResult, depth: u32) -> bool {
    match (data, pattern) {
        (Atom::Symbol(a), Atom::Symbol(b)) => a == b,
        (Atom::Grounded(a), Atom::Grounded(b)) => a == b,
        (Atom::Variable(_), Atom::Variable(v)) => {
            // We stick to prioritize pattern bindings in this case
            // because otherwise the $X in (= (...) $X) will not be matched with
            // (= (if True $then) $then)
            log::trace!("check_and_insert_binding for pattern({:?}, {}, {})", res.pattern_bindings, v, data);
            res.pattern_bindings.check_and_insert_binding(v, data)
        }
        (Atom::Variable(v), b) => {
            log::trace!("check_and_insert_binding for data({:?}, {}, {})", res.data_bindings, v, b);
            res.data_bindings.check_and_insert_binding(v, b)
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
                    res.unifications.push((data.clone(), pattern.clone()).into());
                    true
                }
            } else {
                a.iter().zip(b.iter()).fold(true,
                    |succ, pair| succ && unify_atoms_recursively(pair.0, pair.1, res, depth + 1))
            }
        },
        (Atom::Expression(_), _) | (_, Atom::Expression(_)) => {
            res.unifications.push((data.clone(), pattern.clone()).into());
            true
        }
        _ => false,
    }
}

pub fn unify_atoms(data: &Atom, pattern: &Atom) -> Option<UnifyResult> {
    log::trace!("unify_atoms: data: {}, pattern: {}", data, pattern);
    let mut res = UnifyResult::new();
    if unify_atoms_recursively(data, pattern, &mut res, 0) {
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
        log::trace!("apply_bindings_to_atom: {} | {} -> {}", atom, bindings, result);
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
    // FIXME: try removing this method
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
    log::trace!("apply_bindings_to_bindings: {} | {} -> {}", to, from, res);
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

    fn assert_match(data: Atom, pattern: Atom, expected: Vec<Bindings>) {
        let actual: Vec<Bindings> = data.match_(&pattern).collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn match_variables_in_data() {
        assert_match(
            expr!("+"  a  ("*"  b   c )),
            expr!("+" "A" ("*" "B" "C")),
            vec![bind!{}]);
    }

    #[test]
    fn match_value_conflict_for_variable_in_data() {
        assert_match(
            expr!("+"  a  ("*"  a   c )),
            expr!("+" "A" ("*" "B" "C")),
            vec![]);
    }

    #[test]
    fn bindings_merge_value_conflict() {
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
    fn match_variable_name_conflict() {
        assert_match(expr!("a" (W)), expr!("a" W), vec![]);
    }

    #[test]
    fn test_atoms_are_equivalent() {
        assert!(atoms_are_equivalent(&expr!(a "b" {"c"}), &expr!(x "b" {"c"})));
        assert!(atoms_are_equivalent(&expr!(a b), &expr!(c d)));
        assert!(!atoms_are_equivalent(&expr!(a "b" {"c"}), &expr!(a "x" {"c"})));
        assert!(!atoms_are_equivalent(&expr!(a "b" {"c"}), &expr!(a "b" {"x"})));
        assert!(!atoms_are_equivalent(&expr!(a a), &expr!(c d)));
        assert!(!atoms_are_equivalent(&expr!(a b), &expr!(b b)));
    }

    #[test]
    fn match_spread_value_via_data_variable() {
        assert_match(
            expr!( a  a a),
            expr!("v" x y),
            vec![bind!{x: sym!("v"), y: sym!("v")}]);
    }

    #[test]
    fn match_spread_value_via_data_variable_reverse_order() {
        assert_match(
            expr!(a a  a  a),
            expr!(x x "v" y),
            vec![bind!{x: sym!("v"), y: sym!("v")}]);
    }

    #[test]
    fn match_spread_value_via_pattern_variable() {
        assert_match(
            expr!("v" a a),
            expr!( x  x y),
            vec![bind!{x: sym!("v"), y: sym!("v")}]);
    }

    #[test]
    fn match_spread_value_via_pattern_variable_reverse_order() {
        assert_match(
            expr!(a "v" a),
            expr!(x  x  y),
            vec![bind!{x: sym!("v"), y: sym!("v")}]);
    }

    #[ignore]
    #[test]
    fn match_replace_variable_via_data_variable() {
        assert_match(
            expr!(a a),
            expr!(x y),
            vec![bind!{x: expr!(u0), y: expr!(u0)}]);
    }

    #[test]
    fn match_variable_via_itself() {
        assert_match(
            expr!(a  a ),
            expr!(x (x)),
            vec![]);
    }

    #[ignore]
    #[test]
    fn match_variable_with_unique_itself() {
        assert_match(
            replace_variables(&expr!(("A" x) ("B" x))),
                               expr!(("A" x)    z   ),
            vec![bind!{x: expr!(u0), z: expr!("B" u0)}]);
    }

    #[test]
    fn match_stub_variable_is_unique() {
        let data =    expr!(a a);
        let pattern = expr!(x y);
        let x = VariableAtom::new("x");

        let bindings_a = data.match_(&pattern).next().unwrap();
        let bindings_b = data.match_(&pattern).next().unwrap();

        assert_ne!(bindings_a.get(&x), bindings_b.get(&x));
    }

    #[ignore]
    #[test]
    fn match_equality_of_pattern_variables_inside_expression() {
        assert_match(
            expr!( a    a   a),
            expr!((x) ("v") y),
            vec![bind!{x: expr!("v"), y: expr!(("v"))}]);
    }

    #[ignore]
    #[test]
    fn match_equality_of_data_variables_inside_expression() {
        assert_match(
            expr!((a) ("v") a),
            expr!( x    x   y),
            vec![bind!{x: expr!(("v")), y: expr!("v")}]);
    }
}
