// Macros to simplify bindings writing

#[macro_export]
macro_rules! bind {
    ($($k:ident: $v:expr),*) => {
        $crate::atom::matcher::Bindings::from( vec![$( ($crate::VariableAtom::new(stringify!($k)), $v), )*])
    };
}

use super::*;

use std::collections::{HashMap, HashSet};
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

    fn into_variable_match(self) -> VariableMatch {
        let mut var_match = VariableMatch::new();
        for (k, v) in self.0 {
            var_match.add_pattern_var(&k);
            var_match.add_var_binding(&k, &v);
        }
        var_match
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

#[derive(Clone)]
struct VariableMatch {
    next_var_set: u32,
    vars: HashMap<VariableAtom, u32>,
    values: HashMap<u32, Atom>,
    pattern_vars: HashSet<VariableAtom>,
}

impl VariableMatch {
    fn new() -> Self {
        Self {
            next_var_set: 0,
            vars: HashMap::new(),
            values: HashMap::new(),
            pattern_vars: HashSet::new(),
        }
    }

    fn get(&self, var: &VariableAtom) -> Option<Atom> {
        self.vars.get(var).and_then(|set|
            match self.values.get(set) {
                Some(value) => self.resolve_atom_vars(value, var),
                None => panic!("get() should be called after assigning value or unique variable to each variable set"),
            })
    }

    fn resolve_atom_vars(&self, atom: &Atom, root: &VariableAtom) -> Option<Atom> {
        match atom {
            Atom::Variable(var) if var == root => None, // loop detected
            Atom::Variable(var) => {
                match self.get(var) {
                    Some(atom) => Some(atom.clone()),
                    None => Some(atom.clone()),
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

    fn add_pattern_var(&mut self, var: &VariableAtom) {
        self.pattern_vars.insert(var.clone());
    }

    fn with_var_equality(mut self, a: &VariableAtom, b: &VariableAtom) -> Option<Self> {
        self.add_var_equality(a, b).then(|| self)
    }

    fn add_var_equality(&mut self, a: &VariableAtom, b: &VariableAtom) -> bool {
        match (self.vars.get(a).copied(), self.vars.get(b).copied()) {
            (Some(a_var_set), Some(b_var_set))  =>
                if a_var_set != b_var_set {
                    self.merge_var_sets(a_var_set, b_var_set)
                } else {
                    true
                }
            (Some(var_set), None) => {
                self.vars.insert(b.clone(), var_set);
                true
            },
            (None, Some(var_set)) => {
                self.vars.insert(a.clone(), var_set);
                true
            },
            (None, None) => {
                let var_set = self.get_next_var_set();
                self.vars.insert(a.clone(), var_set);
                self.vars.insert(b.clone(), var_set);
                true
            },
        }
    }

    fn match_values(&self, current: &Atom, value: &Atom) -> Option<VariableMatch> {
        let sub_match: Vec<VariableMatch> =
            match_atoms_recursively(current, value).collect();
        assert!(sub_match.len() <= 1, concat!(
                "Case when sub_match returns more than ",
                "one matcher because match_() is overloaded ",
                "inside grounded atom is not implemented yet"));
        if sub_match.len() == 1 {
            VariableMatch::merge(self, &sub_match[0])
        } else {
            None
        }
    }

    fn merge_var_sets(&mut self, a_var_set: u32, b_var_set: u32) -> bool {
        fn move_set(vars: &mut HashMap<VariableAtom, u32>, from: u32, to: u32) {
            vars.iter_mut().for_each(|(_var, set)| {
                if *set == from {
                    *set = to;
                }
            });
        }
        match (self.values.get(&a_var_set), self.values.get(&b_var_set)) {
            (Some(a_val), Some(b_val)) => {
                match self.match_values(a_val, b_val) {
                    Some(result) => { *self = result; true },
                    None => false,
                }
            },
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

    fn with_var_binding(mut self, var: &VariableAtom, value: &Atom) -> Option<Self> {
        self.add_var_binding(var, value).then(|| self)
    }

    fn add_var_binding(&mut self, var: &VariableAtom, value: &Atom) -> bool {
        match self.vars.get(var) {
            Some(var_set) =>
                match self.values.get(var_set) {
                    Some(current) => {
                        match self.match_values(current, value) {
                            Some(result) => { *self = result; true },
                            None => false,
                        }
                    },
                    None => {
                        self.values.insert(*var_set, value.clone());
                        true
                    },
                },
            None => {
                let var_set = self.get_next_var_set();
                self.vars.insert(var.clone(), var_set);
                self.values.insert(var_set, value.clone());
                true
            },
        }
    }

    fn add_var_no_value(mut self, var: VariableAtom) -> Self {
        if !self.vars.contains_key(&var) {
            let var_set = self.get_next_var_set();
            self.vars.insert(var, var_set);
        }
        self
    }

    fn merge(a: &VariableMatch, b: &VariableMatch) -> Option<VariableMatch> {
        let mut var_sets: HashMap<u32, VariableAtom> = HashMap::new();
        let mut result = b.vars.iter().fold(Some(a.clone()),
            |result, (var, set)| match result {
                Some(result) => {
                    if let Some(first_var) = var_sets.get(&set) {
                        result.with_var_equality(first_var, var)
                    } else {
                        var_sets.insert(*set, var.clone());
                        if let Some(value) = b.values.get(set) {
                            result.with_var_binding(var, value)
                        } else {
                            Some(result.add_var_no_value(var.clone()))
                        }
                    }
                },
                None => None,
            });
        result.iter_mut().for_each(|result| {
            for var in &b.pattern_vars {
                result.pattern_vars.insert(var.clone());
            }
        });
        log::trace!("VariableMatch::merge: {} ^ {} -> {:?}", a, b, result);
        result
    }

    fn vars_by_set(&self) -> HashMap<&u32, Vec<&VariableAtom>> {
        let mut var_sets: HashMap<&u32, Vec<&VariableAtom>> = HashMap::new();
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
        for var in self.vars.keys().filter(|var| self.pattern_vars.contains(var)) {
            match self.get(var) {
                Some(value) => {
                    bindings.insert(var.clone(), value.clone());
                },
                None => return None,
            }
        }
        Some(bindings)
    }
}

impl Display for VariableMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vars_by_set = self.vars_by_set();
        write!(f, "{{ ")?;
        for (i, (set, vars)) in vars_by_set.iter().enumerate() {
            let prefix = if i == 0 { "" } else { ", " };
            write!(f, "{}", prefix)?;
            for (i, var) in vars.iter().enumerate() {
                let prefix = if i == 0 { "" } else { " = " };
                write!(f, "{}{}", prefix, var)?;
            }
            match self.values.get(set) {
                Some(value) => write!(f, " = {}", value)?,
                None => {},
            }
        }
        write!(f, " }}")
    }
}

impl Debug for VariableMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub type MatchResultIter = Box<dyn Iterator<Item=matcher::Bindings>>;

pub trait WithMatch {
    fn match_(&self, pattern: &Atom) -> MatchResultIter;
}

impl WithMatch for Atom {
    fn match_<'a>(&'a self, pattern: &'a Atom) -> MatchResultIter {
        fn find_vars(atom: &Atom, vars: &mut HashSet<VariableAtom>) {
            match atom {
                Atom::Variable(var) => { vars.insert(var.clone()); },
                Atom::Expression(expr) => expr.children.iter()
                    .for_each(|child| find_vars(child, vars)),
                _ => {},
            }
        }
        let mut pattern_vars = HashSet::new();
        find_vars(pattern, &mut pattern_vars);
        Box::new(match_atoms_recursively(self, pattern)
            .map(move |mut matcher| {
                for x in &pattern_vars {
                    matcher.add_pattern_var(&x);
                }
                matcher.into_bindings()
            })
            .filter(Option::is_some).map(Option::unwrap))
    }
}

type VarMatchIter = Box<dyn Iterator<Item=VariableMatch>>;

fn match_atoms_recursively(data: &Atom, pattern: &Atom) -> VarMatchIter {
    log::trace!("match_atoms_recursively: {} ~ {}", data, pattern);
    fn empty() -> VarMatchIter { Box::new(std::iter::empty()) }
    fn once(b: VariableMatch) -> VarMatchIter { Box::new(std::iter::once(b)) }

    match (data, pattern) {
        (Atom::Symbol(a), Atom::Symbol(b)) if a == b => once(VariableMatch::new()),
        (Atom::Grounded(a), Atom::Grounded(_)) => {
            Box::new(a.match_(pattern).map(Bindings::into_variable_match))
        },
        (Atom::Variable(dv), Atom::Variable(pv)) => {
            VariableMatch::new().with_var_equality(dv, pv).map_or(empty(), once)
        }
        (Atom::Variable(v), b) => {
            VariableMatch::new().with_var_binding(v, b).map_or(empty(), once)
        }
        (a, Atom::Variable(v)) => {
            VariableMatch::new().with_var_binding(v, a).map_or(empty(), once)
        },
        (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b }))
                if a.len() == b.len() => {
            a.iter().zip(b.iter()).fold(once(VariableMatch::new()),
                |acc, (a, b)| {
                    variable_matcher_product(acc, match_atoms_recursively(a, b))
                })
        },
        _ => empty(),
    }
}

fn variable_matcher_product(prev: VarMatchIter, next: VarMatchIter) -> VarMatchIter {
    let next : Vec<VariableMatch> = next.collect();
    Box::new(prev.flat_map(move |p| -> Vec<Option<VariableMatch>> {
        next.iter().map(|n| VariableMatch::merge(&p, n)).collect()
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

    fn binding_eq(actual: &Bindings, expected: &Bindings) -> bool {
        let actual_keys: HashSet<&VariableAtom> = actual.0.keys().collect();
        let expected_keys: HashSet<&VariableAtom> = expected.0.keys().collect();
        if actual_keys != expected_keys {
            return false;
        } 
        let mut direct_bindings = Bindings::new();
        let mut reverse_bindings = Bindings::new();
        for (k, v) in actual {
            if !atoms_are_equivalent_with_bindings(v, expected.get(k).unwrap(),
                    &mut direct_bindings, &mut reverse_bindings) {
                return false;
            }
        }
        true
    }

    fn assert_match(data: Atom, pattern: Atom, expected: Vec<Bindings>) {
        let actual: Vec<Bindings> = data.match_(&pattern).collect();
        assert_eq!(actual.len(), expected.len(), "Actual and expected has different number of results:\n  actual: {:?}\nexpected: {:?}", actual, expected);
        for (actual, expected) in actual.iter().zip(expected.iter()) {
            if !binding_eq(actual, expected) {
                assert!(false, "Bindings are different:\n  actual: {}\nexpected: {}", actual, expected);
            }
        }
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

    #[test]
    fn match_variable_with_unique_itself() {
        assert_match(
            make_variables_unique(&expr!(("A" x) ("B" x))),
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

    #[test]
    fn match_equality_of_pattern_variables_inside_expression() {
        assert_match(
            expr!( a    a   a),
            expr!((x) ("v") y),
            vec![bind!{x: expr!("v"), y: expr!(("v"))}]);
    }

    #[test]
    fn match_equality_of_data_variables_inside_expression() {
        assert_match(
            expr!((a) ("v") a),
            expr!( x    x   y),
            vec![bind!{x: expr!(("v")), y: expr!("v")}]);
    }

    #[test]
    fn match_match_values_when_merging_two_variable_sets() {
        assert_match(
            expr!((a)  b   b a),
            expr!( x ("v") x y),
            vec![bind!{x: expr!(("v")), y: expr!("v")}]);
    }

    #[derive(PartialEq, Clone, Debug)]
    struct TestDict(Vec<(Atom, Atom)>);

    impl TestDict {
        fn new() -> Self {
            TestDict(Vec::new())
        }
        fn get(&self, key: &Atom) -> Option<&Atom> {
            self.0.iter().filter(|(k, _)| { k == key }).nth(0).map(|(_, v)| { v })
        }
        fn remove(&mut self, key: &Atom) -> Option<Atom> {
            let v = self.get(key).map(Atom::clone);
            self.0 = self.0.drain(..).filter(|(k, _)| { k != key }).collect();
            v
        }
        fn put(&mut self, key: Atom, value: Atom) -> Option<Atom> {
            let v = self.remove(&key);
            self.0.push((key, value));
            v
        }
    }

    impl Grounded for TestDict {
        fn type_(&self) -> Atom {
            Atom::sym("Dict")
        }
        fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
            execute_not_executable(self)
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            if let Some(other) = other.as_gnd::<TestDict>() {
                other.0.iter().map(|(ko, vo)| {
                    self.0.iter().map(|(k, v)| {
                        Atom::expr(vec![k.clone(), v.clone()]).match_(&Atom::expr(vec![ko.clone(), vo.clone()]))
                    }).fold(Box::new(std::iter::empty()) as MatchResultIter, |acc, i| {
                        Box::new(acc.chain(i))
                    })
                }).fold(Box::new(std::iter::once(Bindings::new())),
                    |acc, i| { matcher::match_result_product(acc, i) })
            } else {
                Box::new(std::iter::empty())
            }
        }
    }
    
    impl Display for TestDict {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{{ ").and_then(|_| self.0.iter().fold(Ok(()),
                |ret, (key, val)| ret.and_then(
                    |_| write!(f, "{}: {}, ", key, val))))
                .and_then(|_| write!(f, "}}"))
        }
    }

    #[test]
    fn match_atoms_with_custom_matcher_implementation() {
        let mut dict = TestDict::new();
        dict.put(expr!("x"), expr!({2} {5}));
        dict.put(expr!("y"), expr!({5}));
        let dict = expr!({dict}); 

        let mut query = TestDict::new();
        query.put(expr!(b), expr!(y));
        query.put(expr!(a), expr!({2} y));
        let query = expr!({query});

        let result: Vec<Bindings> = dict.match_(&query).collect();
        assert_eq!(result, vec![bind!{y: expr!({5}), b: expr!("y"), a: expr!("x")}]);
    }

    #[ignore = "Requires sorting inside VariableMatch to be stable"]
    #[test]
    fn variable_match_display() {
        let mut var_match = VariableMatch::new();
        var_match.add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("b"));
        var_match.add_var_binding(&VariableAtom::new("b"), &Atom::sym("v"));
        var_match.add_var_equality(&VariableAtom::new("c"), &VariableAtom::new("d"));
        
        assert_eq!(var_match.to_string(), "{ $a = $b = v, $c = $d }");
    }
}
