//! Module contains functions to match atoms and work with variable bindings.

/// Constructs new instance of the [Bindings] with predefined content.
/// Macros takes variable/value pairs as arguments. If value is a single
/// variable then the pair means variable equality. Otherwise pair means
/// assigning value. May be ineffective, should be used mainly in unit tests.
///
/// # Examples
///
/// ```
/// use hyperon::*;
///
/// let bindings = bind!{ a: expr!("A"), b: expr!("foo" "B"), c: expr!(a) };
///
/// assert_eq!(bindings.resolve(&VariableAtom::new("a")), Some(expr!("A")));
/// assert_eq!(bindings.resolve(&VariableAtom::new("b")), Some(expr!("foo" "B")));
/// assert_eq!(bindings.resolve(&VariableAtom::new("c")), Some(expr!("A")));
/// ```
#[macro_export]
macro_rules! bind {
    ($($k:ident: $v:expr),*) => {
        $crate::atom::matcher::Bindings::from( vec![$( ($crate::VariableAtom::new(stringify!($k)), $v), )*])
    };
}

use super::*;

use std::collections::{HashMap, HashSet};
use crate::common::reformove::RefOrMove;

enum VarResolutionResult<T> {
    Some(T),
    Loop,
    None
}

/// Represents variable bindings. Keeps two kinds of relations inside:
/// variables equalities and variable value assignments. For example this
/// structure is able to precisely represent result of matching atoms like
/// `($a A C)` and `($x $x $y)`. The result is `{ $a = $x = A, $y = C }`.
/// [Bindings] contains variables from both sides of the match.
#[derive(Clone)]
pub struct Bindings {
    next_var_id: u32,
    id_by_var: HashMap<VariableAtom, u32>,
    value_by_id: HashMap<u32, Atom>,
}

impl Bindings {
    /// Constructs new empty instance of [Bindings].
    pub fn new() -> Self {
        Self {
            next_var_id: 0,
            id_by_var: HashMap::new(),
            value_by_id: HashMap::new(),
        }
    }

    /// Returns true if bindings doesn't contain any variable.
    pub fn is_empty(&self) -> bool {
        self.id_by_var.is_empty()
    }

    fn get_value(&self, var: &VariableAtom) -> Option<&Atom> {
        self.id_by_var.get(var).and_then(|id| self.value_by_id.get(id))
    }

    /// Returns value of the variable with all sub-variables resolved using the
    /// same binding. Returns `None` if variable doesn't have a value assigned
    /// or cannot be resolved because of the variable loop.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::*;
    ///
    /// let norm_bind = bind!{ a: expr!(("foo" b)), b: expr!("bar") };
    /// let loop_bind = bind!{ a: expr!(("foo" b)), b: expr!(("bar" a)) };
    /// let none_bind = bind!{ a: expr!(("foo" b)) };
    ///
    /// assert_eq!(norm_bind.resolve(&VariableAtom::new("a")), Some(expr!(("foo" "bar"))));
    /// assert_eq!(loop_bind.resolve(&VariableAtom::new("a")), None);
    /// assert_eq!(none_bind.resolve(&VariableAtom::new("b")), None);
    /// ```
    pub fn resolve(&self, var: &VariableAtom) -> Option<Atom> {
        let mut used_vars = HashSet::new();
        used_vars.insert(var);
        match self.resolve_internal(var, &used_vars) {
            VarResolutionResult::Some(atom) => Some(atom),
            VarResolutionResult::Loop => None,
            VarResolutionResult::None => None,
        }
    }

    fn var_by_id<F>(&self, var_id: u32, condition: F) -> Option<&VariableAtom>
        where F: Fn(&VariableAtom) -> bool
    {
        self.id_by_var.iter()
            .filter(|(var, &id)| id == var_id && condition(var))
            .map(|(var, _)| var).next()
    }

    fn resolve_internal(&self, var: &VariableAtom, used_vars: &HashSet<&VariableAtom>) -> VarResolutionResult<Atom> {
        let resolve_value_by_id = |&var_id|
            match self.value_by_id.get(&var_id) {
                Some(value) => self.resolve_vars_in_atom(value, used_vars),
                None => {
                    let replacing_var = self.var_by_id(var_id, |alt| *alt != *var);
                    match  replacing_var {
                        Some(var) => VarResolutionResult::Some(Atom::Variable(var.clone())),
                        None => VarResolutionResult::None,
                    }
                },
            };
        self.id_by_var.get(var).map_or(VarResolutionResult::None, resolve_value_by_id)
    }

    fn resolve_vars_in_atom(&self, atom: &Atom, used_vars: &HashSet<&VariableAtom>) -> VarResolutionResult<Atom> {
        match atom {
            Atom::Variable(var) if used_vars.contains(var) => VarResolutionResult::Loop,
            Atom::Variable(var) => {
                let mut used_vars = used_vars.clone();
                used_vars.insert(var);
                match self.resolve_internal(var, &used_vars) {
                    VarResolutionResult::Some(atom) => VarResolutionResult::Some(atom),
                    VarResolutionResult::Loop => VarResolutionResult::Loop,
                    VarResolutionResult::None => VarResolutionResult::Some(atom.clone()),
                }
            }
            Atom::Expression(expr) => {
                let children = expr.children().iter()
                    .fold(VarResolutionResult::Some(Vec::new()), |vec, child| {
                        match (vec, self.resolve_vars_in_atom(child, used_vars)) {
                            (VarResolutionResult::Some(mut vec), VarResolutionResult::Some(child)) => {
                                vec.push(child);
                                VarResolutionResult::Some(vec)
                            },
                            (VarResolutionResult::Loop, _) => VarResolutionResult::Loop,
                            (_, VarResolutionResult::Loop) => VarResolutionResult::Loop,
                            _ => VarResolutionResult::None,
                        }
                    });
                match children {
                    VarResolutionResult::Some(vec) => VarResolutionResult::Some(Atom::expr(vec)),
                    VarResolutionResult::Loop => VarResolutionResult::Loop,
                    VarResolutionResult::None => VarResolutionResult::None,
                }
            }
            _ => VarResolutionResult::Some(atom.clone()),
        }
    }

    // TODO: This method should return Vec<Bindings>; additional bindings can
    // be constructed by Bindings::match_values() call inside
    // Bindings::merge_var_ids() function. It is the reason why this function
    // is not public yet.
    fn with_var_equality(mut self, a: &VariableAtom, b: &VariableAtom) -> Option<Self> {
        self.add_var_equality(a, b).then(|| self)
    }

    // TODO: See comment to Bindings::with_var_equality() function.
    // Returning `bool` from this function is not enough.
    fn add_var_equality(&mut self, a: &VariableAtom, b: &VariableAtom) -> bool {
        match (self.id_by_var.get(a).copied(), self.id_by_var.get(b).copied()) {
            (Some(a_var_id), Some(b_var_id))  =>
                if a_var_id != b_var_id {
                    self.merge_var_ids(a_var_id, b_var_id)
                } else {
                    true
                }
            (Some(var_id), None) => {
                self.id_by_var.insert(b.clone(), var_id);
                true
            },
            (None, Some(var_id)) => {
                self.id_by_var.insert(a.clone(), var_id);
                true
            },
            (None, None) => {
                let var_id = self.get_next_var_id();
                self.id_by_var.insert(a.clone(), var_id);
                self.id_by_var.insert(b.clone(), var_id);
                true
            },
        }
    }

    // TODO: see assert! inside: Bindings::match_values() can return more than
    // one result, so it should return Vec<Bindings> instead. It also affects
    // Bindings::insert() function.
    fn match_values(&self, current: &Atom, value: &Atom) -> Option<Bindings> {
        let sub_match: Vec<Bindings> =
            match_atoms_recursively(current, value).collect();
        assert!(sub_match.len() <= 1, concat!(
                "Case when sub_match returns more than ",
                "one bindings because match_() is overloaded ",
                "inside grounded atom is not implemented yet"));
        if sub_match.len() == 1 {
            Bindings::merge(self, &sub_match[0])
        } else {
            None
        }
    }

    // TODO: should return Vec<Bindings> because of Bindings::match_values()
    fn merge_var_ids(&mut self, a_var_id: u32, b_var_id: u32) -> bool {
        fn replace_id(id_by_var: &mut HashMap<VariableAtom, u32>, to_replace: u32, replace_by: u32) {
            id_by_var.iter_mut().for_each(|(_var, id)| {
                if *id == to_replace {
                    *id = replace_by;
                }
            });
        }
        match (self.value_by_id.get(&a_var_id), self.value_by_id.get(&b_var_id)) {
            (Some(a_val), Some(b_val)) => {
                match self.match_values(a_val, b_val) {
                    Some(result) => { *self = result; true },
                    None => false,
                }
            },
            (Some(_), None) => {
                replace_id(&mut self.id_by_var, b_var_id, a_var_id);
                true
            }
            _ => {
                replace_id(&mut self.id_by_var, a_var_id, b_var_id);
                true
            },
        }
    }

    fn get_next_var_id(&mut self) -> u32 {
        let next_var_id = self.next_var_id;
        self.next_var_id = self.next_var_id + 1;
        next_var_id
    }

    // TODO: function should return Vec<Bindings>, because
    // Bindings::match_values() can return it.
    fn with_var_binding<T1: RefOrMove<VariableAtom>, T2: RefOrMove<Atom>>(mut self, var: T1, value: T2) -> Option<Self> {
        self.add_var_binding(var, value).then(|| self)
    }

    /// Tries to insert `value` as a binding for the `var`. If `self` already
    /// has binding for the `var` and it is not matchable with the `value` then
    /// function returns `false`. Otherwise it inserts binding and returns `true`.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::*;
    /// use hyperon::matcher::Bindings;
    ///
    /// let a = VariableAtom::new("a");
    /// let b = VariableAtom::new("b");
    /// let c = VariableAtom::new("c");
    /// let mut binds = bind!{ a: expr!("A"), b: expr!("B") };
    ///
    /// assert!(binds.add_var_binding(&a, &expr!("A")));
    /// assert!(!binds.add_var_binding(&b, &expr!("C")));
    /// assert!(binds.add_var_binding(&c, &expr!("C")));
    /// assert_eq!(binds.resolve(&a), Some(expr!("A")));
    /// assert_eq!(binds.resolve(&b), Some(expr!("B")));
    /// assert_eq!(binds.resolve(&c), Some(expr!("C")));
    /// ```
    // TODO: function should return Vec<Bindings>, because
    // Bindings::match_values() can return it.
    pub fn add_var_binding<'a, T1: RefOrMove<VariableAtom>, T2: RefOrMove<Atom>>(&mut self, var: T1, value: T2) -> bool {
        match self.id_by_var.get(var.as_ref()) {
            Some(var_id) =>
                match self.value_by_id.get(var_id) {
                    Some(current) => {
                        if current == value.as_ref() {
                            true
                        } else {
                            match self.match_values(current, value.as_ref()) {
                                Some(result) => { *self = result; true },
                                None => false,
                            }
                        }
                    },
                    None => {
                        self.value_by_id.insert(*var_id, value.as_value());
                        true
                    },
                },
            None => {
                let var_id = self.get_next_var_id();
                self.id_by_var.insert(var.as_value(), var_id);
                self.value_by_id.insert(var_id, value.as_value());
                true
            },
        }
    }

    fn with_var_no_value(mut self, var: &VariableAtom) -> Self {
        self.add_var_no_value(var);
        self
    }

    fn add_var_no_value(&mut self, var: &VariableAtom) {
        if !self.id_by_var.contains_key(var) {
            let var_id = self.get_next_var_id();
            self.id_by_var.insert(var.clone(), var_id);
        }
    }

    /// Merges `a` and `b` bindings if they are compatible. Returns `None`
    /// if incompatibility is found.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::*;
    /// use hyperon::matcher::Bindings;
    ///
    /// let mut binds = bind!{ a: expr!("A") };
    /// let mut comp = bind!{ b: expr!("B") };
    /// let mut incomp = bind!{ a: expr!("B") };
    ///
    /// assert_eq!(Bindings::merge(&binds, &comp), Some(bind!{ a: expr!("A"), b: expr!("B") }));
    /// assert_eq!(Bindings::merge(&binds, &incomp), None);
    /// ```
    // TODO: this method should return Vec<Bindings>
    pub fn merge(a: &Bindings, b: &Bindings) -> Option<Bindings> {
        log::trace!("Bindings::merge: a: {}, b: {}", a, b);
        let mut var_ids: HashMap<u32, VariableAtom> = HashMap::new();
        let result = b.id_by_var.iter().fold(Some(a.clone()),
            |result, (var, var_id)| match result {
                Some(result) => {
                    if let Some(first_var) = var_ids.get(&var_id) {
                        result.with_var_equality(first_var, var)
                    } else {
                        var_ids.insert(*var_id, var.clone());
                        if let Some(value) = b.value_by_id.get(var_id) {
                            result.with_var_binding(var, value)
                        } else {
                            Some(result.with_var_no_value(var))
                        }
                    }
                },
                None => None,
            });
        log::trace!("Bindings::merge: {} ^ {} -> {:?}", a, b, result);
        result
    }

    fn vars_by_id(&self) -> HashMap<&u32, Vec<&VariableAtom>> {
        let mut var_by_id = HashMap::new();
        for (var, id) in &self.id_by_var {
            var_by_id.entry(id).or_insert(vec![]).push(var);
        }
        var_by_id
    }

    /// Resolve variable, remove it from [Bindings] and return result.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::*;
    /// 
    /// let mut bindings = bind!{ x: expr!(y), y: expr!("A" z), z: expr!("B") };
    ///
    /// assert_eq!(bindings.resolve_and_remove(&VariableAtom::new("x")), Some(expr!("A" "B")));
    /// assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
    /// assert_eq!(bindings.resolve(&VariableAtom::new("y")), Some(expr!("A" "B")));
    /// ```
    pub fn resolve_and_remove(&mut self, var: &VariableAtom) -> Option<Atom> {
        let result = self.resolve(&var);
        self.remove(&var);
        result
    }

    fn remove(&mut self, var: &VariableAtom) -> Option<Atom> {
        match self.id_by_var.remove(var) {
            None => None,
            Some(var_id) => {
                let no_other_var = self.var_by_id(var_id, |_| true) == None;
                if no_other_var {
                    self.value_by_id.remove(&var_id)
                } else {
                    None
                }
            }
        }
    }

    fn build_var_mapping<'a>(&'a self, required_names: &HashSet<VariableAtom>, required_ids: &HashSet<u32>) -> HashMap<&'a VariableAtom, &'a VariableAtom> {
        let mut id_names: HashSet<VariableAtom> = HashSet::new();
        let mut mapping = HashMap::new();
        for (var, &id) in &self.id_by_var {
            match (required_names.contains(var), required_ids.contains(&id)) {
                (true, _) => { mapping.insert(var, var); },
                (false, false) => {},
                (false, true) => {
                    let mapped = self.var_by_id(id,
                        |alt| required_names.contains(alt) || id_names.contains(alt));
                    match mapped {
                        Some(mapped) => { mapping.insert(var, mapped); },
                        None => {
                            id_names.insert(var.clone());
                            mapping.insert(var, var);
                        },
                    }
                },
            }
        }
        mapping
    }

    fn find_deps(&self, var: &VariableAtom, deps: &mut HashSet<VariableAtom>) {
        deps.insert(var.clone());
        self.get_value(var).iter()
            .for_each(|value| {
                value.iter().filter_map(AtomIter::extract_var)
                    .for_each(|var| { self.find_deps(var, deps); });
            });
    }

    /// Get narrow bindings which contains only passed set of variables and
    /// their dependencies.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::*;
    /// use std::collections::HashSet;
    ///
    /// let bindings = bind!{ leftA: expr!("A"), leftA: expr!(rightB),
    ///     leftC: expr!("C"), leftD: expr!(rightE), rightF: expr!("F") };
    /// let right = bindings.narrow_vars(&HashSet::from([VariableAtom::new("rightB"),
    ///     VariableAtom::new("rightE"), VariableAtom::new("rightF")]));
    ///
    /// assert_eq!(right, bind!{ rightB: expr!("A"), rightF: expr!("F"), rightE: expr!(rightE) });
    /// ```
    pub fn narrow_vars(&self, vars: &HashSet<VariableAtom>) -> Bindings {
        let mut deps: HashSet<VariableAtom> = HashSet::new();
        for var in vars {
            self.find_deps(var, &mut deps);
        }

        let dep_ids: HashSet<u32> = deps.iter()
            .map(|var| self.id_by_var.get(var))
            .filter(Option::is_some)
            .map(Option::unwrap).map(|&id| id)
            .collect();

        let mapping = self.build_var_mapping(&vars, &dep_ids);
        
        let mut bindings = Bindings::new();
        bindings.next_var_id = self.next_var_id;
        for (var, &id) in &self.id_by_var {
            if deps.contains(var) {
                bindings.id_by_var.insert((*mapping.get(var).unwrap()).clone(), id);
            }
        }
        for (&id, value) in &self.value_by_id {
            if dep_ids.contains(&id) {
                let mut mapped_value = value.clone();
                mapped_value.iter_mut().filter_map(AtomIterMut::extract_var)
                    .for_each(|var| { mapping.get(var).map(|mapped| *var = (*mapped).clone()); });
                bindings.value_by_id.insert(id, mapped_value);
            }
        }
        log::trace!("Bindings::narrow_vars: {} -> {}", self, bindings);
        bindings
    }

    fn has_loops(&self) -> bool {
        let vars_by_id = self.vars_by_id();
        for (var_id, value) in &self.value_by_id {
            let mut used_vars = HashSet::new();
            vars_by_id.get(var_id).unwrap().iter().for_each(|var| { used_vars.insert(*var); });
            match self.resolve_vars_in_atom(value, &used_vars) {
                VarResolutionResult::Loop => return true,
                _ => {},
            }
        }
        false
    }

    /// Returns iterator of `(&VariableAtom, Atom)` pairs to represent [Bindings] in C API.
    /// Each pair contains reference to a [VariableAtom] and instance of [Atom]
    /// which contains resolved value of the variable. See [Bindings::resolve].
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::*;
    ///
    /// let bindings = bind!{ leftA: expr!("A"), leftA: expr!(rightB),
    ///     leftC: expr!("C"), leftD: expr!(rightE), rightF: expr!("F") };
    /// let pairs: Vec<(&VariableAtom, Atom)> = bindings.iter().collect();
    ///
    /// assert_eq_no_order!(pairs, vec![
    ///     (&VariableAtom::new("leftA"), expr!("A")),
    ///     (&VariableAtom::new("rightB"), expr!("A")),
    ///     (&VariableAtom::new("leftC"), expr!("C")),
    ///     (&VariableAtom::new("leftD"), expr!(rightE)),
    ///     (&VariableAtom::new("rightE"), expr!(leftD)),
    ///     (&VariableAtom::new("rightF"), expr!("F")),
    /// ]);
    /// ```
    pub fn iter(&self) -> BindingsIter {
        BindingsIter { bindings: self, delegate: self.id_by_var.iter() }
    }
}

impl Display for Bindings {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vars_by_id = self.vars_by_id();
        write!(f, "{{ ")?;
        for (i, (id, vars)) in vars_by_id.iter().enumerate() {
            let prefix = if i == 0 { "" } else { ", " };
            write!(f, "{}", prefix)?;
            for (i, var) in vars.iter().enumerate() {
                let prefix = if i == 0 { "" } else { " = " };
                write!(f, "{}{}", prefix, var)?;
            }
            match self.value_by_id.get(id) {
                Some(value) => write!(f, " = {}", value)?,
                None => {},
            }
        }
        write!(f, " }}")
    }

}

impl Debug for Bindings {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }

}

use std::convert::TryFrom;
use std::cmp::max;

impl PartialEq for Bindings {

    fn eq(&self, other: &Self) -> bool {
        fn to_usize(n: u32) -> usize {
            usize::try_from(n).unwrap()
        }

        let max_var_id = max(self.next_var_id, other.next_var_id);
        let mut other_to_self: Vec<u32> = vec![u32::MAX; to_usize(max_var_id)];
        for (name, self_var) in &self.id_by_var {
            match other.id_by_var.get(name) {
                None => return false, // no such name in other
                Some(other_var) => other_to_self[to_usize(*other_var)] = *self_var,
            }
        }
        for (name, _) in &other.id_by_var {
            match self.id_by_var.get(name) {
                None => return false, // no such name in self
                Some(_) => {},
            }
        }
        for other_var in 0..other.next_var_id {
            let self_var = other_to_self[to_usize(other_var)];
            if self.value_by_id.get(&self_var) != other.value_by_id.get(&other_var) {
                return false; // values are not equal
            }
        }
        true
    }

}

impl From<Vec<(VariableAtom, Atom)>> for Bindings {

    fn from(pairs: Vec<(VariableAtom, Atom)>) -> Self {
        let mut bindings = Bindings::new();
        pairs.into_iter().for_each(|(var, val)| {
            match val {
                Atom::Variable(val) => bindings.add_var_equality(&var, &val),
                _ => bindings.add_var_binding(var, val),
            };
        });
        bindings
    }

}

/// Iterator over `(&VariableAtom, Atom)` pairs in [Bindings].
/// Each pair contains reference to a [VariableAtom] and instance of [Atom]
/// which contains resolved value of the variable. See [Bindings::resolve].
pub struct BindingsIter<'a> {
    bindings: &'a Bindings,
    delegate: std::collections::hash_map::Iter<'a, VariableAtom, u32>,
}

impl<'a> BindingsIter<'a> {

    fn next(&mut self) -> Option<(&'a VariableAtom, Atom)> {
        self.delegate.next().and_then(|(var, _id)| {
            match self.bindings.resolve(var) {
                Some(atom) => Some((var, atom)),
                None => None,
            }
        })
    }

}

impl<'a> Iterator for BindingsIter<'a> {
    type Item = (&'a VariableAtom, Atom);

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

impl<'a> IntoIterator for &'a Bindings {
    type Item = (&'a VariableAtom, Atom);
    type IntoIter = BindingsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}


/// Iterator over atom matching results. Each result is an instance of [Bindings].
pub type MatchResultIter = Box<dyn Iterator<Item=matcher::Bindings>>;

/// Matches two atoms and returns an iterator over results. Atoms are
/// treated symmetrically.
///
/// Both `left` and `right` atom can contain variables. Variables are treated
/// as equality restrictions for the parts of the atoms. If same variable
/// occurs in atom more than once it means the parts of the atom matched
/// by these occurences should be equal or also matchable. Thus result of the
/// matching contains bindings of the variables filled by values from `left` or
/// `right` atom.
///
/// Sometimes two variables are equal without assigning specific
/// value to them. For instance `($x $x)` is being matched with atom
/// `($a $b)`. Effectively it means `$a` is equal to `$b`.  In such case
/// matching algorithm creates variables equality and adds it to bindings.
/// Final bindings contain `{ $a = $x = $b }`.
///
/// Some matching results can contain variable loops. For example for `($a ($a))`
/// matched with `($x $x)` the result is `{ $a = $x = ($x) }`. Such results are
/// excluded from match results.
///
/// # Examples
///
/// ```
/// use hyperon::*;
/// use hyperon::atom::matcher::*;
///
/// let left  = expr!( b (b) a a);
/// let right = expr!("v" x  x y);
/// let norm: Vec<Bindings> = match_atoms(&left, &right).collect();
///
/// assert_eq!(norm, vec![bind!{b: sym!("v"), x: expr!((b)), a: expr!(x), y: expr!(a)}]);
///
/// let left  = expr!(a (a));
/// let right = expr!(x  x );
/// let empty: Vec<Bindings> = match_atoms(&left, &right).collect();
///
/// assert_eq!(empty, vec![]);
/// ```
pub fn match_atoms<'a>(left: &'a Atom, right: &'a Atom) -> MatchResultIter {
    Box::new(match_atoms_recursively(left, right)
        .filter(|binding| !binding.has_loops()))
}

fn match_atoms_recursively(left: &Atom, right: &Atom) -> MatchResultIter {
    log::trace!("match_atoms_recursively: {} ~ {}", left, right);
    fn empty() -> MatchResultIter { Box::new(std::iter::empty()) }
    fn once(b: Bindings) -> MatchResultIter { Box::new(std::iter::once(b)) }

    match (left, right) {
        (Atom::Symbol(a), Atom::Symbol(b)) if a == b => once(Bindings::new()),
        (Atom::Variable(dv), Atom::Variable(pv)) => {
            Bindings::new().with_var_equality(dv, pv).map_or(empty(), once)
        }
        (Atom::Variable(v), b) => {
            Bindings::new().with_var_binding(v, b).map_or(empty(), once)
        }
        (a, Atom::Variable(v)) => {
            Bindings::new().with_var_binding(v, a).map_or(empty(), once)
        },
        (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b }))
                if a.len() == b.len() => {
            a.iter().zip(b.iter()).fold(once(Bindings::new()),
                |acc, (a, b)| {
                    match_result_product(acc, match_atoms_recursively(a, b))
                })
        },
        // TODO: one more case for the special flag to see if GroundedAtom is
        // matchable. If GroundedAtom is matched with VariableAtom there are
        // two way to calculate match: (1) pass variable to the
        // GroundedAtom::match(); (2) assign GroundedAtom to the Variable.
        // Returning both results breaks tests right now.
        (Atom::Grounded(a), _) => {
            Box::new(a.match_(right))
        },
        (_, Atom::Grounded(b)) => {
            Box::new(b.match_(left))
        },
        _ => empty(),
    }
}

/// Merges each bindings from `prev` iter to each bindings from `next`
/// iter. The result is an iter over successfully merged bindings.
pub fn match_result_product(prev: MatchResultIter, next: MatchResultIter) -> MatchResultIter {
    let next : Vec<Bindings> = next.collect();
    log::trace!("match_result_product_iter, next: {:?}", next);
    Box::new(prev.flat_map(move |p| -> Vec<Option<Bindings>> {
        next.iter().map(|n| Bindings::merge(&p, n)).collect()
    }).filter(Option::is_some).map(Option::unwrap))
}

/// Applies bindings to atom. Function replaces all variables in atom by
/// corresponding bindings.
///
/// # Examples
///
/// ```
/// use hyperon::*;
/// use hyperon::atom::matcher::apply_bindings_to_atom;
///
/// let binds = bind!{ y: expr!("Y") };
/// let atom = apply_bindings_to_atom(&expr!("+" "X" y), &binds);
///
/// assert_eq!(atom, expr!("+" "X" "Y"));
/// ```
pub fn apply_bindings_to_atom(atom: &Atom, bindings: &Bindings) -> Atom {
    let mut result = atom.clone();
    if !bindings.is_empty() {
        result.iter_mut().for_each(|atom| match atom {
            Atom::Variable(var) => {
                bindings.resolve(var).map(|value| *atom = value);
            },
            _ => {},
        });
    }
    log::trace!("apply_bindings_to_atom: {} | {} -> {}", atom, bindings, result);
    result
}

/// Applies bindings `from` to the each value from bindings `to`.
/// Function also checks that resulting value is not expressed recursively
/// via variable to which value is bound. Function returns error if such
/// value is detected.
///
/// # Examples
///
/// ```
/// use hyperon::*;
/// use hyperon::atom::matcher::apply_bindings_to_bindings;
///
/// let from = bind!{ x: expr!("Y") };
/// let to = bind!{ y: expr!(x) };
/// let rec = bind!{ x: expr!(y) };
/// let _loop = bind!{ x: expr!((y)) };
///
/// assert_eq!(apply_bindings_to_bindings(&from, &to), Ok(bind!{ y: expr!("Y"), x: expr!(y) }));
/// assert_eq!(apply_bindings_to_bindings(&rec, &to), Ok(bind!{ y: expr!(x) }));
/// assert_eq!(apply_bindings_to_bindings(&_loop, &to), Err(()));
/// ```
pub fn apply_bindings_to_bindings(from: &Bindings, to: &Bindings) -> Result<Bindings, ()> {
    // TODO: apply_bindings_to_bindings can be replaced by Bindings::merge,
    // when Bindings::merge are modified to return Vec<Bindings>
    Bindings::merge(to, from).filter(|bindings| !bindings.has_loops()).ok_or(())
}

/// Checks if atoms are equal up to variables replacement.
///
/// # Examples
///
/// ```
/// use hyperon::expr;
/// use hyperon::atom::matcher::atoms_are_equivalent;
///
/// let atom = expr!(a "b" c);
/// let eq = expr!(x "b" d);
/// let neq = expr!(x "b" x);
///
/// assert!(atoms_are_equivalent(&atom, &eq));
/// assert!(!atoms_are_equivalent(&atom, &neq));
/// ```
pub fn atoms_are_equivalent(left: &Atom, right: &Atom) -> bool {
    atoms_are_equivalent_with_bindings(left, right, &mut HashMap::new(), &mut HashMap::new())
}

use std::collections::hash_map::Entry;

fn atoms_are_equivalent_with_bindings<'a, 'b: 'a>(left: &'b Atom, right: &'b Atom,
        left_vars: &'a mut HashMap<&'b VariableAtom, &'b VariableAtom>,
        right_vars: &'a mut HashMap<&'b VariableAtom, &'b VariableAtom>) -> bool {

    fn can_be_renamed<'a, 'b: 'a>(map: &'a mut HashMap<&'b VariableAtom, &'b VariableAtom>,
        var: &'b VariableAtom, atom: &'b VariableAtom) -> bool {
        match map.entry(var) {
            Entry::Occupied(entry) => *entry.get() == atom,
            Entry::Vacant(entry) => {
                entry.insert(atom);
                true
            }
        }
    }

    match (left, right) {
        (Atom::Variable(left), Atom::Variable(right)) =>
            can_be_renamed(left_vars, left, right) &&
                can_be_renamed(right_vars, right, left),
        (Atom::Symbol(left), Atom::Symbol(right)) => left == right,
        (Atom::Grounded(left), Atom::Grounded(right)) => left == right,
        (Atom::Expression(left), Atom::Expression(right)) =>
            left.children().len() == right.children().len() &&
            left.children().iter().zip(right.children().iter())
                .all(|(left, right)| atoms_are_equivalent_with_bindings(
                        left, right, left_vars, right_vars)),
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use crate::assert_eq_no_order;
    use super::*;

    fn assert_match(left: Atom, right: Atom, expected: Vec<Bindings>) {
        let actual: Vec<Bindings> = match_atoms(&left, &right).collect();
        assert_eq_no_order!(actual, expected);
    }

    #[test]
    fn match_variables_in_left() {
        assert_match(
            expr!("+"  a  ("*"  b   c )),
            expr!("+" "A" ("*" "B" "C")),
            vec![bind!{a: expr!("A"), b: expr!("B"), c: expr!("C")}]);
    }

    #[test]
    fn match_value_conflict_for_variable_in_left() {
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
    fn match_spread_value_via_left_variable() {
        assert_match(
            expr!( a  a a),
            expr!("v" x y),
            vec![bind!{x: expr!(a), y: expr!(a), a: sym!("v")}]);
    }

    #[test]
    fn match_spread_value_via_left_variable_reverse_order() {
        assert_match(
            expr!(a a  a  a),
            expr!(x x "v" y),
            vec![bind!{a: sym!("v"), x: expr!(a), y: expr!(a)}]);
    }

    #[test]
    fn match_spread_value_via_right_variable() {
        assert_match(
            expr!("v" a a),
            expr!( x  x y),
            vec![bind!{x: sym!("v"), x: expr!(a), y: expr!(a)}]);
    }

    #[test]
    fn match_spread_value_via_right_variable_reverse_order() {
        assert_match(
            expr!(a "v" a),
            expr!(x  x  y),
            vec![bind!{x: expr!("v"), x: expr!(a), y: expr!(a)}]);
    }

    #[test]
    fn match_replace_variable_via_left_variable() {
        assert_match(
            expr!(a a),
            expr!(x y),
            vec![bind!{x: expr!(a), y: expr!(a)}]);
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
        let x_uniq = Atom::Variable(VariableAtom::new_id("x", 1));
        assert_match(
            make_variables_unique(&expr!(("A" x) ("B" x))),
                                   expr!(("A" x)    z   ),
            vec![bind!{x: x_uniq.clone(), z: Atom::expr([sym!("B"), x_uniq])}]);
    }

    #[test]
    fn match_equality_of_right_variables_inside_expression() {
        assert_match(
            expr!( a    a   a),
            expr!((x) ("v") y),
            vec![bind!{x: expr!("v"), a: expr!((x)), y: expr!(a)}]);
    }

    #[test]
    fn match_equality_of_left_variables_inside_expression() {
        assert_match(
            expr!((a) ("v") a),
            expr!( x    x   y),
            vec![bind!{x: expr!((a)), y: expr!("v"), y: expr!(a)}]);
    }

    #[test]
    fn match_match_values_when_merging_two_variable_sets() {
        assert_match(
            expr!((a)  b   b a),
            expr!( x ("v") x y),
            vec![bind!{x: expr!((a)), b: expr!(("v")), x: expr!(b), y: expr!(a)}]);
    }

    #[derive(PartialEq, Clone, Debug)]
    struct Rand{}

    impl Grounded for Rand {
        fn type_(&self) -> Atom {
            Atom::sym("Rand")
        }
        fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
            execute_not_executable(self)
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match other {
                Atom::Expression(expr) if expr.children().len() == 1 =>
                    match expr.children()[0] {
                        Atom::Variable(ref var) => Box::new(std::iter::once(
                            Bindings::new().with_var_binding(var, expr!({42})).unwrap())),
                        _ => Box::new(std::iter::empty()),
                }
                _ => Box::new(std::iter::empty()),
            }
        }
    }
    
    impl Display for Rand {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "Rand")
        }
    }

    #[test]
    fn match_atoms_with_custom_matcher_implementation() {
        assert_match(
            expr!({Rand{}}),
            expr!((x)),
            vec![bind!{x: expr!({42})}]);
        assert_match(
            expr!((x)),
            expr!({Rand{}}),
            vec![bind!{x: expr!({42})}]);
    }

    #[ignore = "Requires sorting inside Bindings to be stable"]
    #[test]
    fn bindings_match_display() {
        let mut bindings = Bindings::new();
        bindings.add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("b"));
        bindings.add_var_binding(VariableAtom::new("b"), Atom::sym("v"));
        bindings.add_var_equality(&VariableAtom::new("c"), &VariableAtom::new("d"));
        
        assert_eq!(bindings.to_string(), "{ $a = $b = v, $c = $d }");
    }

    #[test]
    fn bindings_get_variable_no_value() {
        let mut bindings = Bindings::new();
        bindings.add_var_no_value(&VariableAtom::new("x"));

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
    }

    #[test]
    fn bindings_get_variable_bound_to_value() {
        let mut bindings = Bindings::new();
        bindings.add_var_binding(VariableAtom::new("x"), expr!("A" y));
        bindings.add_var_binding(VariableAtom::new("y"), expr!("B" z));

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), Some(expr!("A" ("B" z))));
        assert_eq!(bindings.resolve(&VariableAtom::new("y")), Some(expr!("B" z)));
    }

    #[test]
    fn bindings_get_variable_bound_to_value_with_loop() {
        let mut bindings = Bindings::new();
        bindings.add_var_binding(VariableAtom::new("x"), expr!("A" y));
        bindings.add_var_binding(VariableAtom::new("y"), expr!("B" x));

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
        assert_eq!(bindings.resolve(&VariableAtom::new("y")), None);
    }

    #[test]
    fn bindings_get_variable_bound_to_variable() {
        let mut bindings = Bindings::new();
        bindings.add_var_binding(VariableAtom::new("x"), expr!(x));
        
        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
    }

    #[test]
    fn bindings_get_variable_equal_to_variable() {
        let mut bindings = Bindings::new();
        bindings.add_var_equality(&VariableAtom::new("x"), &VariableAtom::new("y"));

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), Some(expr!(y)));
    }

    #[test]
    fn bindings_partial_eq() {
        assert!(bind!{ x: expr!("X"), y: expr!("Y") } != bind!{ y: expr!("Y") });
        assert!(bind!{ y: expr!("Y") } != bind!{ x: expr!("X"), y: expr!("Y") });
        assert!(bind!{ x: expr!(a) } != bind!{ x: expr!(b) });
        assert!(bind!{ x: expr!(y), y: expr!("X") } == bind!{ x: expr!("X"), y: expr!(x) });
    }

    #[test]
    fn bindings_narrow_vars() {
        let mut bindings = Bindings::new();
        bindings.add_var_binding(VariableAtom::new("leftA"), expr!("A"));
        bindings.add_var_equality(&VariableAtom::new("leftA"), &VariableAtom::new("rightB"));
        bindings.add_var_binding(VariableAtom::new("leftC"), expr!("C"));
        bindings.add_var_equality(&VariableAtom::new("leftD"), &VariableAtom::new("rightE"));
        bindings.add_var_binding(VariableAtom::new("rightF"), expr!("F"));

        let narrow = bindings.narrow_vars(&HashSet::from([VariableAtom::new("rightB"),
            VariableAtom::new("rightE"), VariableAtom::new("rightF")]));

        assert_eq!(narrow, bind!{ rightB: expr!("A"), rightF: expr!("F"), rightE: expr!(rightE) });
    }
}
