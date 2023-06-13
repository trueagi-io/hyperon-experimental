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

/// Constructs a new [BindingsSet] with predefined content.
/// Macros takes variable/value pairs as arguments. If value is a single
/// variable then the pair means variable equality. Otherwise pair means
/// assigning value. May be ineffective, should be used mainly in unit tests.
///
/// # Examples
///
/// ```
/// use hyperon::*;
///
/// // Compose a BindingsSet with implicit Bindings
/// let set = bind_set![{a: expr!("A")}, {a: expr!("APrime")}, {a: expr!("ADoublePrime")}];
///
/// assert_eq!(set.len(), 3);
/// assert_eq!(set[0].resolve(&VariableAtom::new("a")), Some(expr!("A")));
/// assert_eq!(set[2].resolve(&VariableAtom::new("a")), Some(expr!("ADoublePrime")));
///
/// // Compose a BindingsSet with explicitly defined Bindings
/// let set = bind_set![bind!{a: expr!("A")}, bind!{a: expr!("APrime")}];
///
/// assert_eq!(set.len(), 2);
/// assert_eq!(set[0].resolve(&VariableAtom::new("a")), Some(expr!("A")));
/// assert_eq!(set[1].resolve(&VariableAtom::new("a")), Some(expr!("APrime")));
///
/// // Mix and match, including existing Bindings and BindingsSets
/// let other_set = bind_set![];
/// let set = bind_set![{a: expr!("A")}, other_set];
/// ```
#[macro_export]
macro_rules! bind_set {
    // An empty BindingsSet
    [] => {
        $crate::atom::matcher::BindingsSet::empty()
    };
    // An implicitly defined Bindings
    [{$($b:tt)*}] => {
        $crate::atom::matcher::BindingsSet::from($crate::bind!{$($b)*})
    };
    // An explicitly defined Bindings
    [$b:expr] => {
        $crate::atom::matcher::BindingsSet::from($b)
    };
    // Recursive pattern to handle multiple Bindings, where the next item is implicit
    [{$($b:tt)*}, $($b_rest:tt)*] => {{
        let mut b = bind_set![{$($b)*}];
        b.extend(bind_set![$($b_rest)*]);
        b
    }};
    // Recursive pattern to handle multiple Bindings, where the next item is explicit
    [$b:expr, $($b_rest:tt)*] => {{
        let mut b = bind_set![$b];
        b.extend(bind_set![$($b_rest)*]);
        b
    }};
}

use std::collections::{HashMap, HashSet};
use core::iter::FromIterator;
use std::cmp::max;

use super::*;
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
                        None => VarResolutionResult::Some(Atom::Variable(var.clone())),
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

    /// Asserts equality between two [VariableAtom]s.  If the existing bindings for `a` and `b` are
    /// incompatible then this method will return an Err.  This method will also return an Err if the
    /// operation causes split bindings. In situations where split bindings are a possibility, call
    /// [BindingsSet::add_var_equality] instead.
    pub fn add_var_equality(self, a: &VariableAtom, b: &VariableAtom) -> Result<Bindings, &'static str> {
        let temp_set = self.add_var_equality_internal(a, b);
        match temp_set.len() {
            0 => Err("Bindings are incompatible"),
            1 => Ok(Bindings::try_from(temp_set).unwrap()),
            _ => Err("Bindings split occurred.  Try BindingsSet::add_var_equality")
        }
    }

    fn add_var_equality_internal(mut self, a: &VariableAtom, b: &VariableAtom) -> BindingsSet {
        match (self.id_by_var.get(a), self.id_by_var.get(b)) {
            (Some(&a_var_id), Some(&b_var_id))  =>
                if a_var_id != b_var_id {
                    self.merge_var_ids(a_var_id, b_var_id)
                } else {
                    BindingsSet::from(self)
                }
            (Some(&var_id), None) => {
                self.id_by_var.insert(b.clone(), var_id);
                BindingsSet::from(self)
            },
            (None, Some(&var_id)) => {
                self.id_by_var.insert(a.clone(), var_id);
                BindingsSet::from(self)
            },
            (None, None) => {
                let var_id = self.get_next_var_id();
                self.id_by_var.insert(a.clone(), var_id);
                self.id_by_var.insert(b.clone(), var_id);
                BindingsSet::from(self)
            },
        }
    }

    fn match_values(&self, current: &Atom, value: &Atom) -> BindingsSet {
        match_atoms_recursively(current, value).into_iter()
            .flat_map(|binding| binding.merge_v2(self))
            .collect()
    }

    /// Internal function used by the [Bindings::add_var_equality] implementation
    fn merge_var_ids(mut self, a_var_id: u32, b_var_id: u32) -> BindingsSet {
        fn replace_id(id_by_var: &mut HashMap<VariableAtom, u32>, to_replace: u32, replace_by: u32) {
            id_by_var.iter_mut().for_each(|(_var, id)| {
                if *id == to_replace {
                    *id = replace_by;
                }
            });
        }
        match (self.value_by_id.get(&a_var_id), self.value_by_id.get(&b_var_id)) {
            (Some(a_val), Some(b_val)) => {
                self.match_values(a_val, b_val)
            },
            (Some(_), None) => {
                replace_id(&mut self.id_by_var, b_var_id, a_var_id);
                BindingsSet::from(self)
            }
            _ => {
                replace_id(&mut self.id_by_var, a_var_id, b_var_id);
                BindingsSet::from(self)
            },
        }
    }

    fn get_next_var_id(&mut self) -> u32 {
        let next_var_id = self.next_var_id;
        self.next_var_id = self.next_var_id + 1;
        next_var_id
    }

    /// Tries to insert `value` as a binding for the `var`. If `self` already
    /// has binding for the `var` and it is not matchable with the `value` then
    /// function returns Err. Otherwise it returns updated Bindings.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::*;
    /// use hyperon::matcher::Bindings;
    ///
    /// # fn main() -> Result<(), &'static str> {
    /// let a = VariableAtom::new("a");
    /// let b = VariableAtom::new("b");
    /// let c = VariableAtom::new("c");
    /// let mut binds = bind!{ a: expr!("A"), b: expr!("B") };
    ///
    /// // Re-asserting an existing binding is ok
    /// binds = binds.add_var_binding_v2(&a, &expr!("A"))?;
    ///
    /// // Asserting a conflicting binding is an error
    /// assert!(binds.clone().add_var_binding_v2(&b, &expr!("C")).is_err());
    ///
    /// // Creating a new binding is ok
    /// binds = binds.add_var_binding_v2(&c, &expr!("C"))?;
    ///
    /// assert_eq!(binds.resolve(&a), Some(expr!("A")));
    /// assert_eq!(binds.resolve(&b), Some(expr!("B")));
    /// assert_eq!(binds.resolve(&c), Some(expr!("C")));
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// TODO: Rename to `add_var_binding` when clients have adopted the new API
    pub fn add_var_binding_v2<T1, T2>(self, var: T1, value: T2) -> Result<Bindings, &'static str>
        where T1: RefOrMove<VariableAtom>, T2: RefOrMove<Atom>
    {
        let temp_set = self.add_var_binding_internal(var, value);
        match temp_set.len() {
            0 => Err("Bindings are incompatible"),
            1 => Ok(Bindings::try_from(temp_set).unwrap()),
            _ => Err("Bindings split occurred.  Try BindingsSet::add_var_binding")
        }
    }

    fn add_var_binding_internal<T1, T2>(mut self, var: T1, value: T2) -> BindingsSet
        where T1: RefOrMove<VariableAtom>, T2: RefOrMove<Atom>
    {
        match self.id_by_var.get(var.as_ref()) {
            Some(var_id) =>
                match self.value_by_id.get(var_id) {
                    Some(current) => {
                        if current == value.as_ref() {
                            BindingsSet::from(self)
                        } else {
                            self.match_values(current, value.as_ref())
                        }
                    },
                    None => {
                        self.value_by_id.insert(*var_id, value.as_value());
                        BindingsSet::from(self)
                    },
                },
            None => {
                let var_id = self.get_next_var_id();
                self.id_by_var.insert(var.as_value(), var_id);
                self.value_by_id.insert(var_id, value.as_value());
                BindingsSet::from(self)
            },
        }
    }

    /// Tries to insert `value` as a binding for the `var`. If `self` already
    /// has binding for the `var` and it is not matchable with the `value` then
    /// function returns `false`. Otherwise it inserts binding and returns `true`.
    ///
    /// TODO: This implementation should be deprecated in favor of the implementation in `add_var_binding_v2`
    pub fn add_var_binding<'a, T1: RefOrMove<VariableAtom>, T2: RefOrMove<Atom>>(&mut self, var: T1, value: T2) -> bool {
        match self.clone().add_var_binding_v2(var.as_value(), value.as_value()) {
            Ok(new_bindings) => {
                *self = new_bindings;
                true
            },
            Err(_) => false
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

    /// Merges `b` bindings into self if they are compatible.  May return a [BindingsSet] containings
    /// multiple [Bindings] if appropriate.  If no compatible bindings can be merged, [BindingsSet::empty()]
    /// will be returned.
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
    ///
    /// TODO: Rename to `merge` when clients have adopted new API
    pub fn merge_v2(self, b: &Bindings) -> BindingsSet {
        log::trace!("Bindings::merge: a: {}, b: {}", self, b);
        let trace_self = match log::log_enabled!(log::Level::Trace) {
            true => Some(self.clone()),
            false => None
        };

        let results = b.id_by_var.iter().fold(smallvec::smallvec![(self, HashMap::new())],
            |results, (var, var_id)| -> smallvec::SmallVec<[(Bindings, HashMap<u32, VariableAtom>); 1]> {
                let mut all_results = smallvec::smallvec![];

                for (result, mut b_vars_merged) in results {
                    let new_results = if let Some(first_var) = b_vars_merged.get(&var_id) {
                        result.add_var_equality_internal(first_var, var)
                    } else {
                        b_vars_merged.insert(*var_id, var.clone());
                        if let Some(value) = b.value_by_id.get(var_id) {
                            result.add_var_binding_internal(var, value)
                        } else {
                            BindingsSet::from(result.with_var_no_value(var))
                        }
                    };
                    all_results.extend(new_results.into_iter().map(|new_binding| (new_binding, b_vars_merged.clone())));
                }
                all_results
            });

        let results = results.into_iter().map(|(result, _)| result).collect();
        if let Some(self_copy) = trace_self {
            log::trace!("Bindings::merge: {} ^ {} -> {:?}", self_copy, b, results);
        }
        results
    }

    /// Compatibility shim for merge_v2.  TODO: Delete then the new API has been adopted downstream
    pub fn merge(a: &Bindings, b: &Bindings) -> Option<Bindings> {
        a.clone().merge_v2(b).into_iter().next()
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

    fn build_var_mapping<'a>(&'a self, required_names: &HashSet<&VariableAtom>, required_ids: &HashSet<u32>) -> HashMap<&'a VariableAtom, &'a VariableAtom> {
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
                value.iter().filter_map(|atom| <&VariableAtom>::try_from(atom).ok())
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
    /// let right = bindings.narrow_vars(&HashSet::from([&VariableAtom::new("rightB"),
    ///     &VariableAtom::new("rightE"), &VariableAtom::new("rightF")]));
    ///
    /// assert_eq!(right, bind!{ rightB: expr!("A"), rightF: expr!("F"), rightE: expr!(rightE) });
    /// ```
    pub fn narrow_vars(&self, vars: &HashSet<&VariableAtom>) -> Bindings {
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
                mapped_value.iter_mut().filter_map(|atom| <&mut VariableAtom>::try_from(atom).ok())
                    .for_each(|var| { mapping.get(var).map(|mapped| *var = (*mapped).clone()); });
                bindings.value_by_id.insert(id, mapped_value);
            }
        }
        log::trace!("Bindings::narrow_vars: {} -> {}", self, bindings);
        bindings
    }

    /// Remove variable equalities from the Bindings and represent them as a
    /// variable values. Set of preferred variables is used to select the top
    /// level variable name. For example, variable bindings `{ $a = $b <- A }`
    /// is converted into `{ $a <- A, $b <- $a }` when preferred vars set is
    /// `{ $a }`, and into `{ $b <- A, $a <- $b }` when preferred vars set is
    /// `{ $b }`.
    pub fn convert_var_equalities_to_bindings(mut self, preferred_vars: &HashSet<VariableAtom>) -> Self {
        let trace_self = match log::log_enabled!(log::Level::Trace) {
            true => Some(self.clone()),
            false => None
        };
        let mut value_vars: HashMap<u32, &VariableAtom> = HashMap::new();
        let mut renamed_vars: HashMap<VariableAtom, VariableAtom> = HashMap::new();
        for (var, var_id) in &self.id_by_var {
            if let Some(&top_level_var) = value_vars.get(var_id) {
                if top_level_var != var {
                    renamed_vars.insert(var.clone(), top_level_var.clone());
                }
            } else {
                if preferred_vars.contains(var) {
                    value_vars.insert(*var_id, var);
                } else {
                    let new_var = self.var_by_id(*var_id, |v| preferred_vars.contains(v));
                    match new_var {
                        Some(new_var) => {
                            value_vars.insert(*var_id, new_var);
                            renamed_vars.insert(var.clone(), new_var.clone());
                        },
                        None => {
                            value_vars.insert(*var_id, var);
                        },
                    }
                }
            }
        }
        for (old_var, new_var) in renamed_vars {
            self.remove(&old_var);
            self.add_var_binding(old_var, Atom::Variable(new_var));
        }
        if let Some(self_copy) = trace_self {
            log::trace!("Bindings::convert_var_equalities_to_bindings: preferred_vars: {:?}, {} -> {}", preferred_vars, self_copy, self);
        }
        self
    }

    /// Keep only variables passed in vars
    pub fn cleanup(&mut self, vars: &HashSet<&VariableAtom>) {
        let to_remove: HashSet<VariableAtom> = self.id_by_var.iter()
            .filter_map(|(var, _id)| if !vars.contains(var) { Some(var.clone()) } else { None })
            .collect();
        to_remove.into_iter().for_each(|var| { self.id_by_var.remove(&var); });
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

    fn into_vec_of_pairs(mut self) -> Vec<(VariableAtom, Atom)> {
        let mut result = Vec::new();
        let mut core_vars: HashMap<u32, Atom> = HashMap::new();

        for (var, id) in self.id_by_var {
            match self.value_by_id.remove(&id) {
                Some(value) => {
                    core_vars.insert(id, Atom::Variable(var.clone()));
                    result.push((var, value));
                },
                None => {
                    match core_vars.get(&id) {
                        Some(core_var) => { result.push((var, core_var.clone())); }
                        None => { core_vars.insert(id, Atom::Variable(var)); }
                    }
                },
            }
        }

        result
    }

    /// Rename variables inside bindings using `rename`.
    pub fn rename_vars<F>(self, mut rename: F) -> Self where F: FnMut(VariableAtom) -> VariableAtom {
        self.into_iter()
            .map(|(mut v, mut a)| {
                v = rename(v);
                a.iter_mut().filter_type::<&mut VariableAtom>()
                    .for_each(|var| *var = rename(var.clone()));
                (v, a)
            })
            .collect::<Vec<(VariableAtom, Atom)>>()
            .into()
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
                Some(value) => write!(f, " <- {}", value)?,
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
        Bindings::from(&pairs[..])
    }
}

impl From<&[(VariableAtom, Atom)]> for Bindings {
    fn from(pairs: &[(VariableAtom, Atom)]) -> Self {
        let mut bindings = Bindings::new();
        for (var, val) in pairs {
            bindings = match val {
                Atom::Variable(val) => bindings.add_var_equality(&var, &val),
                _ => bindings.add_var_binding_v2(var, val),
            }.unwrap_or_else(|e| panic!("Error creating Bindings from Atoms: {}", e));
        }
        bindings
    }
}

impl IntoIterator for Bindings {
    type Item = (VariableAtom, Atom);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    /// Converts [Bindings] into an iterator of pairs `(VariableAtom, Atom)`.
    fn into_iter(self) -> Self::IntoIter {
        self.into_vec_of_pairs().into_iter()
    }
}

/// Represents a set of [Bindings] instances resulting from an operation where multiple matches are possible.
#[derive(Clone, Debug)]
pub struct BindingsSet(smallvec::SmallVec<[Bindings; 1]>);

// BindingsSets are conceptually unordered
impl PartialEq for BindingsSet {
    fn eq(&self, other: &Self) -> bool {
        match crate::common::assert::vec_eq_no_order(self.iter(), other.iter()) {
            Ok(()) => true,
            Err(_) => false
        }
    }
}

impl FromIterator<Bindings> for BindingsSet {
    fn from_iter<I: IntoIterator<Item=Bindings>>(iter: I) -> Self {
        let new_vec = iter.into_iter().collect();
        BindingsSet(new_vec)
    }
}

impl IntoIterator for BindingsSet {
    type Item = Bindings;
    type IntoIter = smallvec::IntoIter<[Bindings; 1]>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Bindings> for BindingsSet {
    fn from(bindings: Bindings) -> Self {
        BindingsSet(smallvec::smallvec![bindings])
    }
}

impl TryFrom<BindingsSet> for Bindings {
    type Error = &'static str;
    fn try_from(mut set: BindingsSet) -> Result<Self, &'static str> {
        match set.len() {
            0 => Ok(Bindings::new()),
            1 => Ok(set.0.pop().unwrap()),
            _ => Err("Set Contains Multiple Bindings")
        }
    }
}

impl Extend<Bindings> for BindingsSet {
    fn extend<I: IntoIterator<Item=Bindings>>(&mut self, iter: I) {
        self.0.extend(iter);
    }
}

impl core::ops::Deref for BindingsSet {
    type Target = [Bindings];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for BindingsSet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for BindingsSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut iter = self.iter();
        if let Some(first_bindings) = iter.next() {
            write!(f, " {first_bindings}")?;
        }
        for bindings in iter {
            write!(f, ",\n {bindings}")?;
        }
        write!(f, " ]")
    }
}

impl BindingsSet {

    /// Creates a new fully-constrained BindingsSet
    ///
    /// NOTE: This function is useful for making a Bindings Iterator that returns no results,
    /// as you might want for a return from a GroundedAtom match function that matched no atoms.
    /// In other cases, you probably want to use [BinsingsSet::single].
    pub fn empty() -> Self {
        BindingsSet(smallvec::smallvec![])
    }

    /// Creates a new unconstrained BindingsSet
    pub fn single() -> Self {
        BindingsSet(smallvec::smallvec![Bindings::new()])
    }

    /// Returns `true` if a BindingsSet contains no Bindings Objects (fully constrained)
    ///
    /// TODO: Need a better name that doesn't conflict with the intuitions about Bindings::is_empty()
    /// https://github.com/trueagi-io/hyperon-experimental/issues/281
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns `true` if a BindingsSet contains no limiting Bindings inside (unconstrained)
    ///
    /// TODO: Need a better word to describe this concept than "single"
    /// https://github.com/trueagi-io/hyperon-experimental/issues/281
    pub fn is_single(&self) -> bool {
        self.len() == 1 && self.0[0].is_empty()
    }

    pub fn drain<'a, R: std::ops::RangeBounds<usize>>(&'a mut self, range: R) -> impl Iterator<Item=Bindings> +'a {
        self.0.drain(range)
    }

    pub fn push(&mut self, bindings: Bindings) {
        self.0.push(bindings);
    }

    /// An internal function to execute an operation that may take a single Bindings instance and replace
    /// it with a new BindingsSet containing zero or more Bindings instances
    fn perform_one_to_many_op<F>(mut self, mut func: F) -> Self
        where F: FnMut(Bindings) -> BindingsSet
    {
        let mut new_set = BindingsSet::empty();
        for bindings in self.0.drain(..) {
            new_set.extend(func(bindings));
        }
        new_set
    }

    pub fn add_var_equality(self, a: &VariableAtom, b: &VariableAtom) -> Self {
        self.perform_one_to_many_op(|bindings| bindings.add_var_equality_internal(a, b))
    }

    pub fn add_var_binding<T1, T2>(self, var: T1, value: T2) -> Self
        where T1: RefOrMove<VariableAtom>, T2: RefOrMove<Atom>
    {
        self.perform_one_to_many_op(|bindings| bindings.add_var_binding_internal(var.as_ref(), value.as_ref()))
    }

    fn merge_bindings(self, b: &Bindings) -> Self {
        self.perform_one_to_many_op(|bindings| bindings.merge_v2(b))
    }

    /// Merges each bindings from `other` to each bindings from `self`
    ///
    /// NOTE: this subsumes the functionality formerly in `match_result_product`
    pub fn merge(self, other: &BindingsSet) -> Self {
        let mut new_set = BindingsSet::empty();
        for other_binding in other.iter() {
            new_set.extend(self.clone().merge_bindings(other_binding).into_iter());
        }
        new_set
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
//TODO: A situation where a MatchResultIter returns an unbounded (infinite) number of results
// will hang this implementation, on account of `.collect()`
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
    Box::new(match_atoms_recursively(left, right).into_iter()
        .filter(|binding| {
            if binding.has_loops() {
                log::trace!("match_atoms: remove bindings which contains a variable loop: {}", binding);
                false
            } else {
                true
            }
        }))
}

fn match_atoms_recursively(left: &Atom, right: &Atom) -> BindingsSet {
    log::trace!("match_atoms_recursively: {} ~ {}", left, right);

    match (left, right) {
        (Atom::Symbol(a), Atom::Symbol(b)) if a == b => BindingsSet::single(),
        (Atom::Variable(dv), Atom::Variable(pv)) => BindingsSet::single().add_var_equality(dv, pv),
        (Atom::Variable(v), b) => BindingsSet::single().add_var_binding(v, b),
        (a, Atom::Variable(v)) => BindingsSet::single().add_var_binding(v, a),
        (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b }))
        if a.len() == b.len() => {
            a.iter().zip(b.iter()).fold(BindingsSet::single(),
            |acc, (a, b)| {
                acc.merge(&match_atoms_recursively(a, b))
            })
        },
        // TODO: one more case for the special flag to see if GroundedAtom is
        // matchable. If GroundedAtom is matched with VariableAtom there are
        // two way to calculate match: (1) pass variable to the
        // GroundedAtom::match(); (2) assign GroundedAtom to the Variable.
        // Returning both results breaks tests right now.
        (Atom::Grounded(a), _) => {
            a.match_(right).collect()
        },
        (_, Atom::Grounded(b)) => {
            b.match_(left).collect()
        },
        _ => BindingsSet::empty(),
    }
}

//TODO: This function is redundant, as the functionality is subsumed by BindingsSet::merge
/// Merges each bindings from `prev` iter to each bindings from `next`
/// iter. The result is an iter over successfully merged bindings.
pub fn match_result_product(prev: MatchResultIter, next: MatchResultIter) -> MatchResultIter {
    let next: BindingsSet = next.collect();
    let prev: BindingsSet = prev.collect();
    log::trace!("match_result_product_iter, next: {:?}", next);
    Box::new(prev.merge(&next).into_iter())
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
    //TODO: Delete of this function pending refactor of Interpreter
    from.clone().merge_v2(to).into_iter().filter(|bindings| !bindings.has_loops()).next().ok_or(())
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
        assert_eq!(bind!{ a: expr!("A") }.merge_v2(
            &bind!{ a: expr!("C"), b: expr!("B") }), BindingsSet::empty());
        assert_eq!(bind!{ a: expr!("C"), b: expr!("B") }.merge_v2(
            &bind!{ a: expr!("A") }), BindingsSet::empty());
    }

    #[test]
    fn test_bindings_merge() {
        assert_eq!(bind!{ a: expr!("A") }.merge_v2(
            &bind!{ a: expr!("A"), b: expr!("B") }),
            bind_set![{ a: expr!("A"), b: expr!("B") }]);
        assert_eq!(bind!{ a: expr!("A"), b: expr!("B") }.merge_v2(
            &bind!{ a: expr!("A") }),
            bind_set![{ a: expr!("A"), b: expr!("B") }]);
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
            make_variables_unique(expr!(("A" x) ("B" x))),
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
                        Atom::Variable(ref var) => {
                            let bindings = Bindings::new()
                                .add_var_binding_v2(var, expr!({42})).unwrap();
                            Box::new(std::iter::once(bindings))
                        },
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
    fn match_atoms_with_custom_matcher() {
        assert_match(
            expr!( {Rand{}} ),
            expr!(   (x)    ),
            vec![bind!{x: expr!({42})}]);
        assert_match(
            expr!(   (x)    ),
            expr!( {Rand{}} ),
            vec![bind!{x: expr!({42})}]);
    }

    #[derive(PartialEq, Clone, Debug, Copy)]
    struct ReturnPairInX{}

    impl Grounded for ReturnPairInX {
        fn type_(&self) -> Atom {
            Atom::sym("ReturnPairInX")
        }
        fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
            execute_not_executable(self)
        }
        fn match_(&self, _other: &Atom) -> matcher::MatchResultIter {
            let result = vec![ bind!{ x: expr!("B") }, bind!{ x: expr!("C") } ];
            Box::new(result.into_iter())
        }
    }

    impl Display for ReturnPairInX {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "ReturnPairInX")
        }
    }

    #[test]
    fn match_atoms_with_custom_matcher_split_results_by_adding_value() {
        let pair = ReturnPairInX{};

        assert_match(
            expr!( { pair } ("A" x) ),
            expr!(     s        s   ),
            vec![ bind!{ s: expr!({ pair }), x: expr!("B") },
                  bind!{ s: expr!({ pair }), x: expr!("C") } ]);

        assert_match(
            expr!( { pair }    y    y ),
            expr!(     s    ("A" x) s ),
            vec![ bind!{ s: expr!({ pair }), y: expr!("A" x), x: expr!("B") },
                  bind!{ s: expr!({ pair }), y: expr!("A" x), x: expr!("C") } ]);
    }

    #[ignore = "Requires sorting inside Bindings to be stable"]
    #[test]
    fn bindings_match_display() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("b"))?
            .add_var_binding_v2(VariableAtom::new("b"), Atom::sym("v"))?
            .add_var_equality(&VariableAtom::new("c"), &VariableAtom::new("d"))?;

        assert_eq!(bindings.to_string(), "{ $a = $b = v, $c = $d }");
        Ok(())
    }

    #[test]
    fn bindings_get_variable_no_variable() {
        let bindings = Bindings::new();

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
    }

    #[test]
    fn bindings_get_variable_no_value() {
        let mut bindings = Bindings::new();
        bindings.add_var_no_value(&VariableAtom::new("x"));

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), Some(Atom::var("x")));
    }

    #[test]
    fn bindings_get_variable_bound_to_value() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_binding_v2(VariableAtom::new("x"), expr!("A" y))?
            .add_var_binding_v2(VariableAtom::new("y"), expr!("B" z))?;

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), Some(expr!("A" ("B" z))));
        assert_eq!(bindings.resolve(&VariableAtom::new("y")), Some(expr!("B" z)));
        Ok(())
    }

    #[test]
    fn bindings_get_variable_bound_to_value_with_loop() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_binding_v2(VariableAtom::new("x"), expr!("A" y))?
            .add_var_binding_v2(VariableAtom::new("y"), expr!("B" x))?;

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
        assert_eq!(bindings.resolve(&VariableAtom::new("y")), None);
        Ok(())
    }

    #[test]
    fn bindings_get_variable_bound_to_variable() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_binding_v2(VariableAtom::new("x"), expr!(x))?;

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
        Ok(())
    }

    #[test]
    fn bindings_get_variable_equal_to_variable() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("x"), &VariableAtom::new("y"))?;

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), Some(expr!(y)));
        Ok(())
    }

    #[test]
    fn bindings_partial_eq() {
        assert!(bind!{ x: expr!("X"), y: expr!("Y") } != bind!{ y: expr!("Y") });
        assert!(bind!{ y: expr!("Y") } != bind!{ x: expr!("X"), y: expr!("Y") });
        assert!(bind!{ x: expr!(a) } != bind!{ x: expr!(b) });
        assert!(bind!{ x: expr!(y), y: expr!("X") } == bind!{ x: expr!("X"), y: expr!(x) });
    }

    #[test]
    fn bindings_narrow_vars() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_binding_v2(VariableAtom::new("leftA"), expr!("A"))?
            .add_var_equality(&VariableAtom::new("leftA"), &VariableAtom::new("rightB"))?
            .add_var_binding_v2(VariableAtom::new("leftC"), expr!("C"))?
            .add_var_equality(&VariableAtom::new("leftD"), &VariableAtom::new("rightE"))?
            .add_var_binding_v2(VariableAtom::new("rightF"), expr!("F"))?;

        let narrow = bindings.narrow_vars(&HashSet::from([&VariableAtom::new("rightB"),
            &VariableAtom::new("rightE"), &VariableAtom::new("rightF")]));

        assert_eq!(narrow, bind!{ rightB: expr!("A"), rightF: expr!("F"), rightE: expr!(rightE) });
        Ok(())
    }

    #[test]
    fn bindings_narrow_vars_keeps_vars_equality() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("x"), &VariableAtom::new("y"))?
            .add_var_equality(&VariableAtom::new("x"), &VariableAtom::new("z"))?;

        let narrow = bindings.narrow_vars(&HashSet::from([&VariableAtom::new("y"),
            &VariableAtom::new("z")]));

        assert_eq!(narrow, bind!{ y: expr!(z) });
        Ok(())
    }

    #[test]
    fn bindings_convert_var_equalities_to_bindings() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("b"))?
            .add_var_binding_v2(VariableAtom::new("a"), expr!("A"))?;

        let result = bindings.clone().convert_var_equalities_to_bindings(&[VariableAtom::new("a")].into());
        let expected = Bindings::new()
            .add_var_binding_v2(VariableAtom::new("a"), expr!("A"))?
            .add_var_binding_v2(&VariableAtom::new("b"), expr!(a))?;
        assert_eq!(result, expected);

        let result = bindings.clone().convert_var_equalities_to_bindings(&[VariableAtom::new("b")].into());
        let expected = Bindings::new()
            .add_var_binding_v2(VariableAtom::new("b"), expr!("A"))?
            .add_var_binding_v2(&VariableAtom::new("a"), expr!(b))?;
        assert_eq!(result, expected);

        Ok(())
    }

    #[test]
    fn bindings_add_var_value_splits_bindings() {
        let pair = ReturnPairInX{};

        // ({ x -> B, x -> C } (A $x)) ~ ($s $s)
        let bindings = BindingsSet::single()
            .add_var_binding(VariableAtom::new("s"), expr!({ pair }))
            .add_var_binding(VariableAtom::new("s"), expr!("A" x));

        // Bindings::add_var_binding() should return a list of resulting
        // Bindings instances.
        assert_eq!(bindings, bind_set![
            bind!{ s: expr!({ pair }), x: expr!("B") },
            bind!{ s: expr!({ pair }), x: expr!("C") } ]);
    }

    #[test]
    fn bindings_add_var_equality_splits_bindings() {
        let pair = ReturnPairInX{};

        // ({ x -> B, x -> C } $y $y) ~ ($s (A $x) $s)
        let bindings = BindingsSet::single()
            .add_var_binding(VariableAtom::new("s"), expr!({ pair }))
            .add_var_binding(VariableAtom::new("y"), expr!("A" x))
            .add_var_equality(&VariableAtom::new("y"), &VariableAtom::new("s"));

        // Bindings::add_var_binding() should return a list of resulting
        // Bindings instances.
        assert_eq!(bindings, bind_set![
            bind!{ s: expr!({ pair }), y: expr!("A" x), x: expr!("B") },
            bind!{ s: expr!({ pair }), y: expr!("A" x), x: expr!("C") } ]);
    }

    #[test]
    fn bindings_merge_custom_matching() {

        /// Assigner matches the expression atoms in the following form (<variable> <value>...)
        /// and returns the list of the bindings (one per each value) which assign
        /// values to the variable. For example being matched with `($x A B)` atom it
        /// returns `[ { $x = A }, { $x = B } ]`. This grounded atom is implemented for
        /// testing purposes.
        #[derive(PartialEq, Clone, Debug, Copy)]
        struct Assigner{}

        impl Grounded for Assigner {
            fn type_(&self) -> Atom {
                Atom::sym("Assigner")
            }
            fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
                execute_not_executable(self)
            }
            fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
                match other.iter().collect::<Vec<&Atom>>().as_slice() {
                    [ Atom::Variable(var), values @ .. ] => {
                        let result: Vec<Bindings> = values.into_iter()
                            .map(|&val| { Bindings::from(&[(var.clone(), val.clone())][..]) })
                            .collect();
                        Box::new(result.into_iter())
                    },
                    _ => panic!("Assigner expects (<variable> <values>...) atom as a query"),
                }
            }
        }

        impl Display for Assigner {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "Assigner")
            }
        }

        let assigner = Assigner{};

        let a = bind!{ a: expr!({ assigner }), b: expr!({ assigner }) };
        let b = bind!{ a: expr!(x "C" "D"), b: expr!(y "E" "F") };

        let bindings = a.merge_v2(&b);

        assert_eq!(bindings, bind_set![
            bind!{ a: expr!({ assigner }), b: expr!({ assigner }), x: expr!("C"), y: expr!("E") },
            bind!{ a: expr!({ assigner }), b: expr!({ assigner }), x: expr!("C"), y: expr!("F") },
            bind!{ a: expr!({ assigner }), b: expr!({ assigner }), x: expr!("D"), y: expr!("E") },
            bind!{ a: expr!({ assigner }), b: expr!({ assigner }), x: expr!("D"), y: expr!("F") },
        ]);
    }

    #[test]
    fn bindings_cleanup() -> Result<(), &'static str> {
        let mut bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("b"))?
            .add_var_binding_v2(VariableAtom::new("b"), expr!("B"))?
            .add_var_binding_v2(VariableAtom::new("c"), expr!("C"))?;
        bindings.cleanup(&[&VariableAtom::new("b")].into());
        assert_eq!(bindings, bind!{ b: expr!("B") });
        Ok(())
    }

    #[ignore = "Unstable because HashMap doesn't guarantee the order of entries"]
    #[test]
    fn bindings_into_entries() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("x"), &VariableAtom::new("y"))?
            .add_var_binding_v2(VariableAtom::new("x"), Atom::sym("Z"))?
            .add_var_binding_v2(VariableAtom::new("a"), Atom::expr([Atom::sym("A"), Atom::var("x")]))?
            .add_var_binding_v2(VariableAtom::new("b"), Atom::expr([Atom::sym("B"), Atom::var("x")]))?;

        let entries: Vec<(VariableAtom, Atom)> = bindings.into_iter().collect();

        assert_eq_no_order!(entries, vec![
            (VariableAtom::new("x"), Atom::var("y")),
            (VariableAtom::new("y"), Atom::sym("Z")),
            (VariableAtom::new("a"), Atom::expr([ Atom::sym("A"), Atom::var("x") ])),
            (VariableAtom::new("b"), Atom::expr([ Atom::sym("B"), Atom::var("x") ])),
        ]);
        Ok(())
    }

    #[test]
    fn bindings_rename_vars() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("x"), &VariableAtom::new("y"))?
            .add_var_binding_v2(VariableAtom::new("x"), Atom::sym("Z"))?
            .add_var_binding_v2(VariableAtom::new("a"), Atom::expr([Atom::sym("A"), Atom::var("x")]))?
            .add_var_binding_v2(VariableAtom::new("b"), Atom::expr([Atom::sym("B"), Atom::var("x")]))?;

        let map: HashMap<VariableAtom, VariableAtom> = vec![
            (VariableAtom::new("x"), VariableAtom::new("z"))
        ].into_iter().collect();
        let renamed = bindings.rename_vars(|v| {
            match map.get(&v) {
                Some(v) => v.clone(),
                None => v,
            }
        });

        let expected = Bindings::new()
            .add_var_equality(&VariableAtom::new("z"), &VariableAtom::new("y"))?
            .add_var_binding_v2(VariableAtom::new("z"), Atom::sym("Z"))?
            .add_var_binding_v2(VariableAtom::new("a"), Atom::expr([Atom::sym("A"), Atom::var("z")]))?
            .add_var_binding_v2(VariableAtom::new("b"), Atom::expr([Atom::sym("B"), Atom::var("z")]))?;
        assert_eq!(renamed, expected);
        Ok(())
    }
}
