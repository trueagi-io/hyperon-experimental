//! Module contains functions to match atoms and work with variable bindings.

/// Constructs new instance of the [Bindings] with predefined content.
/// Macros takes variable/value pairs as arguments. If value is a single
/// variable then the pair means variable equality. Otherwise pair means
/// assigning value. May be ineffective, should be used mainly in unit tests.
///
/// # Examples
///
/// ```
/// use hyperon_atom::*;
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
        $crate::matcher::Bindings::from( vec![$( ($crate::VariableAtom::new(stringify!($k)), $v), )*])
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
/// use hyperon_atom::*;
///
/// // Compose a BindingsSet with explicitly defined Bindings
/// let set = bind_set![bind!{a: expr!("A")}, bind!{a: expr!("APrime")}];
///
/// assert_eq!(set.len(), 2);
/// assert_eq!(set[0].resolve(&VariableAtom::new("a")), Some(expr!("A")));
/// assert_eq!(set[1].resolve(&VariableAtom::new("a")), Some(expr!("APrime")));
/// ```
#[macro_export]
macro_rules! bind_set {
    // An empty BindingsSet
    [] => {
        $crate::matcher::BindingsSet::empty()
    };
    // A single immediately defined Bindings
    [{$($b:tt)*}] => {
        $crate::matcher::BindingsSet::from($crate::bind!{$($b)*})
    };
    // A single reduced Bindings
    [$b:expr] => {
        $crate::matcher::BindingsSet::from($b)
    };
    // Recursive pattern to handle multiple Bindings, where each item is reduced
    [$b:expr, $($b_rest:tt)*] => {{
        let mut b = bind_set![$b];
        b.extend(bind_set![$($b_rest)*]);
        b
    }};
}

use std::collections::{HashMap, HashSet};

use super::*;
use hyperon_common::reformove::RefOrMove;
use hyperon_common::holeyvec::HoleyVec;

enum VarResolutionResult<T> {
    Some(T),
    Loop,
    None
}

/// Abstraction of the variable set. It is used to allow passing both
/// `HashSet<&VariableAtom>` and `HashSet<VariableAtom>` to the
/// [Bindings::narrow_vars] method.
pub trait VariableSet : Debug {
    type Iter<'a> : Iterator<Item = &'a VariableAtom> where Self: 'a;

    /// Returns true if var is a part of the set.
    fn contains(&self, var: &VariableAtom) -> bool;

    /// Iterate trough a list of variables in the set.
    fn iter(&self) -> Self::Iter<'_>;
}

impl VariableSet for HashSet<&VariableAtom> {
    type Iter<'a> = std::iter::Map<
        std::collections::hash_set::Iter<'a, &'a VariableAtom>,
        fn(&'a &VariableAtom) -> &'a VariableAtom> where Self: 'a;

    fn contains(&self, var: &VariableAtom) -> bool {
        HashSet::contains(self, var)
    }
    fn iter(&self) -> Self::Iter<'_> {
        HashSet::iter(self).map(|a| *a)
    }
}

impl VariableSet for HashSet<VariableAtom> {
    type Iter<'a> = std::collections::hash_set::Iter<'a, VariableAtom> where Self: 'a;

    fn contains(&self, var: &VariableAtom) -> bool {
        HashSet::contains(self, var)
    }
    fn iter(&self) -> Self::Iter<'_> {
        HashSet::iter(self)
    }
}

#[derive(Clone, Debug)]
struct Binding {
    id: usize,
    count: usize,
    var: VariableAtom,
    atom: Option<Atom>,
}

impl Binding {
    fn no_vars(&self) -> bool {
        self.count == 0
    }
    fn dec_count(&mut self) {
        self.count = self.count - 1;
    }
    fn inc_count(&mut self) {
        self.count = self.count + 1;
    }
}

// TODO: rename Bindings to Substitution which is more common term
/// Represents variable bindings. Keeps two kinds of relations inside:
/// variables equalities and variable value assignments. For example this
/// structure is able to precisely represent result of matching atoms like
/// `($a A C)` and `($x $x $y)`. The result is `{ $a = $x = A, $y = C }`.
/// [Bindings] contains variables from both sides of the match.
#[derive(Clone)]
pub struct Bindings {
    binding_by_var: HashMap<VariableAtom, usize>,
    bindings: HoleyVec<Binding>,
}

impl Bindings {

    /// Constructs new empty instance of [Bindings].
    pub fn new() -> Self {
        Self {
            binding_by_var: HashMap::new(),
            bindings: HoleyVec::new(),
        }
    }

    fn new_binding(&mut self, var: VariableAtom, atom: Option<Atom>) -> usize {
        let id = self.bindings.next_index();
        self.bindings.push(Binding{ id, count: 1, var: var.clone(), atom });
        self.binding_by_var.insert(var, id);
        id
    }

    fn get_binding(&self, var: &VariableAtom) -> Option<&Binding> {
        self.binding_by_var.get(var).map_or(None, |&binding|
            self.bindings.get(binding))
    }

    fn add_var_to_binding(&mut self, binding_id: usize, var: VariableAtom) {
        let binding = &mut self.bindings[binding_id];
        binding.inc_count();
        self.binding_by_var.insert(var, binding_id);
    }

    fn remove_var_from_binding(&mut self, var: &VariableAtom) -> Option<Atom> {
        let binding_id = self.binding_by_var.remove(var);
        match binding_id {
            Some(binding_id) => {
                let binding = &mut self.bindings[binding_id];
                binding.dec_count();
                if binding.no_vars() {
                    self.bindings.remove(binding_id).atom
                } else {
                    if binding.var == *var {
                        let _ = self.rename_binding(binding_id);
                    }
                    None
                }
            },
            None => None,
        }
    }

    fn rename_binding(&mut self, binding_id: usize) -> Result<(), ()> {
        let binding = &mut self.bindings[binding_id];
        let var = self.binding_by_var.iter()
            .filter(|(v, i)| **i == binding.id && **v != binding.var)
            .map(|(v, _i)| v)
            .next()
            .ok_or(())?;
        binding.var = var.clone();
        Ok(())
    }

    fn move_binding_to_binding(&mut self, from_binding_id: usize, to_binding_id: usize) {
        let to_binding = &mut self.bindings[to_binding_id];
        self.binding_by_var.iter_mut().for_each(|(_var, id)| {
            if *id == from_binding_id {
                *id = to_binding_id;
                to_binding.inc_count();
            }
        });
        self.bindings.remove(from_binding_id);
    }

    pub fn len(&self) -> usize {
        self.binding_by_var.len()
    }

    /// Returns true if bindings doesn't contain any variable.
    pub fn is_empty(&self) -> bool {
        self.binding_by_var.is_empty()
    }

    /// Returns value of the variable with all sub-variables resolved using the
    /// same binding. Returns `None` if variable doesn't have a value assigned
    /// or cannot be resolved because of the variable loop.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon_atom::*;
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
        match self.resolve_internal(var, used_vars) {
            VarResolutionResult::Some(atom) => Some(atom),
            VarResolutionResult::Loop => None,
            VarResolutionResult::None => None,
        }
    }

    fn resolve_internal<'a, 'b: 'a>(&'b self, var: &'a VariableAtom, used_vars: HashSet<&'a VariableAtom>) -> VarResolutionResult<Atom> {
        self.get_binding(&var).map_or(VarResolutionResult::None, |binding| {
            match &binding.atom {
                Some(atom) =>
                    self.resolve_vars_in_atom(atom.clone(), used_vars),
                None =>
                    VarResolutionResult::Some(Atom::Variable(binding.var.clone())),
            }
        })
    }

    fn resolve_vars_in_atom<'a>(&'a self, mut atom: Atom, used_vars: HashSet<&'a VariableAtom>) -> VarResolutionResult<Atom> {
        for i in atom.iter_mut() {
            match i {
                Atom::Variable(var) if used_vars.contains(var) => return VarResolutionResult::Loop,
                Atom::Variable(var) => {
                    let mut used_vars = used_vars.clone();
                    used_vars.insert(var);
                    match self.resolve_internal(var, used_vars) {
                        VarResolutionResult::Some(atom) => { *i = atom },
                        VarResolutionResult::Loop => return VarResolutionResult::Loop,
                        VarResolutionResult::None => {},
                    }
                },
                _ => {},
            }
        }
        VarResolutionResult::Some(atom)
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
        let result = match (self.binding_by_var.get(a), self.binding_by_var.get(b)) {
            (Some(&a_binding_id), Some(&b_binding_id))  =>
                if a_binding_id == b_binding_id {
                    BindingsSet::from(self)
                } else {
                    self.merge_bindings(a_binding_id, b_binding_id)
                }
            (Some(&binding_id), None) => {
                self.add_var_to_binding(binding_id, b.clone());
                BindingsSet::from(self)
            },
            (None, Some(&binding_id)) => {
                self.add_var_to_binding(binding_id, a.clone());
                BindingsSet::from(self)
            },
            (None, None) => {
                let binding_id = self.new_binding(a.clone(), None);
                self.add_var_to_binding(binding_id, b.clone());
                BindingsSet::from(self)
            },
        };
        log::trace!("Bindings::add_var_equality: {} = {}, result: {:?}", a, b, result);
        result
    }

    fn match_values(&self, current: &Atom, value: &Atom) -> BindingsSet {
        match_atoms_recursively(current, value).into_iter()
            .flat_map(|binding| binding.merge(self))
            .collect()
    }

    /// Internal function used by the [Bindings::add_var_equality] implementation
    fn merge_bindings(mut self, a_binding_id: usize, b_binding_id: usize) -> BindingsSet {
        let a_binding = &self.bindings[a_binding_id];
        let b_binding = &self.bindings[b_binding_id];
        match (&a_binding.atom, &b_binding.atom) {
            (Some(a_atom), Some(b_atom)) => {
                if a_atom == b_atom {
                    let a_binding = a_binding.id;
                    let b_binding = self.bindings.remove(b_binding.id);
                    self.add_var_to_binding(a_binding, b_binding.var);
                    BindingsSet::from(self)
                } else {
                    self.match_values(&a_atom, &b_atom)
                }
            },
            (None, Some(_)) => {
                self.move_binding_to_binding(a_binding_id, b_binding_id);
                BindingsSet::from(self)
            }
            _ => {
                self.move_binding_to_binding(b_binding_id, a_binding_id);
                BindingsSet::from(self)
            },
        }
    }

    /// Tries to insert `value` as a binding for the `var`. If `self` already
    /// has binding for the `var` and it is not matchable with the `value` then
    /// function returns Err. Otherwise it returns updated Bindings.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon_atom::*;
    /// use hyperon_atom::matcher::Bindings;
    ///
    /// # fn main() -> Result<(), &'static str> {
    /// let a = VariableAtom::new("a");
    /// let b = VariableAtom::new("b");
    /// let c = VariableAtom::new("c");
    /// let mut binds = bind!{ a: expr!("A"), b: expr!("B") };
    ///
    /// // Re-asserting an existing binding is ok
    /// binds = binds.add_var_binding(&a, &expr!("A"))?;
    ///
    /// // Asserting a conflicting binding is an error
    /// assert!(binds.clone().add_var_binding(&b, &expr!("C")).is_err());
    ///
    /// // Creating a new binding is ok
    /// binds = binds.add_var_binding(&c, &expr!("C"))?;
    ///
    /// assert_eq!(binds.resolve(&a), Some(expr!("A")));
    /// assert_eq!(binds.resolve(&b), Some(expr!("B")));
    /// assert_eq!(binds.resolve(&c), Some(expr!("C")));
    /// # Ok(())
    /// # }
    /// ```
    pub fn add_var_binding<T1, T2>(self, var: T1, value: T2) -> Result<Bindings, &'static str>
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
        let trace_parameters = match log::log_enabled!(log::Level::Trace) {
            true => Some(format!("{} <- {}", var.as_ref(), value.as_ref())),
            false => None,
        };
        let result = match self.binding_by_var.get(var.as_ref()) {
            Some(&binding_id) => {
                let binding = &self.bindings[binding_id];
                match binding.atom {
                    Some(ref current) => {
                        if current == value.as_ref() {
                            BindingsSet::from(self)
                        } else {
                            self.match_values(current, value.as_ref())
                        }
                    },
                    None => {
                        let binding = &mut self.bindings[binding_id];
                        binding.atom = Some(value.as_value());
                        BindingsSet::from(self)
                    },
                }
            },
            None => {
                self.new_binding(var.as_value(), Some(value.as_value()));
                BindingsSet::from(self)
            },
        };
        if let Some(trace_parameters) = trace_parameters {
            log::trace!("Bindings::add_var_bindings: {}, result: {:?}", trace_parameters, result);
        }
        result
    }

    /// Merges `b` bindings into self if they are compatible.  May return a [BindingsSet] containing
    /// multiple [Bindings] if appropriate.  If no compatible bindings can be merged, [BindingsSet::empty()]
    /// will be returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon_atom::*;
    /// use hyperon_atom::matcher::{Bindings, BindingsSet};
    ///
    /// let mut binds = bind!{ a: expr!("A") };
    /// let mut comp = bind!{ b: expr!("B") };
    /// let mut incomp = bind!{ a: expr!("B") };
    ///
    /// assert_eq!(binds.clone().merge(&comp), BindingsSet::from(bind!{ a: expr!("A"), b: expr!("B") }));
    /// assert_eq!(binds.merge(&incomp), BindingsSet::empty());
    /// ```
    pub fn merge(self, other: &Bindings) -> BindingsSet {
        log::trace!("Bindings::merge: {} ^ {}", self, other);
        let trace_self = match log::log_enabled!(log::Level::Trace) {
            true => Some(self.clone()),
            false => None
        };

        if self.is_empty() {
            return other.clone().into()
        } else if other.is_empty() {
            return self.into()
        }

        let (results, _) = other.binding_by_var.iter().fold((smallvec::smallvec![self], HashMap::new()),
            |(results, mut other_vars_merged), (var, binding_id)| -> (smallvec::SmallVec<[Bindings; 1]>, HashMap<usize, &VariableAtom>) {
                let mut all_results = smallvec::smallvec![];
                log::trace!("next_var: var: {}, binding_id: {}", var, binding_id);

                if let Some(first_binding) = other_vars_merged.get(binding_id) {
                    all_results.extend(results.into_iter()
                        .flat_map(|r| r.add_var_equality_internal(first_binding, var)));
                } else {
                    let binding = other.get_binding(var).expect("Unexpected state");
                    if let Some(atom) = &binding.atom {
                        all_results.extend(results.into_iter()
                            .flat_map(|r| r.add_var_binding_internal(var, atom)));
                    } else {
                        all_results = results;
                    }
                }

                log::trace!("all_results: {:?}", all_results);
                other_vars_merged.insert(*binding_id, var);
                (all_results, other_vars_merged)
            });

        if let Some(self_copy) = trace_self {
            log::trace!("Bindings::merge: {} ^ {} -> {:?}", self_copy, other, results);
        }
        BindingsSet(results)
    }

    fn find_deps<'a>(&'a self, var: &'a VariableAtom, deps: &mut HashSet<&'a VariableAtom>) {
        if !deps.contains(var) {
            deps.insert(var);
            match self.get_binding(var).and_then(|b| b.atom.as_ref()) {
                Some(atom) => {
                    atom.iter().filter_type::<&VariableAtom>()
                        .for_each(|var| { self.find_deps(var, deps); });
                },
                _ => {},
            }
        }
    }

    /// Get narrow bindings which contains only passed set of variables and
    /// their dependencies.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon_atom::*;
    /// use std::collections::HashSet;
    ///
    /// let bindings = bind!{ leftA: expr!("A"), leftA: expr!(rightB),
    ///     leftC: expr!("C"), leftD: expr!(rightE), rightF: expr!("F") };
    /// let right = bindings.narrow_vars(&HashSet::from([&VariableAtom::new("rightB"),
    ///     &VariableAtom::new("rightE"), &VariableAtom::new("rightF")]));
    ///
    /// assert_eq!(right, bind!{ rightB: expr!("A"), rightF: expr!("F"), rightE: expr!(rightE) });
    /// ```
    pub fn narrow_vars<T: VariableSet>(&self, vars: &T) -> Bindings {
        // TODO: can we use binding deps instead of var deps?
        let mut deps = HashSet::new();
        for var in vars.iter() {
            self.find_deps(var, &mut deps);
        }

        let mut bindings = Bindings::new();
        let mut prev_to_new = vec![usize::MAX; self.bindings.index_upper_bound()];
        let mut copy_var = |var| {
            match self.get_binding(var) {
                None => {},
                Some(prev @ binding) => {
                    let new_id = prev_to_new[binding.id];
                    if new_id != usize::MAX {
                        bindings.add_var_to_binding(new_id, (*var).clone());
                    } else {
                        let new_id = bindings.new_binding(var.clone(), binding.atom.clone());
                        prev_to_new[prev.id] = new_id;
                    }
                },
            }
        };

        for var in vars.iter() {
            copy_var(var);
        }
        for var in deps.iter().filter(|v| !vars.contains(v)) {
            copy_var(var);
        }

        log::trace!("Bindings::narrow_vars: vars: {:?}, {} -> {}", vars, self, bindings);
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
        let mut renamed = bitset::BitSet::with_capacity(self.bindings.index_upper_bound());
        for var in preferred_vars {
            match self.binding_by_var.get(&var) {
                Some(&binding_id) => {
                    if !renamed.test(binding_id) {
                        self.bindings[binding_id].var = var.clone();
                        renamed.set(binding_id, true);
                    }
                },
                None => {},
            }
        }
        for (var, binding_id) in &mut self.binding_by_var {
            let binding = &self.bindings[*binding_id];
            if binding.var != *var {
                let var_atom = Atom::Variable(binding.var.clone());
                let new_binding_id = self.bindings.next_index();
                self.bindings.push(Binding{ id: new_binding_id, count: 1, var: var.clone(), atom: Some(var_atom) });
                *binding_id = new_binding_id;
            }
        }
        if let Some(self_copy) = trace_self {
            log::trace!("Bindings::convert_var_equalities_to_bindings: preferred_vars: {:?}, {} -> {}", preferred_vars, self_copy, self);
        }
        self
    }

    pub fn has_loops(&self) -> bool {
        for binding in &self.bindings {
            let mut used_bindings = bitset::BitSet::with_capacity(self.bindings.index_upper_bound());
            used_bindings.set(binding.id, true);
            if self.binding_has_loops(&binding, &mut used_bindings) {
                return true;
            }
        }
        false
    }

    fn binding_has_loops(&self, binding: &Binding, used_bindings: &mut bitset::BitSet) -> bool {
        match &binding.atom {
            None => false,
            Some(atom) => {
                for var in atom.iter().filter_type::<&VariableAtom>() {
                    match self.get_binding(var) {
                        Some(binding) => {
                            if used_bindings.test(binding.id) {
                                return true;
                            }
                            used_bindings.set(binding.id, true);
                            if self.binding_has_loops(binding, used_bindings) {
                                return true;
                            }
                            used_bindings.set(binding.id, false);
                        },
                        None => {},
                    }
                }
                false
            },
        }
    }

    /// Returns iterator of `(&VariableAtom, Atom)` pairs to represent [Bindings] in C API.
    /// Each pair contains reference to a [VariableAtom] and instance of [Atom]
    /// which contains resolved value of the variable. See [Bindings::resolve].
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon_common::assert_eq_no_order;
    /// use hyperon_atom::*;
    ///
    /// let bindings = bind!{ leftA: expr!("A"), leftA: expr!(rightB),
    ///     leftC: expr!("C"), leftD: expr!(rightE), rightF: expr!("F") };
    /// let pairs: Vec<(&VariableAtom, Atom)> = bindings.iter().collect();
    ///
    /// assert_eq_no_order!(pairs, vec![
    ///     (&VariableAtom::new("leftA"), expr!("A")),
    ///     (&VariableAtom::new("rightB"), expr!("A")),
    ///     (&VariableAtom::new("leftC"), expr!("C")),
    ///     (&VariableAtom::new("leftD"), expr!(leftD)),
    ///     (&VariableAtom::new("rightE"), expr!(leftD)),
    ///     (&VariableAtom::new("rightF"), expr!("F")),
    /// ]);
    /// ```
    pub fn iter(&self) -> BindingsIter<'_> {
        BindingsIter { bindings: self, delegate: self.binding_by_var.iter() }
    }

    /// An iterator visiting all variables in arbitrary order.
    pub fn vars(&self) -> impl Iterator<Item=&VariableAtom> {
        self.binding_by_var.keys()
    }

    fn into_vec_of_pairs(mut self) -> Vec<(VariableAtom, Atom)> {
        let mut result = Vec::new();

        for binding in &mut self.bindings {
            match &binding.atom {
                Some(atom) => {
                    result.push((binding.var.clone(), atom.clone()));
                    binding.atom = None;
                },
                None => {},
            }
        }
        for (var, binding_id) in self.binding_by_var {
            let binding = &self.bindings[binding_id];
            if binding.var != var {
                result.push((var, Atom::Variable(binding.var.clone())));
            }
        }

        result
    }

    /// Rename variables inside bindings using `rename`.
    pub fn rename_vars<F>(self, mut rename: F) -> Self where F: FnMut(VariableAtom) -> VariableAtom {
        self.into_vec_of_pairs().into_iter()
            .map(|(mut v, mut a)| {
                v = rename(v);
                a.iter_mut().filter_type::<&mut VariableAtom>()
                    .for_each(|var| *var = rename(var.clone()));
                (v, a)
            })
            .collect::<Vec<(VariableAtom, Atom)>>()
            .into()
    }

    pub fn apply_and_retain<F>(&mut self, atom: &mut Atom, f: F) where F: Fn(&VariableAtom) -> bool {
        let trace_data = match log::log_enabled!(log::Level::Trace) {
            true => Some((self.clone(), atom.clone())),
            false => None
        };
        let to_remove: HashSet<VariableAtom> = self.binding_by_var.keys()
            .filter_map(|var| {
                if !f(var) {
                    Some(var.clone())
                } else {
                    None
                }
            }).collect();

        atom.iter_mut().for_each(|atom| match atom {
            Atom::Variable(var) => {
                if to_remove.contains(&var) {
                    match self.resolve(var) {
                        Some(mut value) => {
                            value.iter_mut().for_each(|atom| match atom {
                                Atom::Variable(var) if to_remove.contains(&var) => {
                                    self.rename_var(var, &to_remove).map(|var| {
                                        *atom = var;
                                    });
                                },
                                _ => {},
                            });
                            *atom = value;
                        },
                        None => {},
                    }
                }
            },
            _ => {},
        });
        let mut removed = Bindings::new();
        for var in to_remove {
            let atom = self.remove_var_from_binding(&var);
            if let Some(atom) = atom {
                removed = removed.add_var_binding(var, atom).unwrap();
            }
        }
        for binding in &mut self.bindings {
            match &mut binding.atom {
                Some(atom) => apply_bindings_to_atom_mut(atom, &removed),
                None => {},
            }
        }

        if let Some((trace_bindings, trace_atom)) = trace_data {
            log::trace!("Bindings::apply_and_retain: atom: {} -> {}", trace_atom, atom);
            log::trace!("Bindings::apply_and_retain: bindings: {:?} / {:?} -> {:?}", trace_bindings, removed, self);
        }
    }

    fn rename_var(&self, var: &VariableAtom, to_remove: &HashSet<VariableAtom>) -> Option<Atom> {
        let renamed = match self.get_binding(var) {
            None => None,
            Some(binding) =>
                match self.binding_by_var.iter().filter(|(v, &b)| b == binding.id && !to_remove.contains(*v)).next() {
                    None => None,
                    Some((v, _)) => Some(Atom::Variable(v.clone())),
                },
        };
        log::trace!("Bindings::rename_var: {} -> {:?}", var, renamed);
        renamed
    }
}

impl Display for Bindings {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut vars_by_binding_id = vec![HashSet::new(); self.bindings.index_upper_bound()];
        for (var, &binding_id) in &self.binding_by_var {
            if *var != self.bindings[binding_id].var {
                vars_by_binding_id[binding_id].insert(var);
            }
        }

        write!(f, "{{ ")?;
        let mut first = true;
        for (binding_id, vars) in vars_by_binding_id.iter().enumerate() {
            if let Some(binding) = self.bindings.get(binding_id) {
                let prefix = if first { first = false; "" } else { ", " };
                write!(f, "{}", prefix)?;
                write!(f, "{}", binding.var)?;
                for var in vars {
                    write!(f, " = {}", var)?;
                }
                match &binding.atom {
                    Some(atom) => write!(f, " <- {}", atom)?,
                    None => {},
                }
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

    /// This implementation is for testing only. It doesn't take into account
    /// names of the renamed variables (see `Binding::var`).
    fn eq(&self, other: &Self) -> bool {
        for (var, self_binding_id) in &self.binding_by_var {
            match other.binding_by_var.get(var) {
                None => return false, // no such name in other
                Some(other_binding_id) => {
                    let self_binding = &self.bindings[*self_binding_id];
                    let other_binding = &other.bindings[*other_binding_id];

                    if  self_binding.atom != other_binding.atom {
                        return false; // values are not equal
                    }
                }
            }
        }

        for (var, _) in &other.binding_by_var {
            match self.binding_by_var.get(var) {
                None => return false, // no such name in self
                Some(_) => {},
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
                _ => bindings.add_var_binding(var, val),
            }.unwrap_or_else(|e| panic!("Error creating Bindings from Atoms: {}", e));
        }
        bindings
    }
}

/// Iterator over `(&VariableAtom, Atom)` pairs in [Bindings].
/// Each pair contains reference to a [VariableAtom] and instance of [Atom]
/// which contains resolved value of the variable. See [Bindings::resolve].
pub struct BindingsIter<'a> {
    bindings: &'a Bindings,
    delegate: std::collections::hash_map::Iter<'a, VariableAtom, usize>,
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


/// Represents a set of [Bindings] instances resulting from an operation where multiple matches are possible.
#[derive(Clone, Debug)]
pub struct BindingsSet(smallvec::SmallVec<[Bindings; 1]>);

// BindingsSets are conceptually unordered
impl PartialEq for BindingsSet {
    fn eq(&self, other: &Self) -> bool {
        !hyperon_common::assert::compare_vec_no_order(self.iter(), other.iter(), hyperon_common::collections::DefaultEquality{}).has_diff()
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
    /// In other cases, you probably want to use [Self::single].
    pub fn empty() -> Self {
        BindingsSet(smallvec::smallvec![])
    }

    /// Creates a new BindingsSet with a single full match
    pub fn single() -> Self {
        BindingsSet(smallvec::smallvec![Bindings::new()])
    }
    
    /// Creates a new BindingsSet with `count` full matches
    pub fn count(count: usize) -> Self {
        BindingsSet(smallvec::SmallVec::from_elem(Bindings::new(), count))
    }

    /// Returns `true` if a BindingsSet contains no Bindings Objects (fully constrained)
    ///
    /// TODO: Need a better name that doesn't conflict with the intuitions about Bindings::is_empty()
    /// [issue#281](https://github.com/trueagi-io/hyperon-experimental/issues/281)
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns `true` if a BindingsSet contains no limiting Bindings inside (unconstrained)
    ///
    /// TODO: Need a better word to describe this concept than "single"
    /// [issue#281](https://github.com/trueagi-io/hyperon-experimental/issues/281)
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
        self.perform_one_to_many_op(|bindings| bindings.merge(b))
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

/// Iterator over atom matching results. Each result is an instance of [Bindings].
//TODO: A situation where a MatchResultIter returns an unbounded (infinite) number of results
// will hang this implementation, on account of `.collect()`
pub type MatchResultIter = BoxedIter<'static, Bindings>;

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
/// use hyperon_atom::*;
/// use hyperon_atom::matcher::*;
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
    let res = match (left, right) {
        (Atom::Symbol(a), Atom::Symbol(b)) if a == b => BindingsSet::single(),
        (Atom::Variable(dv), Atom::Variable(pv)) => {
            let mut bind = Bindings::new();
            let binding_id = bind.new_binding(dv.clone(), None);
            bind.add_var_to_binding(binding_id, pv.clone());
            BindingsSet::from(bind)
        },
        // TODO: If GroundedAtom is matched with VariableAtom there are
        // two way to calculate match: (1) pass variable to the
        // GroundedAtom::match(); (2) assign GroundedAtom to the Variable.
        // Returning both results breaks tests right now.
        (Atom::Variable(v), b) => {
            let mut bind = Bindings::new();
            bind.new_binding(v.clone(), Some(b.clone()));
            BindingsSet::from(bind)
        }, 
        (a, Atom::Variable(v)) => {
            let mut bind = Bindings::new();
            bind.new_binding(v.clone(), Some(a.clone()));
            BindingsSet::from(bind)
        }, 
        (Atom::Expression(ExpressionAtom{ children: a, ..  }), Atom::Expression(ExpressionAtom{ children: b, .. }))
        if a.len() == b.len() => {
            a.iter().zip(b.iter()).fold(BindingsSet::single(),
            |acc, (a, b)| {
                acc.merge(&match_atoms_recursively(a, b))
            })
        },
        (Atom::Grounded(a), _) if a.as_grounded().as_match().is_some() => {
            a.as_grounded().as_match().unwrap().match_(right).collect()
        },
        (_, Atom::Grounded(b)) if b.as_grounded().as_match().is_some() => {
            b.as_grounded().as_match().unwrap().match_(left).collect()
        },
        (Atom::Grounded(a), Atom::Grounded(b)) if a.eq_gnd(AsRef::as_ref(b)) => BindingsSet::single(),
        _ => BindingsSet::empty(),
    };
    log::trace!("match_atoms_recursively: {} ~ {} => {}", left, right, res);
    res
}

/// Applies bindings to atom and return it (see [apply_bindings_to_atom_mut]).
#[inline]
pub fn apply_bindings_to_atom_move(mut atom: Atom, bindings: &Bindings) -> Atom {
    apply_bindings_to_atom_mut(&mut atom, bindings);
    atom
}

/// Applies bindings to atom. Function replaces all variables in atom by
/// corresponding bindings.
///
/// # Examples
///
/// ```
/// use hyperon_atom::*;
/// use hyperon_atom::matcher::apply_bindings_to_atom_mut;
///
/// let binds = bind!{ y: expr!("Y") };
/// let mut atom = expr!("+" "X" y);
/// apply_bindings_to_atom_mut(&mut atom, &binds);
///
/// assert_eq!(atom, expr!("+" "X" "Y"));
/// ```
pub fn apply_bindings_to_atom_mut(atom: &mut Atom, bindings: &Bindings) {
    let trace_atom = match log::log_enabled!(log::Level::Trace) {
        true => Some(atom.clone()),
        false => None,
    };
    let mut updated = false;
    if !bindings.is_empty() {
        atom.iter_mut().for_each(|atom| match atom {
            Atom::Variable(var) => {
                bindings.resolve(var).map(|value| {
                    *atom = value;
                    updated = true;
                });
            },
            _ => {},
        });
    }
    if updated {
        match atom {
            Atom::Expression(e) => e.evaluated = false,
            _ => {},
        }
    }
    if let Some(atom_copy) = trace_atom {
        log::trace!("apply_bindings_to_atom: {} | {} -> {}", atom_copy, bindings, atom);
    }
}

/// Checks if atoms are equal up to variables replacement.
///
/// # Examples
///
/// ```
/// use hyperon_atom::expr;
/// use hyperon_atom::matcher::atoms_are_equivalent;
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

fn atoms_are_equivalent_with_bindings<'a>(left: &'a Atom, right: &'a Atom,
        left_vars: &mut HashMap<&'a VariableAtom, &'a VariableAtom>,
        right_vars: &mut HashMap<&'a VariableAtom, &'a VariableAtom>) -> bool {

    fn can_be_renamed<'a>(map: &mut HashMap<&'a VariableAtom, &'a VariableAtom>,
        var: &'a VariableAtom, atom: &'a VariableAtom) -> bool {
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
        (Atom::Grounded(left), Atom::Grounded(right)) => crate::gnd::gnd_eq(&**left, &**right),
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
    use hyperon_common::assert_eq_no_order;
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
        assert_eq!(bind!{ a: expr!("A") }.merge(
            &bind!{ a: expr!("C"), b: expr!("B") }), BindingsSet::empty());
        assert_eq!(bind!{ a: expr!("C"), b: expr!("B") }.merge(
            &bind!{ a: expr!("A") }), BindingsSet::empty());
    }

    #[test]
    fn bindings_merge() {
        assert_eq!(bind!{ a: expr!("A") }.merge(
            &bind!{ a: expr!("A"), b: expr!("B") }),
            bind_set![{ a: expr!("A"), b: expr!("B") }]);
        assert_eq!(bind!{ a: expr!("A"), b: expr!("B") }.merge(
            &bind!{ a: expr!("A") }),
            bind_set![{ a: expr!("A"), b: expr!("B") }]);
    }

    #[test]
    fn bindings_merge_self_recursion() {
        assert_eq!(bind!{ a: expr!(b)  }.merge(
            &bind!{ b: expr!("S" b) }),
            bind_set![{ a: expr!(b), b: expr!("S" b) }]);
    }

    #[test]
    fn match_variable_name_conflict() {
        assert_match(expr!("a" (W)), expr!("a" W), vec![]);
    }

    #[test]
    fn test_atoms_are_equivalent() {
        assert!(atoms_are_equivalent(&expr!(x "b" {"c"}), &expr!(x "b" {"c"})));
        assert!(atoms_are_equivalent(&expr!(x "b" x), &expr!(x "b" x)));
        assert!(atoms_are_equivalent(&expr!(a a "b" {"c"}), &expr!(x x "b" {"c"})));
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
            vec![bind!{a: sym!("v"), x: expr!(a), y: expr!(a)}]);
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
            vec![bind!{a: expr!(x), x: expr!("v"), y: expr!(a)}]);
    }

    #[test]
    fn match_replace_variable_via_left_variable() {
        assert_match(
            expr!(a a),
            expr!(x y),
            vec![bind!{a: expr!(x), a: expr!(y)}]);
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
        let last_id = VariableAtom::new("x").make_unique().id;
        let x_uniq = Atom::Variable(VariableAtom::new_id("x", last_id + 1));
        assert_match(
                                      expr!(("A" x)    z   ),
                make_variables_unique(expr!(("A" x) ("B" x))),
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
            vec![bind!{x: expr!((a)), a: expr!(y), y: expr!("v")}]);
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
        fn as_match(&self) -> Option<&dyn CustomMatch> {
            Some(self)
        }
    }

    impl CustomMatch for Rand {
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match other {
                Atom::Expression(expr) if expr.children().len() == 1 =>
                    match expr.children()[0] {
                        Atom::Variable(ref var) => {
                            let bindings = Bindings::new()
                                .add_var_binding(var, expr!({42})).unwrap();
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
        fn as_match(&self) -> Option<&dyn CustomMatch> {
            Some(self)
        }
    }

    impl CustomMatch for ReturnPairInX {
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
            .add_var_binding(VariableAtom::new("b"), Atom::sym("v"))?
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
        let bindings = Bindings::new();

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
    }

    #[test]
    fn bindings_get_variable_bound_to_value() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_binding(VariableAtom::new("x"), expr!("A" y))?
            .add_var_binding(VariableAtom::new("y"), expr!("B" z))?;

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), Some(expr!("A" ("B" z))));
        assert_eq!(bindings.resolve(&VariableAtom::new("y")), Some(expr!("B" z)));
        Ok(())
    }

    #[test]
    fn bindings_get_variable_bound_to_value_with_loop() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_binding(VariableAtom::new("x"), expr!("A" y))?
            .add_var_binding(VariableAtom::new("y"), expr!("B" x))?;

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
        assert_eq!(bindings.resolve(&VariableAtom::new("y")), None);
        Ok(())
    }

    #[test]
    fn bindings_get_variable_bound_to_variable() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_binding(VariableAtom::new("x"), expr!(x))?;

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), None);
        Ok(())
    }

    #[test]
    fn bindings_get_variable_equal_to_variable() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("x"), &VariableAtom::new("y"))?;

        assert_eq!(bindings.resolve(&VariableAtom::new("x")), Some(expr!(x)));
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
            .add_var_binding(VariableAtom::new("leftA"), expr!("A"))?
            .add_var_equality(&VariableAtom::new("leftA"), &VariableAtom::new("rightB"))?
            .add_var_binding(VariableAtom::new("leftC"), expr!("C"))?
            .add_var_equality(&VariableAtom::new("leftD"), &VariableAtom::new("rightE"))?
            .add_var_binding(VariableAtom::new("rightF"), expr!("F"))?;

        let narrow = bindings.narrow_vars(&HashSet::from([&VariableAtom::new("rightB"),
            &VariableAtom::new("rightE"), &VariableAtom::new("rightF")]));

        assert_eq!(narrow, bind!{ rightB: expr!("A"), rightF: expr!("F"), rightE: expr!(rightE) });
        Ok(())
    }

    #[test]
    fn bindings_narrow_vars_infinite_loop() -> Result<(), &'static str> {
        let bindings = Bindings::new().add_var_binding(VariableAtom::new("x"), expr!("a" x "b"))?;

        let narrow = bindings.narrow_vars(&HashSet::from([&VariableAtom::new("x")]));

        assert_eq!(narrow, bind!{ x: expr!("a" x "b") });
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
        let a = VariableAtom::new("a");
        let b = VariableAtom::new("b");
        let bindings = Bindings::new()
            .add_var_equality(&a, &b)?
            .add_var_binding(a.clone(), expr!("A"))?;

        let result = bindings.clone().convert_var_equalities_to_bindings(&[a.clone()].into());
        let expected = Bindings::new()
            .add_var_binding(a.clone(), expr!("A"))?
            .add_var_binding(&b, expr!(a))?;
        assert_eq!(result, expected);

        let result = bindings.clone().convert_var_equalities_to_bindings(&[b.clone()].into());
        let expected = Bindings::new()
            .add_var_binding(b.clone(), expr!("A"))?
            .add_var_binding(&a, expr!(b))?;
        assert_eq!(result, expected);

        Ok(())
    }

    #[test]
    fn bindings_add_var_binding_splits_bindings() {
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
    fn bindings_add_var_binding_self_recursion() {
        let a = VariableAtom::new("a");
        let bindings = Bindings::new();
        assert_eq!(
            bindings.add_var_binding(a.clone(), Atom::expr([Atom::sym("S"), Atom::Variable(a)])),
            Ok(bind!{ a: expr!("S" a) }));
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
    fn bindings_add_var_equality_self_recursion() -> Result<(), &'static str> {
        let a = VariableAtom::new("a");
        let b = VariableAtom::new("b");
        let bindings = Bindings::new();
        let bindings = bindings.add_var_binding(b.clone(), Atom::expr([Atom::sym("S"), Atom::Variable(a.clone())]))?;
        let bindings = bindings.add_var_equality(&a, &b);
        assert_eq!(bindings, Ok(bind!{ b: expr!("S" a),  a: expr!(b) }));
        Ok(())
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
            fn as_match(&self) -> Option<&dyn CustomMatch> {
                Some(self)
            }
        }

        impl CustomMatch for Assigner {
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

        let bindings = a.merge(&b);

        assert_eq!(bindings, bind_set![
            bind!{ a: expr!({ assigner }), b: expr!({ assigner }), x: expr!("C"), y: expr!("E") },
            bind!{ a: expr!({ assigner }), b: expr!({ assigner }), x: expr!("C"), y: expr!("F") },
            bind!{ a: expr!({ assigner }), b: expr!({ assigner }), x: expr!("D"), y: expr!("E") },
            bind!{ a: expr!({ assigner }), b: expr!({ assigner }), x: expr!("D"), y: expr!("F") },
        ]);
    }

    #[test]
    fn bindings_retain() -> Result<(), &'static str> {
        let mut atom = expr!(a b);
        let mut bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("b"))?;
        bindings.apply_and_retain(&mut atom, |v| *v == VariableAtom::new("a") || *v == VariableAtom::new("b"));
        assert_eq!(bindings, bind!{ a: expr!(b) });
        assert_eq!(atom, expr!(a b));

        let mut atom = expr!(a b c d e);
        let mut bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("b"))?
            .add_var_binding(VariableAtom::new("b"), expr!("B" d))?
            .add_var_binding(VariableAtom::new("c"), expr!("C"))?
            .add_var_binding(VariableAtom::new("d"), expr!("D"))?;
        bindings.apply_and_retain(&mut atom, |v| *v == VariableAtom::new("b") || *v == VariableAtom::new("e"));
        let expected = Bindings::new()
            .add_var_binding(VariableAtom::new("b"), expr!("B" "D"))?;
        assert_eq!(bindings, expected);
        assert_eq!(atom, expr!(("B" "D") b "C" "D" e));
        Ok(())
    }

    #[test]
    fn bindings_retain_all() -> Result<(), &'static str> {
        let mut atom = expr!(a b);
        let mut bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("b"))?;
        bindings.apply_and_retain(&mut atom, |_| false);
        assert!(bindings.is_empty());
        assert_eq!(atom, expr!(a a));
        Ok(())
    }

    #[test]
    fn bindings_retain_apply_to_self() -> Result<(), &'static str> {
        let mut bindings = Bindings::new()
            .add_var_binding(&VariableAtom::new("y"), expr!((x)))?
            .add_var_binding(&VariableAtom::new("x"), sym!("value"))?;
        bindings.apply_and_retain(&mut expr!(), |v| *v == VariableAtom::new("y"));
        assert_eq!(bindings, bind!{ y: expr!(("value")) });
        Ok(())
    }

    #[test]
    fn bindings_retain_apply_wiped_variable() -> Result<(), &'static str> {
        let mut atom = expr!(b);
        let mut bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("a"), &VariableAtom::new("retained"))?
            .add_var_binding(&VariableAtom::new("b"), expr!((a)))?;
        bindings.apply_and_retain(&mut atom, |v| *v == VariableAtom::new("retained"));
        assert_eq!(bindings, bind!{ retained: expr!(retained) });
        assert_eq!(atom, expr!((retained)));
        Ok(())
    }

    #[test]
    fn bindings_rename_binding() -> Result<(), &'static str> {
        let a = &VariableAtom::new("a");
        let b = &VariableAtom::new("b");
        let mut bindings = Bindings::new().add_var_equality(a, b)?;
        let binding_id = bindings.get_binding(a).unwrap().id;
        assert_eq!(bindings.resolve(a), Some(Atom::Variable(a.clone())));
        assert_eq!(bindings.resolve(b), Some(Atom::Variable(a.clone())));

        assert_eq!(bindings.rename_binding(binding_id), Ok(()));
        assert_eq!(bindings.resolve(a), Some(Atom::Variable(b.clone())));
        assert_eq!(bindings.resolve(b), Some(Atom::Variable(b.clone())));
        Ok(())
    }

    #[ignore = "Unstable because HashMap doesn't guarantee the order of entries"]
    #[test]
    fn bindings_into_entries() -> Result<(), &'static str> {
        let bindings = Bindings::new()
            .add_var_equality(&VariableAtom::new("x"), &VariableAtom::new("y"))?
            .add_var_binding(VariableAtom::new("x"), Atom::sym("Z"))?
            .add_var_binding(VariableAtom::new("a"), Atom::expr([Atom::sym("A"), Atom::var("x")]))?
            .add_var_binding(VariableAtom::new("b"), Atom::expr([Atom::sym("B"), Atom::var("x")]))?;

        let entries: Vec<(VariableAtom, Atom)> = bindings.into_vec_of_pairs();

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
            .add_var_binding(VariableAtom::new("x"), Atom::sym("Z"))?
            .add_var_binding(VariableAtom::new("a"), Atom::expr([Atom::sym("A"), Atom::var("x")]))?
            .add_var_binding(VariableAtom::new("b"), Atom::expr([Atom::sym("B"), Atom::var("x")]))?;

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
            .add_var_binding(VariableAtom::new("z"), Atom::sym("Z"))?
            .add_var_binding(VariableAtom::new("a"), Atom::expr([Atom::sym("A"), Atom::var("z")]))?
            .add_var_binding(VariableAtom::new("b"), Atom::expr([Atom::sym("B"), Atom::var("z")]))?;
        assert_eq!(renamed, expected);
        Ok(())
    }
}
