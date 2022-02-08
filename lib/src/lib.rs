pub mod text;
pub mod common;
pub mod interpreter;
pub mod arithmetics;

mod matcher;
mod types;
#[cfg(test)]
mod tests;
mod examples;

#[macro_use]
extern crate mopa;

use text::{SExprSpace};

use std::fmt::{Display, Debug};
use std::collections::{HashMap, HashSet};

use delegate::delegate;

// Macros to simplify expression writing

#[macro_export]
macro_rules! expr {
    () => { Atom::expr(&[]) };
    ($x:ident) => { Atom::var(stringify!($x)) };
    ($x:literal) => { Atom::sym($x) };
    ({$x:tt}) => { Atom::gnd($x) };
    (($($x:tt),*)) => { Atom::expr(&[ $( expr!($x) , )* ]) };
    ($($x:tt),*) => { Atom::expr(&[ $( expr!($x) , )* ]) };
}

#[macro_export]
macro_rules! bind {
    ($($k:ident: $v:expr),*) => { Bindings(vec![$( (VariableAtom::from(stringify!($k)), $v), )*]
        .iter().cloned().collect()) };
}

// Symbol atom

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolAtom {
    name: String,
}

impl From<String> for SymbolAtom {
    fn from(name: String) -> Self {
        SymbolAtom{ name }
    }
}

impl From<&str> for SymbolAtom {
    fn from(name: &str) -> Self {
        SymbolAtom::from(name.to_string())
    }
}

impl SymbolAtom {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Display for SymbolAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

// Expression atom

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionAtom {
    children: Vec<Atom>,
}

impl ExpressionAtom {
    pub fn is_plain(&self) -> bool {
        self.children.iter().all(|atom| ! matches!(atom, Atom::Expression(_)))
    }

    pub fn children(&self) -> &Vec<Atom> {
        &self.children
    }

    pub fn children_mut(&mut self) -> &mut Vec<Atom> {
        &mut self.children
    }
}

impl From<Vec<Atom>> for ExpressionAtom {
    fn from(children: Vec<Atom>) -> Self {
        ExpressionAtom{ children }
    }
}

impl From<&[Atom]> for ExpressionAtom {
    fn from(children: &[Atom]) -> Self {
        ExpressionAtom::from(children.to_vec())
    }
}

impl Display for ExpressionAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")
            .and_then(|_| self.children.iter().take(1).fold(Ok(()),
                |res, atom| res.and_then(|_| write!(f, "{}", atom))))
            .and_then(|_| self.children.iter().skip(1).fold(Ok(()),
                |res, atom| res.and_then(|_| write!(f, " {}", atom))))
            .and_then(|_| write!(f, ")"))
    }
}

// Variable atom

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VariableAtom {
    name: String,
}

impl From<String> for VariableAtom {
    fn from(name: String) -> Self {
        VariableAtom{ name }
    }
}

impl From<&str> for VariableAtom {
    fn from(name: &str) -> Self {
        VariableAtom::from(name.to_string())
    }
}

impl VariableAtom {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Display for VariableAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.name)
    }
}

// Grounded atom

pub trait GroundedAtom : Debug + mopa::Any {
    fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        Err(format!("{:?} is not executable", self))
    }
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool;
    fn clone_gnd(&self) -> Box<dyn GroundedAtom>;
}

mopafy!(GroundedAtom);

// FIXME: one cannot implement custom execute() for the type which
// have derived Clone, PartialEq and Debug at the same time because
// it automatically have GroundedAtom implemented from the impl below.
// This is a Rust restriction: there is not method overriding and
// trait can be implemented only once for each type.

// GroundedAtom implementation for all "regular" types
// to allow using them as GroundedAtoms
// 'static is required because mopa::Any requires it
impl<T: 'static + Clone + PartialEq + Debug> GroundedAtom for T {
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<T>() {
            Some(o) => self.eq(o),
            None => false,
        }
    }

    fn clone_gnd(&self) -> Box<dyn GroundedAtom> {
        Box::new(self.clone())
    }
}

// Atom enum

pub enum Atom {
    Symbol(SymbolAtom),
    Expression(ExpressionAtom),
    Variable(VariableAtom),
    // We need using Box here because:
    // - we cannot use GroundedAtom because trait size is not known at compile time
    // - reference to trait does not allow heap allocated values
    // - other smart pointers like Rc doesn't allow choosing whether value should
    //   be copied or shared between two atoms when clone() is called
    Grounded(Box<dyn GroundedAtom>),
}

impl Atom {
    pub fn sym(name: &str) -> Self {
        Self::Symbol(SymbolAtom::from(name))
    }

    pub fn expr(children: &[Atom]) -> Self {
        Self::Expression(ExpressionAtom::from(children))
    }

    pub fn var(name: &str) -> Self {
        Self::Variable(VariableAtom::from(name))
    }

    pub fn gnd<T: GroundedAtom>(gnd: T) -> Atom {
        Self::Grounded(Box::new(gnd))
    }

    pub fn as_gnd<T: GroundedAtom>(&self) -> Option<&T> {
        match self {
            Atom::Grounded(gnd) => gnd.downcast_ref::<T>(),
            _ => None,
        }
    }

    pub fn as_gnd_mut<T: GroundedAtom>(&mut self) -> Option<&mut T> {
        match self {
            Atom::Grounded(gnd) => gnd.downcast_mut::<T>(),
            _ => None,
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Symbol(sym1), Atom::Symbol(sym2)) => sym1 == sym2,
            (Atom::Expression(expr1), Atom::Expression(expr2)) => expr1 == expr2,
            (Atom::Variable(var1), Atom::Variable(var2)) => var1 == var2,
            (Atom::Grounded(gnd1), Atom::Grounded(gnd2)) => gnd1.eq_gnd(&**gnd2),
            _ => false,
        }
    }
}

impl Eq for Atom {}

impl Clone for Atom {
    fn clone(&self) -> Self {
        match self {
            Atom::Symbol(sym) => Atom::Symbol(sym.clone()),
            Atom::Expression(expr) => Atom::Expression(expr.clone()),
            Atom::Variable(var) => Atom::Variable(var.clone()),
            Atom::Grounded(gnd) => Atom::Grounded((*gnd).clone_gnd()),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Symbol(sym) => Display::fmt(sym, f),
            Atom::Expression(expr) => Display::fmt(expr, f),
            Atom::Variable(var) => Display::fmt(var, f),
            Atom::Grounded(gnd) => Debug::fmt(gnd, f),
        }
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

// Grounding space

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
        }
    }

    fn merge_bindings(prev: &Bindings, next: &Bindings) -> Option<Bindings> {
        let move_value = | to: &mut Bindings, from: &Bindings, k: &VariableAtom | {
           to.insert(k.clone(), from.get(k).unwrap().clone()); };
        let mut res = Bindings::new();
        for k in prev.0.keys().chain(next.0.keys()).collect::<HashSet<&VariableAtom>>() {
            match (prev.0.contains_key(k), next.0.contains_key(k)) {
                (true, true) if prev.get(k) == next.get(k) => move_value(&mut res, prev, k),
                (true, false) => move_value(&mut res, prev, k),
                (false, true) => move_value(&mut res, next, k),
                _ => return None,
            }
        }
        Some(res) 
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

pub type Unifications = Vec<matcher::UnificationPair>;

use std::rc::Rc;

// TODO: Clone is required by C API
#[derive(Clone)]
pub struct GroundingSpace {
    content: Rc<Vec<Atom>>,
}

impl GroundingSpace {

    pub fn new() -> Self {
        Self{ content: Rc::new(Vec::new()) }
    }
    
    pub fn add(&mut self, atom: Atom) {
        Rc::get_mut(&mut self.content).expect("Cannot mutate shared atomspace").push(atom)
    }

    pub fn query(&self, pattern: &Atom) -> Vec<Bindings> {
        match pattern {
            Atom::Expression(expr)
                if expr.children().len() > 0
                    && expr.children()[0] == Atom::sym(",") => {
                        let mut children = expr.children().iter().skip(1);
                        let first = children.next();
                        if let Some(first) = first {
                            children.fold(self.query(first),
                                |acc, pattern| {
                                    if acc.len() == 0 {
                                        acc
                                    } else {
                                        let res = self.query(pattern);
                                        Self::merge_results(acc, res)
                                    }
                                })
                        } else {
                            Vec::new()
                        }
                },
            _ => self.single_query(pattern),
        }
    }

    fn merge_results(prev: Vec<Bindings>, next: Vec<Bindings>) -> Vec<Bindings> {
        prev.iter().flat_map(|p| -> Vec<Option<Bindings>> {
            next.iter().map(|n| Bindings::merge_bindings(p, n)).collect()
        }).filter(Option::is_some).map(Option::unwrap).collect()
    }
    
    fn single_query(&self, pattern: &Atom) -> Vec<Bindings> {
        log::debug!("query: pattern: {}", pattern);
        let mut result = Vec::new();
        for next in &(*self.content) {
            match matcher::match_atoms(next, pattern) {
                Some(res) => {
                    let bindings = matcher::apply_bindings_to_bindings(&res.candidate_bindings, &res.pattern_bindings);
                    if let Ok(bindings) = bindings {
                        // TODO: implement Display for bindings
                        log::debug!("query: push result: {}, bindings: {:?}", next, bindings);
                        result.push(bindings);
                    }
                },
                None => continue,
            }
        }
        result
    }

    pub fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        self.query(pattern).drain(0..)
            .map(| bindings | matcher::apply_bindings_to_atom(template, &bindings))
            .collect()
    }

    // TODO: for now we have separate methods query() and unify() but
    // they probably can be merged. One way of doing it is designating
    // in the query which part of query should be unified and which matched.
    // For example for the typical query in a form (= (+ a b) $X) the
    // (= (...) $X) level should not be unified otherwise we will recursively
    // infer that we need calculating (+ a b) again which is equal to original
    // query. Another option is designating this in the data itself.
    // After combining match and unification we could leave only single
    // universal method.
    pub fn unify(&self, pattern: &Atom) -> Vec<(Bindings, Unifications)> {
        log::debug!("unify: pattern: {}", pattern);
        let mut result = Vec::new();
        for next in &(*self.content) {
            match matcher::unify_atoms(next, pattern) {
                Some(res) => {
                    let bindings = matcher::apply_bindings_to_bindings(&res.candidate_bindings, &res.pattern_bindings);
                    if let Ok(bindings) = bindings {
                        // TODO: implement Display for bindings
                        log::debug!("unify: push result: {}, bindings: {:?}", next, bindings);
                        result.push((bindings, res.unifications));
                    }
                },
                None => continue,
            }
        }
        result
    }

    pub fn as_vec(&self) -> &Vec<Atom> {
        &self.content
    }

    pub fn atom_iter(&self) -> std::slice::Iter<Atom>{
        self.content.iter()
    }
}

impl PartialEq for GroundingSpace {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl Debug for GroundingSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundingSpace")
    }
}

impl Display for GroundingSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundingSpace")
    }
}

impl From<&SExprSpace> for GroundingSpace {
    fn from(other: &SExprSpace) -> Self {
        let mut space = GroundingSpace::new();
        other.into_grounding_space(&mut space);
        space
    }
}

