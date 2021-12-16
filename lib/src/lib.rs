pub mod text;
pub mod common;
pub mod interpreter;
pub mod arithmetics;

mod matcher;
#[cfg(test)]
mod tests;

#[macro_use]
extern crate mopa;

use text::{SExprSpace};

use std::fmt::{Display, Debug};
use std::collections::HashMap;

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
    ($($k:ident: $v:expr),*) => { vec![$( (VariableAtom::from(stringify!($k)), $v), )*]
        .iter().cloned().collect() };
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

#[derive(Debug)]
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

// Grounding space

pub type Bindings = HashMap<VariableAtom, Atom>;

// FIXME: clone should not be required but as interpret puts the GroundingSpace
// as Atom into data stack we cannot run interpret without moving into
// into interpreter.
#[derive(Clone)]
pub struct GroundingSpace {
    content: Vec<Atom>,
}

impl GroundingSpace {

    pub fn new() -> Self {
        Self{ content: Vec::new() }
    }
    
    pub fn add(&mut self, atom: Atom) {
        self.content.push(atom)
    }

    pub fn query(&self, pattern: &Atom) -> Vec<Bindings> {
        log::debug!("query: pattern: {}", pattern);
        let mut result = Vec::new();
        for next in &self.content {
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

