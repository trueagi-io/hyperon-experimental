pub mod common;
pub mod interpreter;
pub mod arithmetics;

mod matcher;
#[cfg(test)]
mod tests;

#[macro_use]
extern crate mopa;

use std::collections::HashMap;
use std::fmt::{Display, Debug};

// Macros to simplify expression writing

#[macro_export]
macro_rules! expr {
    () => { Atom::expr(&[]) };
    ($x:ident) => { Atom::var(stringify!($x)) };
    ($x:literal) => { Atom::sym($x) };
    (($($x:tt),*)) => { Atom::expr(&[ $( expr!($x) , )* ]) };
    ($($x:tt),*) => { Atom::expr(&[ $( expr!($x) , )* ]) };
}

// Expression atom

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionAtom {
    children: Vec<Atom>,
}

impl ExpressionAtom {
    fn from(children: &[Atom]) -> Self {
        ExpressionAtom{ children: children.to_vec() }
    }

    pub fn is_plain(&self) -> bool {
        self.children.iter().all(|atom| ! matches!(atom, Atom::Expression(_)))
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

impl VariableAtom {
    fn from(name: &str) -> Self {
        VariableAtom{ name: name.to_string() }
    }
}

impl Display for VariableAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.name)
    }
}

// Grounded atom

pub trait GroundedAtom : Display + mopa::Any {
    fn execute(&self, _ops: &mut Vec<Atom>, _data: &mut Vec<Atom>) -> Result<(), String> {
        Err(format!("{} is not executable", self))
    }
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool;
    fn clone_gnd(&self) -> Box<dyn GroundedAtom>;
}

impl Debug for dyn GroundedAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

mopafy!(GroundedAtom);

// GroundedAtom implementation for all "regular" types
// to allow using them as GroundedAtoms
impl<T: 'static + Clone + PartialEq + Display> GroundedAtom for T {
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
    Symbol{ symbol: String },
    Expression(ExpressionAtom),
    Variable(VariableAtom),
    // We need using Box here because if we use reference then we cannot keep
    // values created dynamically on heap.
    Grounded(Box<dyn GroundedAtom>),
}

impl Atom {
    pub fn sym(name: &str) -> Self {
        Self::Symbol{ symbol: name.to_string() }
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

    pub fn as_expr(&self) -> Option<&ExpressionAtom> {
        match self {
            Atom::Expression(ref expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_expr_mut(&mut self) -> Option<&mut ExpressionAtom> {
        match self {
            Atom::Expression(ref mut expr) => Some(expr),
            _ => None,
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Symbol{ symbol: sym1 }, Atom::Symbol{ symbol: sym2 }) => sym1 == sym2,
            (Atom::Expression(expr1), Atom::Expression(expr2)) => expr1 == expr2,
            (Atom::Variable(var1), Atom::Variable(var2)) => var1 == var2,
            (Atom::Grounded(gnd1), Atom::Grounded(gnd2)) => gnd1.eq_gnd(&**gnd2),
            _ => false,
        }
    }
}

impl Clone for Atom {
    fn clone(&self) -> Self {
        match self {
            Atom::Symbol{ symbol: sym } => Atom::Symbol{ symbol: sym.clone() },
            Atom::Expression(expr) => Atom::Expression(expr.clone()),
            Atom::Variable(var) => Atom::Variable(var.clone()),
            Atom::Grounded(gnd) => Atom::Grounded((*gnd).clone_gnd()),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Symbol{ symbol: sym } => Display::fmt(sym, f),
            Atom::Expression(expr) => Display::fmt(expr, f),
            Atom::Variable(var) => Display::fmt(var, f),
            Atom::Grounded(gnd) => Display::fmt(gnd, f),
        }
    }
}

// Grounding space

pub type Bindings = HashMap<VariableAtom, Atom>;

pub struct GroundingSpace {
    content: Vec<Atom>,
}

impl GroundingSpace {

    pub fn new() -> Self {
        GroundingSpace{ content: Vec::new() }
    }
    
    pub fn add(&mut self, atom: Atom) {
        self.content.push(atom)
    }

    pub fn query(&self, pattern: &Atom) -> Vec<Bindings> {
        let mut result = Vec::new();
        for next in &self.content {
            match matcher::match_atoms(next, pattern) {
                Some((_, b_bindings)) => result.push(b_bindings),
                None => continue,
            }
        }
        result
    }

    pub fn interpret(&self, ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
        let op = ops.pop();
        match op {
            Some(Atom::Grounded(atom)) => atom.execute(ops, data),
            Some(_) => Err("Ops stack contains non grounded atom".to_string()),
            None => Err("Ops stack is empty".to_string()),
        }
    }

}

