pub mod arithmetics;

mod matcher;
#[cfg(test)]
mod tests;

use std::collections::HashMap;
use std::rc::Rc;

#[macro_export]
macro_rules! expr {
    () => {};
    ($x:ident) => { Atom::var(stringify!($x)) };
    ($x:literal) => { Atom::sym($x) };
    (($($x:tt),*)) => { Atom::expr(&[ $( expr!($x) , )* ]) };
    ($($x:tt),*) => { Atom::expr(&[ $( expr!($x) , )* ]) };
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VariableAtom {
    name: String,
}

impl VariableAtom {
    fn from(name: &str) -> VariableAtom {
        VariableAtom{ name: name.to_string() }
    }
}

pub trait GroundedAtom : std::fmt::Display {
    fn execute(&self, _ops: &mut Vec<&Atom>, _data: &mut Vec<&Atom>) -> Result<(), String> {
        Err(format!("{} is not executable", self))
    }
    fn eq(&self, other: Rc<dyn GroundedAtom>) -> bool;
}

pub struct GroundedAtomHolder {
    atom: Rc<dyn GroundedAtom>,
}

impl Clone for GroundedAtomHolder {
    fn clone(&self) -> Self {
        GroundedAtomHolder{ atom: Rc::clone(&self.atom) }
    }
}

impl PartialEq for GroundedAtomHolder {
    fn eq(&self, other: &Self) -> bool {
        self.atom.eq(Rc::clone(&other.atom))
    }
}

impl std::fmt::Debug for GroundedAtomHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.atom.fmt(f)
    }
}

pub struct GroundedValue<T> {
    x: T,
}

impl<T: 'static + PartialEq + std::fmt::Display> GroundedValue<T> {
    pub fn new(x: T) -> Atom {
        Atom::Grounded(GroundedAtomHolder{ atom: Rc::new(GroundedValue{ x: x }) })
    }
}

impl<T: PartialEq + std::fmt::Display> GroundedAtom for GroundedValue<T> {
    fn eq(&self, other: Rc<dyn GroundedAtom>) -> bool {
        let o = Rc::into_raw(other) as *const GroundedValue<T>;
        unsafe {
            self.x == (*o).x
        }
    }
}

impl<T: PartialEq + std::fmt::Display> std::fmt::Display for GroundedValue<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.x.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Symbol{ symbol: String },
    Expression{ children: Vec<Atom> },
    Variable(VariableAtom),
    Grounded(GroundedAtomHolder),
}

impl Atom {
    pub fn sym(name: &str) -> Self {
        Self::Symbol{ symbol: name.to_string() }
    }

    pub fn expr(children: &[Atom]) -> Self {
        Self::Expression{ children: children.to_vec() }
    }

    pub fn var(name: &str) -> Self {
        Self::Variable(VariableAtom::from(name))
    }
}

pub type Bindings = HashMap<VariableAtom, Atom>;

pub struct GroundingSpace {
    content: Vec<Atom>,
}

impl GroundingSpace {

    pub fn new() -> GroundingSpace {
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

    pub fn interpret(&self, ops: &mut Vec<&Atom>, data: &mut Vec<&Atom>) -> Result<(), String> {
        let op = ops.pop();
        match op {
            Some(Atom::Grounded(GroundedAtomHolder{ atom })) => atom.execute(ops, data),
            Some(_) => Err("Ops stack contains non grounded atom".to_string()),
            None => Err("Ops stack is empty".to_string()),
        }
    }

}
