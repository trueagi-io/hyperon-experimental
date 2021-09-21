mod matching;
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

pub trait GroundedValue {
    fn execute(&self, args: &Vec<&Atom>) -> Result<Vec<Atom>, &str>;
    fn eq(&self, other: Rc<dyn GroundedValue>) -> bool;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error>;
}

pub struct GroundedAtom {
    value: Rc<dyn GroundedValue>,
}

impl Clone for GroundedAtom {
    fn clone(&self) -> Self {
        GroundedAtom{ value: Rc::clone(&self.value) }
    }
}

impl PartialEq for GroundedAtom {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(Rc::clone(&other.value))
    }
}

impl std::fmt::Debug for GroundedAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.value.fmt(f)
    }
}

pub struct Value<T> {
    x: T,
}

impl<T: 'static + PartialEq + std::fmt::Display> Value<T> {
    pub fn new(x: T) -> Atom {
        Atom::Grounded(GroundedAtom{ value: Rc::new(Value{ x: x }) })
    }
}

impl<T: PartialEq + std::fmt::Display> GroundedValue for Value<T> {
    fn execute(&self, _args: &Vec<&Atom>) -> Result<Vec<Atom>, &str> {
        Err("value is not executable")
    }

    fn eq(&self, other: Rc<dyn GroundedValue>) -> bool {
        let o = Rc::into_raw(other) as *const Value<T>;
        unsafe {
            self.x == (*o).x
        }
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.x.fmt(f)
    }
}

pub type Int = Value<i32>;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Symbol{ symbol: String },
    Expression{ children: Vec<Atom> },
    Variable(VariableAtom),
    Grounded(GroundedAtom),
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
            match matching::match_atoms(next, pattern) {
                Some((_, b_bindings)) => result.push(b_bindings),
                None => continue,
            }
        }
        result
    }

}

