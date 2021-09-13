mod matching;
#[cfg(test)]
mod tests;

use std::collections::HashMap;

#[macro_export]
macro_rules! expr {
    () => {};
    ($x:ident) => { Atom::var(stringify!($x)) };
    ($x:literal) => { Atom::sym($x) };
    (($($x:tt),*)) => { Atom::expr(&[ $( expr!($x) , )* ]) };
    ($($x:tt),*) => { Atom::expr(&[ $( expr!($x) , )* ]) };
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Hash)]
#[derive(PartialEq)]
#[derive(Eq)]
pub struct VariableAtom {
    name: String,
}

impl VariableAtom {
    fn from(name: &str) -> VariableAtom {
        VariableAtom{ name: name.to_string() }
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub enum Atom {
    Symbol{ symbol: String },
    Expression{ children: Vec<Atom> },
    Variable(VariableAtom),
    Grounded,
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

