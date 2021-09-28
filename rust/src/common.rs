use super::*;
use std::fmt::Display;

pub type ExecuteFunc = fn(&mut Vec<Atom>, &mut Vec<Atom>) -> Result<(), String>;

pub struct Operation {
    pub name: &'static str,
    pub execute: ExecuteFunc,
}

impl GroundedAtom for &'static Operation {
    fn execute(&self, ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
        (self.execute)(ops, data)
    }

    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<&Operation>() {
            Some(o) => self.name.eq(o.name),
            None => false,
        }
    }

    fn clone_gnd(&self) -> Box<dyn GroundedAtom> {
        Box::new(*self)
    }
}

impl Display for &'static Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
