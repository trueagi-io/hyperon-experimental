use super::*;
use std::fmt::Display;

// TODO: should be possible to do it by explicitly implementing GroundedAtom
// trait for all 'static values which implement PartialEqa+Display+Copy.
pub struct GroundedValue<T> {
    value: T,
}

impl<T: 'static + PartialEq + Display + Copy> GroundedValue<T> {
    pub fn new(value: T) -> Atom {
        Atom::gnd(GroundedValue{ value: value })
    }
    pub fn get(&self) -> T {
        self.value
    }
}

impl<T: 'static + PartialEq + Display + Copy> GroundedAtom for GroundedValue<T> {
    fn eq(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<GroundedValue<T>>() {
            Some(o) => self.value == o.value,
            None => false,
        }
    }
    fn clone(&self) -> Box<dyn GroundedAtom> {
        Box::new(GroundedValue{ value: self.value })
    }
}

impl<T: Display> Display for GroundedValue<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

pub type ExecuteFunc = fn(&mut Vec<Atom>, &mut Vec<Atom>) -> Result<(), String>;

pub struct Operation {
    pub name: &'static str,
    pub execute: ExecuteFunc,
}

impl GroundedAtom for Operation {
    fn execute(&self, ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
        (self.execute)(ops, data)
    }

    fn eq(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<Operation>() {
            Some(o) => self.name == o.name,
            None => false,
        }
    }

    fn clone(&self) -> Box<dyn GroundedAtom> {
        panic!("This method is not expected to be called")
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grounded_value() {
        assert_eq!(GroundedValue::new(3),
            Atom::gnd(GroundedValue{ value: 3 }));
    }
}
