pub mod arithmetics;

use super::*;
use std::fmt::Display;

pub struct GroundedValue<T> {
    value: T,
}

impl<T: 'static + PartialEq + Display + Copy> GroundedValue<T> {
    pub fn new(value: T) -> Atom {
        Atom::gnd(GroundedValue{ value: value })
    }
}

impl<T: 'static + PartialEq + Display + Copy> GroundedAtom for GroundedValue<T> {
    fn eq(&self, other: &Box<dyn GroundedAtom>) -> bool {
        match other.downcast_ref::<GroundedValue<T>>() {
            Some(o) => self.value == o.value,
            None => false,
        }
    }
    fn clone(&self) -> Box<dyn GroundedAtom> {
        // FIXME: we could actually clone Box without copying value as it is immutable
        Box::new(GroundedValue{ value: self.value })
    }
}

impl<T: Display> Display for GroundedValue<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
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
