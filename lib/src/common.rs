use super::*;
use std::cell::RefCell;

// GndRefCell is used to keep pointer to the data located on heap as GroundedAtom.
// RefCell itself doesn't implement Display, and forwards PartialEq to internal
// data even when kept type doesn't implement PartialEq. GndRefCell fixes this
// by implementing dummy Display and implementing PartialEq via comparing
// pointers to the data.
pub struct GndRefCell<T>(RefCell<T>);

impl<T> GndRefCell<T> {
    pub const fn new(value: T) -> Self {
        Self(RefCell::new(value))
    }
    pub fn raw(&self) -> &RefCell<T> {
        &self.0
    }
}

impl<T> PartialEq for GndRefCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<T> Debug for GndRefCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GndRefCell")
    }
}

// Operation implements stateless operations as GroundedAtom.
// Each operation has the only instance which is identified by unique name.
// The instance has 'static lifetime and not copied when cloned.
pub struct Operation {
    pub name: &'static str,
    pub execute: ExecuteFunc,
}

pub type ExecuteFunc = fn(&mut Vec<Atom>) -> Result<Vec<Atom>, String>;

impl GroundedAtom for &'static Operation {
    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        (self.execute)(args)
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

impl Debug for &'static Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(_args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        Ok(vec![])
    }

    #[test]
    fn test_operation_display() {
        let op = &Operation{ name: "test", execute: test };
        assert_eq!(format!("{}", Atom::gnd(op)), "test");
    }

    #[test]
    fn test_operation_eq() {
        let a = Atom::gnd(&Operation{ name: "a", execute: test });
        let aa = Atom::gnd(&Operation{ name: "a", execute: test });
        let b = Atom::gnd(&Operation{ name: "b", execute: test });
        assert!(a == aa);
        assert!(a != b);
    }

    #[test]
    fn test_operation_clone() {
        let opa = Atom::gnd(&Operation{ name: "a", execute: test });
        let opc = opa.clone();
        if let (Atom::Grounded(boxa), Atom::Grounded(boxc)) = (opa, opc) {
            let ptra: *const Operation = *(boxa.downcast::<&Operation>().unwrap());
            let ptrc: *const Operation = *(boxc.downcast::<&Operation>().unwrap());
            assert_eq!(ptra, ptrc);
        } else {
            assert!(false);
        }
    }
}
