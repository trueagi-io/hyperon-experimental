pub mod plan;
pub mod collections;
mod arithmetics;

pub use arithmetics::*;

use crate::*;
use std::cell::RefCell;
use std::fmt::{Debug, Display};

use crate::metta::metta_atom;

pub fn init_logger(is_test: bool) {
   let _ = env_logger::builder().is_test(is_test).try_init();
}

// TODO: move Operation and arithmetics under metta package as it uses metta_atom
// Operation implements stateless operations as GroundedAtom.
// Each operation has the only instance which is identified by unique name.
// The instance has 'static lifetime and not copied when cloned.
pub struct Operation {
    pub name: &'static str,
    pub execute: fn(&Operation, &mut Vec<Atom>) -> Result<Vec<Atom>, String>,
    pub typ: &'static str,
}

impl Grounded for &'static Operation {
    fn type_(&self) -> Atom {
        metta_atom(self.typ)
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        (self.execute)(self, args)
    }

    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        match_by_equality(self, other)
    }
}

impl PartialEq for Operation {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Debug for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Operation")
            .field("name", &self.name)
            .field("typ", &self.typ)
            .finish()
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

// GndRefCell is used to keep pointer to the data located on heap as GroundedAtom.
// RefCell itself doesn't implement Display, and forwards PartialEq to internal
// data even when kept type doesn't implement PartialEq. GndRefCell fixes this
// by implementing dummy Display and implementing PartialEq via comparing
// pointers to the data.
#[derive(Clone, Debug)]
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

impl<T> Display for GndRefCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GndRefCell")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_op(_this: &Operation, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        Ok(vec![])
    }

    #[test]
    fn test_operation_display() {
        let op = &Operation{ name: "test_op", execute: test_op, typ: "(-> ())" };
        assert_eq!(format!("{}", Atom::gnd(op)), "test_op");
    }

    #[test]
    fn test_operation_debug() {
        let op = &Operation{ name: "test_op", execute: test_op, typ: "(-> ())" };
        assert_eq!(format!("{:?}", Atom::gnd(op)), "Grounded(CustomGrounded(Operation { name: \"test_op\", typ: \"(-> ())\" }))");
    }

    #[test]
    fn test_operation_eq() {
        let a = Atom::gnd(&Operation{ name: "a", execute: test_op, typ: "(-> ())" });
        let aa = Atom::gnd(&Operation{ name: "a", execute: test_op, typ: "(-> ())" });
        let b = Atom::gnd(&Operation{ name: "b", execute: test_op, typ: "(-> ())" });
        assert!(a == aa);
        assert!(a != b);
    }

    #[test]
    fn test_operation_clone() {
        let opa = Atom::gnd(&Operation{ name: "a", execute: test_op, typ: "(-> ())" });
        let opc = opa.clone();
        if let (Some(refa), Some(refc)) = (opa.as_gnd::<&Operation>(), opc.as_gnd::<&Operation>()) {
            let ptra: *const Operation = *refa; 
            let ptrc: *const Operation = *refc;
            assert_eq!(ptra, ptrc);
        } else {
            assert!(false);
        }
    }
}
