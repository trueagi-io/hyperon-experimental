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

#[cfg(test)]
mod tests {
    use super::*;

    fn test(_ops: &mut Vec<Atom>, _data: &mut Vec<Atom>) -> Result<(), String> {
        Ok(())
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
