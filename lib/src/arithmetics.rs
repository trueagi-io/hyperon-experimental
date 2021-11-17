use crate::*;
use crate::common::*;

pub static SUM: &Operation = &Operation{ name: "+", execute: |ops, data| bin_op(ops, data, |a, b| a + b) };
pub static SUB: &Operation = &Operation{ name: "-", execute: |ops, data| bin_op(ops, data, |a, b| a - b) };
pub static MUL: &Operation = &Operation{ name: "*", execute: |ops, data| bin_op(ops, data, |a, b| a * b) };

fn bin_op(_ops: &mut Vec<Atom>, data: &mut Vec<Atom>, op: fn(i32, i32) -> i32) -> Result<(), String> {
    // TODO: getting arguments from stack and checking their type can be
    // done in separate helper function or macros.
    let arg1 = data.pop().expect("Sum operation called without arguments"); 
    let arg2 = data.pop().expect("Sum operation called with only argument");
    if let (Some(arg1), Some(arg2)) = (arg1.as_gnd::<i32>(), arg2.as_gnd::<i32>()) {
        data.push(Atom::gnd(op(*arg1, *arg2)));
        Ok(())
    } else {
        Err(format!("One of the arguments is not integer: ({}, {})", arg1, arg2))
    }
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

    use super::*;
    use crate::interpreter::*;
    use std::rc::Rc;

    // Aliases to have a shorter notation
    fn S(name: &str) -> Atom { Atom::sym(name) }
    fn E(children: &[Atom]) -> Atom { Atom::expr(children) }
    fn V(name: &str) -> Atom { Atom::var(name) }
    fn G<T: GroundedAtom>(value: T) -> Atom { Atom::gnd(value) }

    #[test]
    fn test_sum_ints() {
        let space = GroundingSpace::new();
        // (+ 3 5)
        let expr = E(&[G(SUM), G(3), G(5)]);

        assert_eq!(interpret(Rc::new(space), &expr), Ok(G(8)));
    }

    #[test]
    fn test_sum_ints_recursively() {
        let space = GroundingSpace::new();
        // (+ 4 (+ 3 5))
        let expr = E(&[G(SUM), G(4), E(&[G(SUM), G(3), G(5)])]);

        assert_eq!(interpret(Rc::new(space), &expr), Ok(G(12)));
    }

    #[test]
    fn test_match_factorial() {
        let mut space = GroundingSpace::new();
        // (= (fac 0) 1)
        space.add(E(&[ S("="), E(&[ S("fac"), G(0) ]), G(1) ]));
        // (= (fac n) (* n (fac (- n 1))))
        space.add(E(&[ S("="), E(&[ S("fac"), V("n") ]),
            E(&[ G(MUL), V("n"), E(&[ S("fac"), E(&[ G(SUB), V("n"), G(1) ]) ]) ]) ]));

        let expr = E(&[ S("fac"), G(3) ]);
        let expected: Bindings = [(VariableAtom::from("X"), E(&[ G(MUL), G(3), E(&[ S("fac"), E(&[ G(SUB), G(3), G(1) ]) ]) ]) )].iter().cloned().collect();
        assert_eq!(space.query(&E(&[ S("="), expr, V("X") ])), vec![expected]);
    }

    #[test]
    fn test_factorial() {
        let mut space = GroundingSpace::new();
        // (= (fac n) (* n (fac (- n 1))))
        space.add(E(&[ S("="), E(&[ S("fac"), V("n") ]),
            E(&[ G(MUL), V("n"), E(&[ S("fac"), E(&[ G(SUB), V("n"), G(1) ]) ]) ]) ]));
        // (= (fac 0) 1)
        space.add(E(&[ S("="), E(&[ S("fac"), G(0) ]), G(1) ]));
        let expr = E(&[ S("fac"), G(3) ]);
        assert_eq!(interpret(Rc::new(space), &expr), Ok(G(6)));
    }
}
