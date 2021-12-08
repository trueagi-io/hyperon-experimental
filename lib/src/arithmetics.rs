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
    fn G<T: GroundedAtom>(value: T) -> Atom { Atom::gnd(value) }

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_sum_ints() {
        init();
        let space = GroundingSpace::new();
        // (+ 3 5)
        let expr = expr!({SUM}, {3}, {5});

        assert_eq!(interpret(Rc::new(space), &expr), Ok(vec![G(8)]));
    }

    #[test]
    fn test_sum_ints_recursively() {
        init();
        let space = GroundingSpace::new();
        // (+ 4 (+ 3 5))
        let expr = expr!({SUM}, {4}, ({SUM}, {3}, {5}));

        assert_eq!(interpret(Rc::new(space), &expr), Ok(vec![G(12)]));
    }

    #[test]
    fn test_match_factorial() {
        init();
        let mut space = GroundingSpace::new();
        // (= (fac 0) 1)
        space.add(expr!("=", ("fac", {0}), {1}));
        // (= (fac n) (* n (fac (- n 1))))
        space.add(expr!("=", ("fac", n), ({MUL}, n, ("fac", ({SUB}, n, {1})))));

        let expected: Bindings = [(VariableAtom::from("X"),
            expr!({MUL}, {3}, ("fac", ({SUB}, {3}, {1}))))].iter().cloned().collect();
        assert_eq!(space.query(&expr!("=", ("fac", {3}), X)), vec![expected]);
    }

    //#[test]
    fn test_factorial() {
        init();
        let mut space = GroundingSpace::new();
        // (= (fac n) (* n (fac (- n 1))))
        space.add(expr!("=", ("fac", n), ({MUL}, n, ("fac", ({SUB}, n, {1})))));
        // (= (fac 0) 1)
        space.add(expr!("=", ("fac", {0}), {1}));

        let expr = expr!("fac", {3});
        assert_eq!(interpret(Rc::new(space), &expr), Ok(vec![G(6)]));
    }
}
