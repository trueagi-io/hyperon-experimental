use crate::*;
use crate::common::*;

pub static SUM: &Operation = &Operation{ name: "+", execute: |args| bin_op(args, |a, b| a + b) };
pub static SUB: &Operation = &Operation{ name: "-", execute: |args| bin_op(args, |a, b| a - b) };
pub static MUL: &Operation = &Operation{ name: "*", execute: |args| bin_op(args, |a, b| a * b) };

fn bin_op(args: &mut Vec<Atom>, op: fn(i32, i32) -> i32) -> Result<Vec<Atom>, String> {
    // TODO: getting arguments from stack and checking their type can be
    // done in separate helper function or macros.
    let arg1 = args.get(0).ok_or_else(|| format!("Sum operation called without arguments"))?; 
    let arg2 = args.get(1).ok_or_else(|| format!("Sum operation called with only argument"))?;
    if let (Some(arg1), Some(arg2)) = (arg1.as_gnd::<i32>(), arg2.as_gnd::<i32>()) {
        Ok(vec![Atom::gnd(op(*arg1, *arg2))])
    } else {
        Err(format!("One of the arguments is not integer: ({}, {})", arg1, arg2))
    }
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

    use super::*;
    use crate::interpreter::*;
    // Aliases to have a shorter notation
    fn G<T: GroundedAtom>(value: T) -> Atom { Atom::gnd(value) }

    fn init_logger() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_sum_ints() {
        init_logger();
        let space = GroundingSpace::new();
        // (+ 3 5)
        let expr = expr!({SUM}, {3}, {5});

        assert_eq!(interpret(space, &expr), Ok(vec![G(8)]));
    }

    #[test]
    fn test_sum_ints_recursively() {
        init_logger();
        let space = GroundingSpace::new();
        // (+ 4 (+ 3 5))
        let expr = expr!({SUM}, {4}, ({SUM}, {3}, {5}));

        assert_eq!(interpret(space, &expr), Ok(vec![G(12)]));
    }

    #[test]
    fn test_match_factorial() {
        init_logger();
        let mut space = GroundingSpace::new();
        // (= (fac 0) 1)
        space.add(expr!("=", ("fac", {0}), {1}));
        // (= (fac n) (* n (fac (- n 1))))
        space.add(expr!("=", ("fac", n), ({MUL}, n, ("fac", ({SUB}, n, {1})))));

        let expected: Bindings = Bindings([(VariableAtom::from("X"),
            expr!({MUL}, {3}, ("fac", ({SUB}, {3}, {1}))))].iter().cloned().collect());
        assert_eq!(space.query(&expr!("=", ("fac", {3}), X)), vec![expected]);
    }

    // TODO: reimplement using grounded if to prevent infinite loop
    //#[test]
    fn test_factorial() {
        init_logger();
        let mut space = GroundingSpace::new();
        // (= (fac n) (* n (fac (- n 1))))
        space.add(expr!("=", ("fac", n), ({MUL}, n, ("fac", ({SUB}, n, {1})))));
        // (= (fac 0) 1)
        space.add(expr!("=", ("fac", {0}), {1}));

        let expr = expr!("fac", {3});
        assert_eq!(interpret(space, &expr), Ok(vec![G(6)]));
    }
}
