use crate::*;
use crate::common::*;

macro_rules! def_op {
    ($x:ident, $o:tt, $t1:ty, $r:ty) => { pub static $x: &Operation =
            &Operation{ name: stringify!($o), execute: |args| bin_op::<$t1,$t1,$r>(args, |a, b| a $o b) }; };
}

def_op!(SUM, +, i32, i32);
def_op!(SUB, -, i32, i32);
def_op!(MUL, *, i32, i32);

def_op!(LT, <, i32, bool);
def_op!(GT, >, i32, bool);

def_op!(AND, &&, bool, bool);
def_op!(OR, ||, bool, bool);

pub static NOT: &Operation = &Operation{ name: "!", execute: |args| unary_op(args, |a: bool| !a) };

pub static IS_INT: &Operation = &Operation{ name: "int", execute: |args| check_type(args,
    // TODO: it is ugly, but I cannot do something more clear without downcasting
    |a| is_instance::<i32>(a) || is_instance::<u32>(a)
    || is_instance::<i64>(a) || is_instance::<u64>(a)
    || is_instance::<i128>(a) || is_instance::<u128>(a)
)};

pub static NOP: &Operation = &Operation{ name: "nop", execute: |_| Ok(vec![]) };

fn check_type(args: &mut Vec<Atom>, op: fn(&Atom) -> bool) -> Result<Vec<Atom>, String> {
    let arg = args.get(0).ok_or_else(|| format!("Unary operation called without arguments"))?; 
    Ok(vec![Atom::gnd(op(arg))])
}

fn is_instance<T>(arg: &Atom) -> bool
where
    T: GroundedAtom,
{
    matches!(arg.as_gnd::<T>(), Some(_))
}

fn unary_op<T, R>(args: &mut Vec<Atom>, op: fn(T) -> R) -> Result<Vec<Atom>, String>
where
    T: GroundedAtom + Copy,
    R: GroundedAtom,
{
    let arg = args.get(0).ok_or_else(|| format!("Unary operation called without arguments"))?; 
    if let Some(arg) = arg.as_gnd::<T>() {
        Ok(vec![Atom::gnd(op(*arg))])
    } else {
        Err(format!("Incorrect type of the unary operation argument: ({})", arg))
    }
}

fn bin_op<T1, T2, R>(args: &mut Vec<Atom>, op: fn(T1, T2) -> R) -> Result<Vec<Atom>, String>
where
    T1: GroundedAtom + Copy,
    T2: GroundedAtom + Copy,
    R: GroundedAtom,
{
    let arg1 = args.get(0).ok_or_else(|| format!("Binary operation called without arguments"))?; 
    let arg2 = args.get(1).ok_or_else(|| format!("Binary operation called with only argument"))?;
    if let (Some(arg1), Some(arg2)) = (arg1.as_gnd::<T1>(), arg2.as_gnd::<T2>()) {
        Ok(vec![Atom::gnd(op(*arg1, *arg2))])
    } else {
        Err(format!("Incorrect type of the binary operation argument: ({}, {})", arg1, arg2))
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

    #[test]
    fn test_factorial() {
        init_logger();
        let mut space = GroundingSpace::new();
        // NOTE: multiple matches are treated non-deterministically.
        // ATM, we don't have means to describe mutually exclusive ordered lists
        // of cases for recursive functions typical for FP. This code
        //   space.add(expr!("=", ("fac", n), ({MUL}, n, ("fac", ({SUB}, n, {1})))));
        //   space.add(expr!("=", ("fac", {0}), {1}));
        // should not work. For now, we have to resort to explicit
        // termination conditions:
        space.add(expr!("=", ("if", {true}, a, b), a));
        space.add(expr!("=", ("if", {false}, a, b), b));
        space.add(expr!("=", ("fac", n),
            ("if", ({GT}, n, {0}),
                   ({MUL}, n, ("fac", ({SUB}, n, {1}))),
                   {1})));

        let expr = expr!("fac", {3});
        assert_eq!(interpret(space, &expr), Ok(vec![G(6)]));
    }
}
