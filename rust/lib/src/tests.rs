#![allow(non_snake_case)]

use super::*;

// Aliases to have a shorter notation
fn S(name: &str) -> Atom { Atom::sym(name) }
fn E(children: &[Atom]) -> Atom { Atom::expr(children) }
fn V(name: &str) -> Atom { Atom::var(name) }

// Make Bindings map from list of (k, v) pairs
macro_rules! bind {
    ($($k:ident: $v:expr),*) => { vec![$( (VariableAtom::from(stringify!($k)), $v), )*]
        .iter().cloned().collect() };
}

#[test]
fn test_expr_symbol() {
    assert_eq!(expr!("="), S("="));
    assert_eq!(expr!("1"), S("1"));
    assert_eq!(expr!("*"), S("*"));
    assert_eq!(expr!("foo"), S("foo"));
}

#[test]
fn test_expr_variable() {
    assert_eq!(expr!(n), V("n"));
    assert_eq!(expr!(self), V("self"));
}

#[test]
fn test_expr_expression() {
    assert_eq!(expr!("=", ("fact", n), ("*", n, ("-", n, "1"))), 
               E(&[S("="), E(&[S("fact"), V("n")]),
               E(&[ S("*"), V("n"), E(&[ S("-"), V("n"), S("1") ]) ]) ]));
}

#[test]
fn test_grounded() {
    assert_eq!(Atom::gnd(3), Atom::Grounded(Box::new(3)));
}

#[test]
fn test_display_symbol() {
    assert_eq!(format!("{}", Atom::sym("test")), "test");
}

#[test]
fn test_display_variable() {
    assert_eq!(format!("{}", Atom::var("x")), "$x");
}

#[test]
fn test_display_expression() {
    assert_eq!(format!("{}", expr!("=", ("fact", n), ("*", n, ("-", n, "1")))),
        "(= (fact $n) (* $n (- $n 1)))");
    assert_eq!(format!("{}", expr!()), "()");
}

#[test]
fn test_display_grounded() {
    assert_eq!(format!("{}", Atom::gnd(42)), "42");
}

#[test]
fn test_match_symbol() {
    let mut space = GroundingSpace::new();
    space.add(expr!("foo"));
    assert_eq!(space.query(&expr!("foo")), vec![bind!{}]);
}

#[test]
fn test_match_variable() {
    let mut space = GroundingSpace::new();
    space.add(expr!("foo"));
    assert_eq!(space.query(&expr!(x)), vec![bind!{x: expr!("foo")}]);
}

#[test]
fn test_match_expression() {
    let mut space = GroundingSpace::new();
    space.add(expr!("+", "a", ("*", "b", "c")));
    assert_eq!(space.query(&expr!("+", "a", ("*", "b", "c"))), vec![bind!{}]);
}

#[test]
fn test_match_expression_with_variables() {
    let mut space = GroundingSpace::new();
    space.add(expr!("+", "A", ("*", "B", "C")));
    assert_eq!(space.query(&expr!("+", a, ("*", b, c))),
        vec![bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }]);
}

#[test]
fn test_match_different_value_for_variable() {
    let mut space = GroundingSpace::new();
    space.add(expr!("+", "A", ("*", "B", "C")));
    assert_eq!(space.query(&expr!("+", a, ("*", a, c))), vec![]);
}

#[test]
fn test_match_variables_in_data() {
    assert_eq!(
        matcher::match_atoms(&expr!("+", a, ("*", b, c)), &expr!("+", "A", ("*", "B", "C"))),
        Some((bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }, bind!{})));
}

#[test]
fn test_match_different_value_for_variable_in_data() {
    assert_eq!(
        matcher::match_atoms(&expr!("+", a, ("*", a, c)), &expr!("+", "A", ("*", "B", "C"))),
        None);
}

#[test]
fn test_subexpression_iterator() {
    // (+ (* 3 (+ 1 1)) (- 4 3))
    let plus11 = ExpressionAtom::from(&[S("+"), S("1"), V("n")]);
    let mul3plus11 = ExpressionAtom::from(&[S("*"), S("3"), Atom::Expression(plus11.clone())]);
    let minus43 = ExpressionAtom::from(&[S("-"), S("4"), S("3")]);
    let expr = ExpressionAtom::from(&[S("+"), Atom::Expression(mul3plus11.clone()), Atom::Expression(minus43.clone())]);

    let iter = ExpressionAtomIter::from(&expr);

    assert_eq!(iter
        .map(|(a, p, i)| (a.clone(), p.clone(), i))
        .collect::<Vec<_>>(), vec![
            (plus11, mul3plus11.clone(), 2),
            (mul3plus11, expr.clone(), 1),
            (minus43, expr, 2),
        ]);
}
