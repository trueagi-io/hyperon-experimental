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
fn test_expr_self_expression() {
    assert_eq!(expr!("=", ("fact", n), ("*", n, ("-", n, "1"))), 
               E(&[S("="), E(&[S("fact"), V("n")]),
               E(&[ S("*"), V("n"), E(&[ S("-"), V("n"), S("1") ]) ]) ]));
}

#[test]
fn test_grounded_value() {
    assert_eq!(GroundedValue::new(3),
        Atom::Grounded(GroundedAtomHolder{ atom: Rc::new(GroundedValue{ x: 3 })}));
}

#[test]
fn test_match_symbol() {
    let mut space = GroundingSpace::new();
    space.add(expr!("foo"));
    assert_eq!(space.query(&expr!("foo")), vec![bind!{}])
}

#[test]
fn test_match_variable() {
    let mut space = GroundingSpace::new();
    space.add(expr!("foo"));
    assert_eq!(space.query(&expr!(x)), vec![bind!{x: expr!("foo")}])
}

#[test]
fn test_match_expression() {
    let mut space = GroundingSpace::new();
    space.add(expr!("+", "a", ("*", "b", "c")));
    assert_eq!(space.query(&expr!("+", "a", ("*", "b", "c"))), vec![bind!{}])
}

#[test]
fn test_match_expression_with_variables() {
    let mut space = GroundingSpace::new();
    space.add(expr!("+", "A", ("*", "B", "C")));
    assert_eq!(space.query(&expr!("+", a, ("*", b, c))),
        vec![bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }])
}

#[test]
fn test_match_different_value_for_variable() {
    let mut space = GroundingSpace::new();
    space.add(expr!("+", "A", ("*", "B", "C")));
    assert_eq!(space.query(&expr!("+", a, ("*", a, c))), vec![])
}

#[test]
fn test_match_variables_in_data() {
    assert_eq!(
        matcher::match_atoms(&expr!("+", a, ("*", b, c)), &expr!("+", "A", ("*", "B", "C"))),
        Some((bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }, bind!{})))
}

#[test]
fn test_match_different_value_for_variable_in_data() {
    assert_eq!(
        matcher::match_atoms(&expr!("+", a, ("*", a, c)), &expr!("+", "A", ("*", "B", "C"))),
        None)
}
