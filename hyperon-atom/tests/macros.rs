use hyperon_atom::*;
use hyperon_macros::metta;

#[test]
fn macros_metta_symbol() {
    assert_eq!(metta!{A}, Atom::sym("A"));
}

#[test]
fn macros_metta_variable() {
    assert_eq!(metta!{$a}, Atom::var("a"));
    assert_eq!(metta!{$a+}, Atom::var("a+"));
}

#[test]
fn macros_metta_expression() {
    assert_eq!(metta!{()}, Atom::expr([]));
    assert_eq!(metta!{(A $a)}, Atom::expr([Atom::sym("A"), Atom::var("a")]));
    assert_eq!(metta!{(A ($b B) $a)}, Atom::expr([
        Atom::sym("A"),
        Atom::expr([Atom::var("b"), Atom::sym("B")]),
        Atom::var("a")
    ]));
}
