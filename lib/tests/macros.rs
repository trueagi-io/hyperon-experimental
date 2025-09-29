use hyperon_atom::*;
use hyperon_macros::metta;
use hyperon::metta::runner::number::*;
use hyperon_atom::gnd::str::*;
use hyperon::metta::runner::bool::*;

#[test]
fn macros_metta_literal() {
    assert_eq!(metta!{1}, Atom::gnd(Number::Integer(1)));
    assert_eq!(metta!{-1}, Atom::gnd(Number::Integer(-1)));
    assert_eq!(metta!{+1}, Atom::gnd(Number::Integer(1)));
    assert_eq!(metta!{1.0}, Atom::gnd(Number::Float(1.0)));
    assert_eq!(metta!{-1.0}, Atom::gnd(Number::Float(-1.0)));
    assert_eq!(metta!{+1.0}, Atom::gnd(Number::Float(1.0)));
    assert_eq!(metta!{"text"}, Atom::gnd(Str::from_str("text")));
    assert_eq!(metta!{True}, Atom::gnd(Bool(true)));
    assert_eq!(metta!{False}, Atom::gnd(Bool(false)));
}

#[test]
fn macros_metta_literal_to_symbol() {
    assert_eq!(metta!{true123}, Atom::sym("true123"));
    assert_eq!(metta!{$true123}, Atom::var("true123"));
    assert_eq!(metta!{123+4}, Atom::sym("123+4"));
    assert_eq!(metta!{$123+4}, Atom::var("123+4"));
    assert_eq!(metta!{false-4}, Atom::sym("false-4"));
    assert_eq!(metta!{$false-4}, Atom::var("false-4"));
}

#[test]
fn macros_metta_literal_expression() {
    assert_eq!(metta!{(-1 $a (A False) "text")}, Atom::expr([
            Atom::gnd(Number::Integer(-1)), Atom::var("a"),
            Atom::expr([Atom::sym("A"), Atom::gnd(Bool(false))]),
            Atom::gnd(Str::from_str("text"))
    ]));
}

#[test]
fn macros_metta_gnd() {
    assert_eq!(metta!{{Number::Integer(42)}}, Atom::gnd(Number::Integer(42)));
}
