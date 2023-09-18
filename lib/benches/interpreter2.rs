#![feature(test)]

extern crate test;

use test::Bencher;

use hyperon::*;
use hyperon::space::grounding::*;
use hyperon::metta::interpreter2::*;
use hyperon::metta::*;

fn chain_atom(size: isize) -> Atom {
    let mut atom = Atom::expr([CHAIN_SYMBOL, Atom::sym("A"), Atom::var("x"), Atom::var("x")]);
    for _i in (1..size).step_by(1) {
        atom = Atom::expr([CHAIN_SYMBOL, atom, Atom::var("x"), Atom::var("x")])
    }
    atom
}

#[bench]
fn chain_x100(bencher: &mut Bencher) {
    let atom = chain_atom(100);
    let expected = Ok(vec![expr!("A")]);
    bencher.iter(|| {
        let space = GroundingSpace::new();
        let res = interpret(space, &atom);
        assert_eq!(res, expected);
    })
}
