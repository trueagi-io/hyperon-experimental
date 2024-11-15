#![feature(test)]
mod interpreter_minimal_bench {

extern crate test;

use test::Bencher;

use hyperon::*;
use hyperon::space::grounding::*;
use hyperon::metta::*;
use hyperon::metta::interpreter_minimal::*;

fn chain_atom(size: isize) -> Atom {
    let mut atom = Atom::expr([CHAIN_SYMBOL, Atom::sym("A"), Atom::var("x"), Atom::var("x")]);
    for _i in (1..size).step_by(1) {
        atom = Atom::expr([CHAIN_SYMBOL, atom, Atom::var("x"), Atom::var("x")])
    }
    atom
}


#[bench]
fn chain_x10(bencher: &mut Bencher) {
    let space = GroundingSpace::new();
    let atom = chain_atom(10);
    let expected = Ok(vec![expr!("A")]);
    bencher.iter(move || {
        let res = interpret(&space, &atom);
        assert_eq!(res, expected);
    })
}

#[bench]
fn chain_x100(bencher: &mut Bencher) {
    let space = GroundingSpace::new();
    let atom = chain_atom(100);
    let expected = Ok(vec![expr!("A")]);
    bencher.iter(move || {
        let res = interpret(&space, &atom);
        assert_eq!(res, expected);
    })
}

#[bench]
fn chain_x1000(bencher: &mut Bencher) {
    let space = GroundingSpace::new();
    let atom = chain_atom(1000);
    let expected = Ok(vec![expr!("A")]);
    bencher.iter(move || {
        let res = interpret(&space, &atom);
        assert_eq!(res, expected);
    })
}

}
