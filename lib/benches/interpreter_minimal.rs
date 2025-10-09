#![feature(test)]
mod interpreter_bench {

extern crate test;

use hyperon_atom::{expr, Atom};
use test::Bencher;

use hyperon_space::DynSpace;
use hyperon::space::grounding::*;
use hyperon::metta::*;
use hyperon::metta::interpreter::*;

fn chain_atom(size: isize) -> Atom {
    let mut atom = Atom::expr([CHAIN_SYMBOL, Atom::sym("A"), Atom::var("x"), Atom::var("x")]);
    for _i in (1..size).step_by(1) {
        atom = Atom::expr([CHAIN_SYMBOL, atom, Atom::var("x"), Atom::var("x")])
    }
    atom
}

fn new_space() -> DynSpace {
    GroundingSpace::new().into()
}


#[bench]
fn chain_x10(bencher: &mut Bencher) {
    let space = new_space();
    let atom = chain_atom(10);
    let expected = Ok(vec![expr!("A")]);
    bencher.iter(move || {
        let res = interpret(space.clone(), &atom);
        assert_eq!(res, expected);
    })
}

#[bench]
fn chain_x100(bencher: &mut Bencher) {
    let space = new_space();
    let atom = chain_atom(100);
    let expected = Ok(vec![expr!("A")]);
    bencher.iter(move || {
        let res = interpret(space.clone(), &atom);
        assert_eq!(res, expected);
    })
}

#[bench]
fn chain_x1000(bencher: &mut Bencher) {
    let space = new_space();
    let atom = chain_atom(1000);
    let expected = Ok(vec![expr!("A")]);
    bencher.iter(move || {
        let res = interpret(space.clone(), &atom);
        assert_eq!(res, expected);
    })
}

}
