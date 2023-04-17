#![feature(test)]

extern crate test;

use test::Bencher;

use hyperon::*;
use hyperon::space::grounding::*;

fn space(size: isize) -> GroundingSpace {
    let mut space = GroundingSpace::new();
    for i in (0..size).step_by(1) {
        let func_sym = Atom::sym(format!("func-{:X}", i));
        let func_def = Atom::expr([Atom::sym("="),
            Atom::expr([func_sym, Atom::var("x")]),
            Atom::var("x")]);
        space.add(func_def);
    }
    space
}

#[bench]
fn query_x10(bencher: &mut Bencher) {
    let space = space(10);
    bencher.iter(|| {
        let res = space.query(&expr!("=" ("func-9" "arg") X));
        assert_eq!(res, bind_set![{ X: Atom::sym("arg") }]);
    })
}

#[bench]
fn query_x100(bencher: &mut Bencher) {
    let space = space(100);
    bencher.iter(|| {
        let res = space.query(&expr!("=" ("func-2A" "arg") X));
        assert_eq!(res, bind_set![{ X: Atom::sym("arg") }]);
    })
}
