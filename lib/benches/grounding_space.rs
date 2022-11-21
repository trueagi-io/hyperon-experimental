#![feature(test)]

extern crate test;

use test::Bencher;

use hyperon::*;
use hyperon::space::grounding::*;

fn space(size: isize) -> GroundingSpace {
    let mut space = GroundingSpace::new();
    for i in (0..size).step_by(1) {
        space.add(expr!("A" {i}));
    }
    space
}

#[bench]
fn query_x10(bencher: &mut Bencher) {

    let space = space(10);
    bencher.iter(|| space.query(&expr!("A" {0})))
}

#[bench]
fn query_x100(bencher: &mut Bencher) {
    let space = space(100);

    bencher.iter(|| space.query(&expr!("A" {0})))
}
