#![feature(test)]

extern crate test;

use test::Bencher;

use hyperon::metta::runner::*;
use hyperon::metta::text::SExprParser;
use hyperon::atom::Atom;

// TODO: use a better benchmark framework with parameterization or somehow else get rid of copypaste code

/* 
This benchmark is intended to compare the time of querying and changing entries
in the space with States and remove/add atoms. The problem with querying states
is that they are wrapped in the grounded atoms, which may not be indexed
(which is the case now), while removeing/adding atoms has higher overheads.
Both techniques can be improved in the future, so the benchmark is intended to
keep track of such possible changes.
 */

fn metta_state(size: isize) -> Metta {
    let mut metta = new_metta_rust();
    let program = "
        ! (bind! &data (new-space))
        (= (new-entry! $key $value)
           (let $new-state (new-state $value)
                (add-atom &data (= (get-data $key) $new-state))
        ))
    ";
    metta.run(&mut SExprParser::new(program));
    for i in (0..size).step_by(1) {
        metta.run(&mut SExprParser::new((format!("! (new-entry! k-{:X} v-{:X})", i, i)).as_str()));
    }
    metta
}

fn metta_atoms(size: isize) -> Metta {
    let mut metta = new_metta_rust();
    let program = "
        ! (bind! &data (new-space))
        (= (new-entry! $key $value)
           (add-atom &data (= (get-data $key) $value)
        ))
    ";
    metta.run(&mut SExprParser::new(program));
    for i in (0..size).step_by(1) {
        metta.run(&mut SExprParser::new((format!("! (new-entry! k-{:X} v-{:X})", i, i)).as_str()));
    }
    metta
}

#[bench]
fn query_state_x10(bencher: &mut Bencher) {
    let size = 10;
    let metta = metta_state(size);
    bencher.iter(|| {
        let i = size / 2;
        // Retrieval of states by value
        let res = metta.run(&mut SExprParser::new(
            (format!("! (let $v (new-state v-{:X})
                            (match &data (= (get-data $x) $v) $x))", i)).as_str()
        ));
        assert_eq!(res, Ok(vec![vec![Atom::sym(format!("k-{:X}", i))]]))
    })
}

#[bench]
fn query_state_x50(bencher: &mut Bencher) {
    let size = 50;
    let metta = metta_state(size);
    bencher.iter(|| {
        let i = size / 2;
        // It is expected to be dependent on the space size
        let res = metta.run(&mut SExprParser::new(
            (format!("! (let $v (new-state v-{:X})
                            (match &data (= (get-data $x) $v) $x))", i)).as_str()
        ));
        assert_eq!(res, Ok(vec![vec![Atom::sym(format!("k-{:X}", i))]]))
    })
}

#[bench]
fn change_state_x10(bencher: &mut Bencher) {
    let size = 10;
    let metta = metta_state(size);
    // States are changed using on keys, so value-based retrieval is not involved
    bencher.iter(|| {
        let i = size / 2;
        metta.run(&mut SExprParser::new(
            (format!("! (change-state! (match &data (= (get-data k-{:X}) $x) $x) v-{:X})", i, i*2)).as_str()
        ));
    });
    let res = metta.run(&mut SExprParser::new(
        (format!("! (get-state (match &data (= (get-data k-{:X}) $x) $x))", size / 2)).as_str()
    ));
    assert_eq!(res, Ok(vec![vec![Atom::sym(format!("v-{:X}", size))]]))
}

#[bench]
fn change_state_x50(bencher: &mut Bencher) {
    let size = 50;
    let metta = metta_state(size);
    bencher.iter(|| {
        let i = size / 2;
        // Changing states is expected to be almost independent on the space size
        metta.run(&mut SExprParser::new(
            (format!("! (change-state! (match &data (= (get-data k-{:X}) $x) $x) v-{:X})", i, i*2)).as_str()
        ));
    });
    let res = metta.run(&mut SExprParser::new(
        (format!("! (get-state (match &data (= (get-data k-{:X}) $x) $x))", size / 2)).as_str()
    ));
    assert_eq!(res, Ok(vec![vec![Atom::sym(format!("v-{:X}", size))]]))
}

#[bench]
fn query_atom_x10(bencher: &mut Bencher) {
    let size = 10;
    let metta = metta_atoms(size);
    bencher.iter(|| {
        let i = size / 2;
        // Querying atoms by value should be as fast as by key
        let res = metta.run(&mut SExprParser::new(
            (format!("!(match &data (= (get-data $x) v-{:X}) $x)", i)).as_str()
        ));
        assert_eq!(res, Ok(vec![vec![Atom::sym(format!("k-{:X}", i))]]))
    })
}

#[bench]
fn query_atom_x50(bencher: &mut Bencher) {
    let size = 50;
    let metta = metta_atoms(size);
    bencher.iter(|| {
        let i = size / 2;
        // It should be almost independent on the space size
        let res = metta.run(&mut SExprParser::new(
            (format!("!(match &data (= (get-data $x) v-{:X}) $x)", i)).as_str()
        ));
        assert_eq!(res, Ok(vec![vec![Atom::sym(format!("k-{:X}", i))]]))
    })
}

#[bench]
fn change_atom_x10(bencher: &mut Bencher) {
    let size = 10;
    let metta = metta_atoms(size);
    bencher.iter(|| {
        let i = size / 2;
        // Replacing atoms has some overheads
        metta.run(&mut SExprParser::new(
            (format!("! (match &data (= (get-data k-{:X}) $x)
                           (remove-atom &data (= (get-data k-{:X}) $x)))", i, i)).as_str()
        ));
        metta.run(&mut SExprParser::new(
            (format!("! (new-entry! k-{:X} v-{:X})", i, i*2)).as_str()
        ));
    });
    let res = metta.run(&mut SExprParser::new(
        (format!("! (match &data (= (get-data k-{:X}) $x) $x)", size / 2)).as_str()
    ));
    assert_eq!(res, Ok(vec![vec![Atom::sym(format!("v-{:X}", size))]]))
}

#[bench]
fn change_atom_x50(bencher: &mut Bencher) {
    let size = 50;
    let metta = metta_atoms(size);
    bencher.iter(|| {
        let i = size / 2;
        // It may be not too dependent on the space size, but maybe
        // some shuffling of atoms under replacement is needed
        metta.run(&mut SExprParser::new(
            (format!("! (match &data (= (get-data k-{:X}) $x)
                           (remove-atom &data (= (get-data k-{:X}) $x)))", i, i)).as_str()
        ));
        metta.run(&mut SExprParser::new(
            (format!("! (new-entry! k-{:X} v-{:X})", i, i*2)).as_str()
        ));
    });
    let res = metta.run(&mut SExprParser::new(
        (format!("! (match &data (= (get-data k-{:X}) $x) $x)", size / 2)).as_str()
    ));
    assert_eq!(res, Ok(vec![vec![Atom::sym(format!("v-{:X}", size))]]))
}
