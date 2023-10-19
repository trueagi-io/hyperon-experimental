
use crate::*;
use crate::metta::runner::{Metta, EnvBuilder};
use crate::space::grounding::GroundingSpace;

pub(crate) fn metta_space(text: &str) -> GroundingSpace {
    let metta = Metta::new(Some(EnvBuilder::test_env()));
    let atoms = metta.parse_all_atoms(text).unwrap();
    let mut space = GroundingSpace::new();
    atoms.into_iter().for_each(|atom| space.add(atom));
    space
}

pub(crate) fn metta_atom(atom_str: &str) -> Atom {
    let metta = Metta::new(Some(EnvBuilder::test_env()));
    metta.parse_one_atom(atom_str).expect("Single atom is expected")
}
