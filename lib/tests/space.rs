use hyperon_atom::*;
use hyperon_space::DynSpace;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_custom_match_with_space() {
    let mut main_space = GroundingSpace::new();
    let inserted_space = DynSpace::new(GroundingSpace::new());
    inserted_space.borrow_mut().add(expr!("implies" ("B" x) ("C" x)));
    inserted_space.borrow_mut().add(expr!("implies" ("A" x) ("B" x)));
    inserted_space.borrow_mut().add(expr!("A" "Sam"));
    main_space.add(Atom::gnd(inserted_space));
    let result = main_space.query(&expr!("," ("implies" ("B" x) z) ("implies" ("A" x) y) ("A" x)));
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].resolve(&VariableAtom::new("y")), Some(expr!("B" "Sam")));
    assert_eq!(result[0].resolve(&VariableAtom::new("z")), Some(expr!("C" "Sam")));
}
