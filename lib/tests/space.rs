use hyperon::*;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_custom_match_with_space() {
    let mut main_space = GroundingSpace::new();
    let mut inserted_space = GroundingSpace::new();
    inserted_space.add(expr!("implies" ("B" x) ("C" x)));
    inserted_space.add(expr!("implies" ("A" x) ("B" x)));
    inserted_space.add(expr!("A" "Sam"));
    main_space.add(Atom::gnd(inserted_space));
    let result = main_space.query(&expr!("," ("implies" ("B" x) z) ("implies" ("A" x) y) ("A" x)));
    assert_eq!(result, vec![bind!{x: sym!("Sam"), y: expr!("B" "Sam"), z: expr!("C" "Sam")}]);
}
