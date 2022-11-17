use hyperon::*;
use hyperon::common::*;
use hyperon::metta::interpreter::*;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_types_in_metta() {
    let mut space = GroundingSpace::new();
    space.add(expr!("=" ("check" (":" n "Int")) ({IS_INT} n)));
    space.add(expr!("=" ("check" (":" n "Nat")) ({AND} ("check" (":" n "Int")) ({GT} n {0}))));
    space.add(expr!("=" ("if" {true} then else) then));
    space.add(expr!("=" ("if" {false} then else) else));
    space.add(expr!(":" "if" ("->" "bool" "Atom" "Atom" "Atom")));
    space.add(expr!("=" ("fac" n) ("if" ("check" (":" n "Nat")) ("if" ({EQ} n {1}) {1} ({MUL} n ("fac" ({SUB} n {1})))) ({ERR}))));

    assert_eq!(interpret(&space, &expr!("check" (":" {3} "Int"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {(-3)} "Int"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {3} "Nat"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {(-3)} "Nat"))), Ok(vec![expr!({false})]));
    assert_eq!(interpret(&space, &expr!("if" ("check" (":" {(3)} "Nat")) "ok" "nok")), Ok(vec![expr!("ok")]));
    assert_eq!(interpret(&space, &expr!("if" ("check" (":" {(-3)} "Nat")) "ok" "nok")), Ok(vec![expr!("nok")]));
    assert_eq!(interpret(&space, &expr!("fac" {1})), Ok(vec![expr!({1})]));
    assert_eq!(interpret(&space, &expr!("fac" {3})), Ok(vec![expr!({6})]));
}
