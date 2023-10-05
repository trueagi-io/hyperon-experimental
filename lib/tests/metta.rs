#[cfg(not(feature = "minimal"))]
use hyperon::metta::runner::stdlib::UNIT_ATOM;
#[cfg(feature = "minimal")]
use hyperon::metta::runner::stdlib2::UNIT_ATOM;
use hyperon::metta::text::*;
use hyperon::metta::runner::new_metta_rust;

#[test]
fn test_reduce_higher_order() {
    let program = "
        ; Curried plus
        (: plus (-> Number (-> Number Number)))
        (= ((plus $x) $y) (+ $x $y))
        ; Define inc as partial evaluation of plus
        (: inc (-> (-> Number Number)))
        (= (inc) (plus 1))

        !(assertEqualToResult ((inc) 2) (3))
    ";
    let metta = new_metta_rust();

    let result = metta.run(&mut SExprParser::new(program));

    assert_eq!(result, Ok(vec![vec![UNIT_ATOM()]]));
}
