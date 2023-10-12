#[cfg(not(feature = "minimal"))]
use hyperon::metta::runner::stdlib::UNIT_ATOM;
#[cfg(feature = "minimal")]
use hyperon::metta::runner::stdlib2::UNIT_ATOM;
use hyperon::metta::text::*;
use hyperon::metta::runner::{Metta, EnvBuilder};

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
    let metta = Metta::new(Some(EnvBuilder::test_env()));

    let result = metta.run(SExprParser::new(program));

    assert_eq!(result, Ok(vec![vec![UNIT_ATOM()]]));
}
