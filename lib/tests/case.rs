use hyperon::assert_eq_metta_results;
use hyperon::metta::text::SExprParser;
use hyperon::metta::runner::{Metta, EnvBuilder};

#[test]
fn test_case_operation() {
    let metta = Metta::new(Some(EnvBuilder::test_env()));
    let result = metta.run(SExprParser::new("
        ; cases are processed sequentially
        !(case (+ 1 5)
          ((5 Error)
           (6 OK)
           (6 Error)))

        ; we can use variables as cases
        !(case (+ 1 5)
          (($x (+ 1 $x))))

        ; it is non-deterministic: each value is matched against all cases
        !(case (+ 1 (superpose (1 2 3)))
          ((3 OK-3)
           (4 OK-4)))

        ; one case can produce multiple results
        !(case (+ 1 (superpose (1 2 3)))
          (($x (+ 1 $x))))

        ; cases are not necessarily exhaustive,
        ; and the result can be empty
        !(case 5
          ((6 OK)))
    "));
    let expected = metta.run(SExprParser::new("
        ! OK
        ! 7
        ! (superpose (OK-3 OK-4))
        ! (superpose (3 4 5))
        ! (superpose ())
    "));
    assert_eq!(result, expected);

    let metta = Metta::new(Some(EnvBuilder::test_env()));
    let result = metta.run(SExprParser::new("
        (Rel-P A B)
        (Rel-Q A C)

        ; cases can be used for deconstruction
        !(case (match &self ($rel A $x) ($rel $x))
            (((Rel-P $y) (P $y))
            ((Rel-Q $y) (Q $y))))

        ; %void% can be used to capture empty results
        !(case (match &self ($rel B $x) ($rel $x))
            (((Rel-P $y) (P $y))
            ((Rel-Q $y) (Q $y))
            (%void% no-match)))

        ; a functional example
        (= (maybe-inc $x)
            (case $x
            (((Just $v) (Just (+ 1 $v)))
                (Nothing Nothing)))
        )
        !(maybe-inc Nothing)
        !(maybe-inc (Just 2))
    "));
    let expected = metta.run(SExprParser::new("
        ! (superpose ((Q C) (P B)))
        ! no-match
        ! Nothing
        ! (Just 3)
    "));
    assert_eq_metta_results!(result, expected);
}
