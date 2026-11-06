use hyperon_atom::*;
use hyperon::metta::*;
use hyperon::metta::text::*;
use hyperon::metta::runner::{Metta, EnvBuilder};
use hyperon_atom::gnd::number::Number;
use hyperon_atom::gnd::bool::Bool;

#[test]
fn test_types_in_metta() {
    let program = "
        (= (check (: $n Int)) (is-int $n))
        (= (check (: $n Nat)) (and (is-int $n) (> $n 0)))

        (= (fac $n) (if (check (: $n Nat))
            (if (== $n 1) 1 (* $n (fac (- $n 1))))
            (Error (fac $n) BadType) ))

        !(assertEqual (check (: 3 Int)) True)
        !(assertEqual (check (: -3 Int)) True)
        !(assertEqual (check (: 3 Nat)) True)
        !(assertEqual (check (: -3 Nat)) False)
        !(assertEqual (fac 1) 1)
        !(assertEqual (fac 3) 6)
    ";

    let metta = Metta::new(Some(EnvBuilder::test_env()));
    metta.tokenizer().borrow_mut().register_token(regex::Regex::new("is-int").unwrap(), |_t| Atom::gnd(IsInt{}));
    let result = metta.run(SExprParser::new(program));
    assert_eq!(result, Ok(vec![vec![UNIT_ATOM], vec![UNIT_ATOM], vec![UNIT_ATOM], vec![UNIT_ATOM], vec![UNIT_ATOM], vec![UNIT_ATOM]]));
}

#[test]
fn test_get_type_rejects_badly_typed_argument() {
    let program = r#"
        (: typed-outer (-> $t Number))
        (: raw-expr (-> Expression Number))
        (: unchecked (-> %Undefined% Number))

        !(assertEqualToResult (get-type (typed-outer (+ 5 "S"))) ())
        !(assertEqualToResult (get-type (raw-expr (+ 5 "S"))) (Number))
        !(assertEqualToResult (get-type (unchecked (+ 5 "S"))) (Number))
        !(assertEqualToResult (get-type (typed-outer untyped)) (Number))
        !(assertEqualToResult (typed-outer (+ 5 "S"))
            ((Error (+ 5 "S") (BadArgType 2 Number String))))

        !(pragma! type-check auto)
        !(assertEqualToResult (get-type (typed-outer (+ 5 "S"))) ())
        !(assertEqualToResult (typed-outer (+ 5 "S"))
            ((Error (+ 5 "S") (BadArgType 2 Number String))))
    "#;

    let metta = Metta::new(Some(EnvBuilder::test_env()));
    let result = metta.run(SExprParser::new(program));
    assert_eq!(result, Ok(vec![vec![UNIT_ATOM]; 8]));
}

#[derive(Clone, Debug)]
struct IsInt{}

impl PartialEq for IsInt {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl std::fmt::Display for IsInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "is-int")
    }
}

impl Grounded for IsInt {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, Atom::sym("Bool")])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IsInt {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg = args.get(0).ok_or_else(|| format!("Unary operation called without arguments"))?; 
        let is_int = match arg.as_gnd::<Number>() {
            Some(Number::Integer(_)) => true,
            _ => false,
        };
        Ok(vec![Atom::gnd(Bool(is_int))])
    }
}

