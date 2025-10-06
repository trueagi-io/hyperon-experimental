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

