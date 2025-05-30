use hyperon_common::collections::ImmutableString;
use hyperon_atom::*;
use hyperon_atom::gnd::*;
use hyperon::metta::UNIT_ATOM;
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

    assert_eq!(result, Ok(vec![vec![UNIT_ATOM]]));
}

fn execute_expr(expr: Atom) -> Result<Vec<Vec<Atom>>, String> {
    let metta = Metta::new(Some(EnvBuilder::test_env()));
    metta.run([Atom::sym("!"), expr].as_slice())
}

#[test]
fn test_lambda_into_grounded_atom() {
    let f = move |_args: &[Atom]| -> Result<Vec<Atom>, ExecError> {
        Ok(vec![Atom::sym("some-symbol")])
    };

    let op = GroundedFunctionAtom::new("get-some-symbol".into(), expr!("->" "%Undefined%"), f);

    assert_eq!(execute_expr(Atom::expr([Atom::gnd(op)])),
        Ok(vec![vec![Atom::sym("some-symbol")]]));
}

#[test]
fn test_function_into_grounded_atom() {
    fn f(_args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        Ok(vec![Atom::sym("some-symbol")])
    }

    let op = GroundedFunctionAtom::new("get-some-symbol".into(), expr!("->" "%Undefined%"), f);

    assert_eq!(execute_expr(Atom::expr([Atom::gnd(op)])),
        Ok(vec![vec![Atom::sym("some-symbol")]]));
}

#[test]
fn test_method_into_grounded_atom() {
    struct S {
        sym: ImmutableString,
    }

    impl GroundedFunction for S {
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![Atom::sym(self.sym.as_str())])
        }
    }

    let op = GroundedFunctionAtom::new("get-some-symbol".into(), expr!("->" "%Undefined%"), S{ sym: "some-symbol".into() });

    assert_eq!(execute_expr(Atom::expr([Atom::gnd(op)])),
        Ok(vec![vec![Atom::sym("some-symbol")]]));
}
