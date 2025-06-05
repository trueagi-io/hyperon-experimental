use hyperon_common::collections::ImmutableString;
use hyperon_atom::*;
use hyperon_atom::gnd::*;
use hyperon::metta::{UNIT_ATOM, ERROR_SYMBOL};
use hyperon::metta::text::*;
use hyperon::metta::runner::{Metta, EnvBuilder};
use hyperon_atom::str::atom_to_string;

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

fn run_metta_test(code: &[u8]) -> Result<(), String> {
    let metta = Metta::new(Some(EnvBuilder::test_env()));
    let result = metta.run(SExprParser::new(code));
    match result {
        Ok(result) => {
            for vec in result {
                for atom in vec {
                    match <&[Atom]>::try_from(&atom) {
                        Ok([op, atom, err]) if *op == ERROR_SYMBOL => {
                            let err = atom_to_string(err);
                            return Err(format!("{} returns error: {}", atom, err));
                        },
                        Ok([op, ..]) if *op == ERROR_SYMBOL => {
                            return Err(format!("incorrect error format: {}", atom));
                        },
                        _ => {},
                    }
                }
            }
            Ok(())
        },
        Err(err) => {
            Err(err)
        }
    }
}

macro_rules! run_metta_test {
    ($name:literal) => {
        match run_metta_test(include_bytes!($name)) {
            Ok(()) => {},
            Err(msg) => assert!(false, "Test {} fails: {}", $name, msg),
        }
    }
}

#[test]
fn test_run_metta() {
    run_metta_test!("test_stdlib.metta");
}
