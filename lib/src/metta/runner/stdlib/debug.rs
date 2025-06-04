use hyperon_atom::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use hyperon_common::collections::{SliceDisplay, Equality, DefaultEquality};
use hyperon_common::assert::compare_vec_no_order;
use hyperon_atom::matcher::atoms_are_equivalent;
use crate::metta::runner::stdlib::{grounded_op, regex, unit_result};
use crate::metta::runner::bool::*;
use crate::metta::runner::str::*;
use hyperon_atom::gnd::GroundedFunctionAtom;

use std::convert::TryInto;

/// Implement trace! built-in.
///
/// It is equivalent to Idris or Haskell Trace, that is, it prints a
/// message to stderr and pass a value along.
///
/// For instance
/// ```metta
/// !(trace! "Here?" 42)
/// ```
/// prints to stderr
/// ```stderr
/// Here?
/// ```
/// and returns
/// ```metta
/// [42]
/// ```
///
/// Note that the first argument does not need to be a string, which
/// makes `trace!` actually quite capable on its own.  For instance
/// ```metta
/// !(trace! ("Hello world!" (if True A B) 1 2 3) 42)
/// ```
/// prints to stderr
/// ```stderr
/// (Hello world! A 1 2 3)
/// ```
/// and returns
/// ```metta
/// [42]
/// ```

#[derive(Clone, Debug)]
pub struct TraceOp {}

grounded_op!(TraceOp, "trace!");

impl Grounded for TraceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for TraceOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("trace! expects two atoms as arguments");
        let val = args.get(1).ok_or_else(arg_error)?;
        let msg = args.get(0).ok_or_else(arg_error)?;
        eprintln!("{}", msg);
        Ok(vec![val.clone()])
    }
}

#[derive(Clone, Debug)]
pub struct PrintAlternativesOp {}

grounded_op!(PrintAlternativesOp, "print-alternatives!");

impl Grounded for PrintAlternativesOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for PrintAlternativesOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("print-alternatives! expects format string as a first argument and expression as a second argument");
        let atom = atom_to_string(args.get(0).ok_or_else(arg_error)?);
        let args = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?;
        let args: Vec<String> = args.children().iter()
            .map(|atom| atom_to_string(atom))
            .collect();
        println!("{} {}:", args.len(), atom);
        args.iter().for_each(|arg| println!("    {}", arg));
        Ok(vec![UNIT_ATOM])
    }
}

struct AlphaEquality{}

impl Equality<&Atom> for AlphaEquality {
    fn eq(a: &&Atom, b: &&Atom) -> bool {
        atoms_are_equivalent(*a, *b)
    }
}

fn assert_results_are_equal<'a, E: Equality<&'a Atom>>(args: &'a [Atom], cmp: E) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("Pair of evaluation results with bindings is expected as an argument");
    let actual = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children();
    let expected = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children();
    let assert = args.get(2).ok_or_else(arg_error)?;

    let report = format!("\nExpected: {}\nGot: {}", SliceDisplay(expected), SliceDisplay(actual));

    match compare_vec_no_order(actual.iter(), expected.iter(), cmp).as_display() {
        None => unit_result(),
        Some(diff) => {
            let msg = format!("{}\n{}", report, diff);
            Ok(vec![Atom::expr([ERROR_SYMBOL, assert.clone(), Atom::sym(msg)])])
        },
    }
}

#[derive(Clone, Debug)]
pub struct AlphaEqOp {
}
grounded_op!(AlphaEqOp, "=alpha");

impl Grounded for AlphaEqOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_BOOL])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AlphaEqOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AlphaEqOp::execute: {:?}", args);
        let arg_error = || ExecError::from("=alpha expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected_atom = args.get(1).ok_or_else(arg_error)?;

        Ok(vec![Atom::gnd(Bool(atoms_are_equivalent(actual_atom, expected_atom)))])
    }
}

pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    let trace_op = Atom::gnd(TraceOp{});
    tref.register_token(regex(r"trace!"), move |_| { trace_op.clone() });
    let print_alternatives_op = Atom::gnd(PrintAlternativesOp{});
    tref.register_token(regex(r"print-alternatives!"), move |_| { print_alternatives_op.clone() });
    let alpha_eq_op = Atom::gnd(AlphaEqOp{});
    tref.register_token(regex(r"=alpha"), move |_| { alpha_eq_op.clone() });
    tref.register_function(GroundedFunctionAtom::new(
            r"_assert-results-are-equal".into(),
            expr!("->" "Atom" "Atom" "Atom" ("->")),
            |args: &[Atom]| -> Result<Vec<Atom>, ExecError> { assert_results_are_equal(args, DefaultEquality{}) }, 
            ));
    tref.register_function(GroundedFunctionAtom::new(
            r"_assert-results-are-alpha-equal".into(),
            expr!("->" "Atom" "Atom" "Atom" ("->")),
            |args: &[Atom]| -> Result<Vec<Atom>, ExecError> { assert_results_are_equal(args, AlphaEquality{}) }, 
            ));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::{Metta, EnvBuilder, SExprParser};
    use crate::metta::runner::run_program;

    #[test]
    fn metta_assert_equal_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let program = "
            (= (foo $x) $x)
            (= (bar $x) $x)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar A))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar B))")), Ok(vec![
            vec![expr!("Error" ("assertEqual" ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed results: B\nExcessive results: A")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ("assertEqual" ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive results: A")]
        ]));
    }

    #[test]
    fn metta_assert_alpha_equal_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let program = "
            (= (foo $x) $x)
            (= (bar $x) $x)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqual (foo $x) (bar $x))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqual (foo A) (bar B))")), Ok(vec![
            vec![expr!("Error" ("assertAlphaEqual" ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed results: B\nExcessive results: A")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ("assertAlphaEqual" ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive results: A")]
        ]));
    }

    #[test]
    fn metta_alpha_eq_op() {
        assert_eq!(run_program(&format!("(= (foo) (R $x $y)) !(let $foo (eval (foo)) (=alpha $foo (R $x $y)))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("(= (foo) (R $x $y)) !(let $foo (eval (foo)) (=alpha $foo (R $x $x)))")), Ok(vec![vec![expr!({Bool(false)})]]));
    }

    #[test]
    fn metta_assert_equal_to_result_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let program = "
            (= (foo) A)
            (= (foo) B)
            (= (bar) C)
            (= (baz) D)
            (= (baz) D)
            (= (baz) D)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (foo) (A B))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (bar) (A))")), Ok(vec![
            vec![expr!("Error" ("assertEqualToResult" ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed results: A\nExcessive results: C")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (baz) (D))")), Ok(vec![
            vec![expr!("Error" ("assertEqualToResult" ("baz") ("D")) "\nExpected: [D]\nGot: [D, D, D]\nExcessive results: D, D")]
        ]));
    }

    #[test]
    fn metta_assert_alpha_equal_to_result_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let program = "
            (= (foo) $x)
            (= (bar) C)
            (= (baz) D)
            (= (baz) D)
            (= (baz) D)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqualToResult (foo) ($x))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqualToResult ((foo) (foo)) (($x $y)))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));

        let res = metta.run(SExprParser::new("!(assertAlphaEqualToResult ((foo) (foo)) (($x $x)))")).unwrap();
        let res_first_atom = res.get(0).unwrap().get(0);
        assert_eq!(res_first_atom.unwrap().iter().next().unwrap(), &sym!("Error"));
        assert_eq!(res.get(0).unwrap().len(), 1);

        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqualToResult (bar) (A))")), Ok(vec![
            vec![expr!("Error" ("assertAlphaEqualToResult" ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed results: A\nExcessive results: C")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqualToResult (baz) (D))")), Ok(vec![
            vec![expr!("Error" ("assertAlphaEqualToResult" ("baz") ("D")) "\nExpected: [D]\nGot: [D, D, D]\nExcessive results: D, D")]
        ]));
    }

    #[test]
    fn trace_op() {
        assert_eq!(TraceOp{}.execute(&mut vec![sym!("\"Here?\""), sym!("42")]),
                   Ok(vec![sym!("42")]));
    }
}
