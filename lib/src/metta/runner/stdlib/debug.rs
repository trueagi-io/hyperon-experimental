use crate::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::space::*;
use crate::common::collections::Equality;
use crate::common::assert::{vec_eq_no_order, compare_vec_no_order};
use crate::atom::matcher::atoms_are_equivalent;
use crate::metta::runner::stdlib::{grounded_op, atom_to_string, regex, interpret_no_error, unit_result};
use crate::metta::runner::bool::*;

use std::convert::TryInto;


fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
    let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
    match vec_eq_no_order(actual.iter(), expected.iter()) {
        None => unit_result(),
        Some(diff) => Err(ExecError::Runtime(format!("{}\n{}", report, diff)))
    }
}

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
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, Atom::var("a"), Atom::var("a")])
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

fn assert_alpha_equal(actual: &Vec<Atom>, expected: &Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
    let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
    let res = compare_vec_no_order(actual.iter(), expected.iter(), AlphaEquality{});
    match res.as_string() {
        None => unit_result(),
        Some(diff) => Err(ExecError::Runtime(format!("{}\n{}", report, diff)))
    }
}

#[derive(Clone, Debug)]
struct AssertResultsEqual {
}

grounded_op!(AssertResultsEqual, "assertResultsEqual");

impl Grounded for AssertResultsEqual {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertResultsEqual {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertResultsEqual::execute: {:?}", args);
        let arg_error = || ExecError::from("assertResultsEqual expects three arguments: two expressions (actual and expected) and atom (actual in form of atom) arguments");
        let actual: Vec<Atom> = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children().into();
        let expected: Vec<Atom> = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children().into();

        assert_results_equal(&actual, &expected)
    }
}


#[derive(Clone, Debug)]
struct AssertResultsAlphaEqual {
}

grounded_op!(AssertResultsAlphaEqual, "assertResultsAlphaEqual");

impl Grounded for AssertResultsAlphaEqual {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertResultsAlphaEqual {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertResultsAlphaEqual::execute: {:?}", args);
        let arg_error = || ExecError::from("assertResultsAlphaEqual expects three arguments: two expressions (actual and expected) and atom (actual in form of atom) arguments");
        let actual: Vec<Atom> = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children().into();
        let expected: Vec<Atom> = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children().into();

        assert_alpha_equal(&actual, &expected)
    }
}

#[derive(Clone, Debug)]
pub struct AlphaEqOp {
    space: DynSpace,
}

grounded_op!(AlphaEqOp, "=alpha");

impl AlphaEqOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for AlphaEqOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
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

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;
        let expected = interpret_no_error(self.space.clone(), expected_atom)?;
        Ok(vec![Atom::gnd(Bool(atoms_are_equivalent(actual.get(0).unwrap(), expected.get(0).unwrap())))])
    }
}

pub fn register_common_tokens(tref: &mut Tokenizer) {
    let trace_op = Atom::gnd(TraceOp{});
    tref.register_token(regex(r"trace!"), move |_| { trace_op.clone() });
    let print_alternatives_op = Atom::gnd(PrintAlternativesOp{});
    tref.register_token(regex(r"print-alternatives!"), move |_| { print_alternatives_op.clone() });
}

pub fn register_runner_tokens(tref: &mut Tokenizer, space: &DynSpace) {
    let assert_results_alpha_equal_op = Atom::gnd(AssertResultsAlphaEqual{});
    tref.register_token(regex(r"assertResultsAlphaEqual"), move |_| { assert_results_alpha_equal_op.clone() });
    let assert_results_equal_op = Atom::gnd(AssertResultsEqual{});
    tref.register_token(regex(r"assertResultsEqual"), move |_| { assert_results_equal_op.clone() });
    let alpha_eq_op = Atom::gnd(AlphaEqOp::new(space.clone()));
    tref.register_token(regex(r"=alpha"), move |_| { alpha_eq_op.clone() });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::{Metta, EnvBuilder, SExprParser};
    use crate::common::test_utils::metta_space;
    use crate::metta::runner::stdlib::tests::run_program;

    use regex::Regex;

    fn assert_runtime_error(actual: Result<Vec<Atom>, ExecError>, expected: Regex) {
        match actual {
            Err(ExecError::Runtime(msg)) => assert!(expected.is_match(msg.as_str()),
                                                    "Incorrect error message:\nexpected: {:?}\n  actual: {:?}", expected.to_string(), msg),
            _ => assert!(false, "Error is expected as result, {:?} returned", actual),
        }
    }

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
            vec![expr!("Error" ("assertEqual" ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed result: B")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ("assertEqual" ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive result: A")]
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
            vec![expr!("Error" ("assertAlphaEqual" ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed result: B")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ("assertAlphaEqual" ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive result: A")]
        ]));
    }

    #[test]
    fn metta_alpha_eq_op() {
        assert_eq!(run_program(&format!("(= (foo) (R $x $y)) !(=alpha (foo) (R $x $y))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("(= (foo) (R $x $y)) !(=alpha (foo) (R $x $x))")), Ok(vec![vec![expr!({Bool(false)})]]));
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
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (foo) (A B))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        // assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (bar) (A))")), Ok(vec![
        //     vec![expr!("Error" ("assertEqualToResult" ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed result: A")],
        // ]));
        // assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (baz) (D))")), Ok(vec![
        //     vec![expr!("Error" ("assertEqualToResult" ("baz") ("D")) "\nExpected: [D]\nGot: [D, D]\nExcessive result: D")]
        // ]));
    }

    // #[test]
    // fn assert_equal_op() {
    //     let space = DynSpace::new(metta_space("
    //         (= (foo) (A B))
    //         (= (foo) (B C))
    //         (= (bar) (B C))
    //         (= (bar) (A B))
    //         (= (err) (A B))
    //     "));
    //
    //     let assert_equal_op = AssertEqualOp::new(space);
    //
    //     assert_eq!(assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("bar"))]), unit_result());
    //
    //     let actual = assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("err"))]);
    //     let expected = Regex::new("\nExpected: \\[(A B)\\]\nGot: \\[\\((B C)|, |(A B)\\){3}\\]\nExcessive result: (B C)").unwrap();
    //     assert_runtime_error(actual, expected);
    //
    //     let actual = assert_equal_op.execute(&mut vec![expr!(("err")), expr!(("foo"))]);
    //     let expected = Regex::new("\nExpected: \\[\\((B C)|, |(A B)\\){3}\\]\nGot: \\[(A B)\\]\nMissed result: (B C)").unwrap();
    //     assert_runtime_error(actual, expected);
    // }

    // #[test]
    // fn assert_equal_to_result_op() {
    //     let space = DynSpace::new(metta_space("
    //         (= (foo) (A B))
    //         (= (foo) (B C))
    //     "));
    //     let assert_equal_to_result_op = AssertEqualToResultOp::new(space);
    //
    //     assert_eq!(assert_equal_to_result_op.execute(&mut vec![
    //         expr!(("foo")), expr!(("B" "C") ("A" "B"))]),
    //                unit_result());
    // }

    #[test]
    fn trace_op() {
        assert_eq!(TraceOp{}.execute(&mut vec![sym!("\"Here?\""), sym!("42")]),
                   Ok(vec![sym!("42")]));
    }
}
