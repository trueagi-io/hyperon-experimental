use crate::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::space::*;
use crate::common::collections::{VecDisplay, Equality, DefaultEquality};
use crate::common::assert::compare_vec_no_order;
use crate::atom::matcher::atoms_are_equivalent;
use crate::metta::runner::stdlib::{grounded_op, regex, interpret_no_error, unit_result};
use crate::metta::runner::bool::*;
use crate::metta::runner::str::atom_to_string;
use crate::metta::runner::{Metta, PragmaSettings};

use std::convert::TryInto;

fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
    let report = format!("\nExpected: {}\nGot: {}", VecDisplay(expected), VecDisplay(actual));
    match compare_vec_no_order(actual.iter(), expected.iter(), DefaultEquality{}).as_display() {
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

fn assert_alpha_equal(actual: &Vec<Atom>, expected: &Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
    let report = format!("\nExpected: {}\nGot: {}", VecDisplay(expected), VecDisplay(actual));
    let res = compare_vec_no_order(actual.iter(), expected.iter(), AlphaEquality{});
    match res.as_display() {
        None => unit_result(),
        Some(diff) => Err(ExecError::Runtime(format!("{}\n{}", report, diff)))
    }
}

#[derive(Clone, Debug)]
pub struct AssertEqualOp {
    space: DynSpace,
    settings: PragmaSettings,
}

grounded_op!(AssertEqualOp, "assertEqual");

impl AssertEqualOp {
    pub fn new(space: DynSpace, settings: PragmaSettings) -> Self {
        Self{ space, settings }
    }
}

impl Grounded for AssertEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertEqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertEqualOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertEqual expects two atoms: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected_atom = args.get(1).ok_or_else(arg_error)?;

        let actual = interpret_no_error(self.space.clone(), actual_atom, self.settings.clone())?;
        let expected = interpret_no_error(self.space.clone(), expected_atom, self.settings.clone())?;

        assert_results_equal(&actual, &expected)
    }
}

#[derive(Clone, Debug)]
pub struct AssertAlphaEqualOp {
    space: DynSpace,
    settings: PragmaSettings,
}

grounded_op!(AssertAlphaEqualOp, "assertAlphaEqual");

impl AssertAlphaEqualOp {
    pub fn new(space: DynSpace, settings: PragmaSettings) -> Self {
        Self{ space, settings }
    }
}

impl Grounded for AssertAlphaEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertAlphaEqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertAlphaEqualOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertAlphaEqual expects two atoms: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected_atom = args.get(1).ok_or_else(arg_error)?;

        let actual = interpret_no_error(self.space.clone(), actual_atom, self.settings.clone())?;
        let expected = interpret_no_error(self.space.clone(), expected_atom, self.settings.clone())?;

        assert_alpha_equal(&actual, &expected)
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

#[derive(Clone, Debug)]
pub struct AssertEqualToResultOp {
    space: DynSpace,
    settings: PragmaSettings,
}

grounded_op!(AssertEqualToResultOp, "assertEqualToResult");

impl AssertEqualToResultOp {
    pub fn new(space: DynSpace, settings: PragmaSettings) -> Self {
        Self{ space, settings }
    }
}

impl Grounded for AssertEqualToResultOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertEqualToResultOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertEqualToResultOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertEqualToResult expects atom and expression as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)
            .map_err(|_| arg_error())?
            .children();

        let actual = interpret_no_error(self.space.clone(), actual_atom, self.settings.clone())?;

        assert_results_equal(&actual, &expected.into())
    }
}

#[derive(Clone, Debug)]
pub struct AssertAlphaEqualToResultOp {
    space: DynSpace,
    settings: PragmaSettings,
}

grounded_op!(AssertAlphaEqualToResultOp, "assertAlphaEqualToResult");

impl AssertAlphaEqualToResultOp {
    pub fn new(space: DynSpace, settings: PragmaSettings) -> Self {
        Self{ space, settings }
    }
}

impl Grounded for AssertAlphaEqualToResultOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertAlphaEqualToResultOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertAlphaEqualToResultOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertAlphaEqualToResultOp expects atom and expression as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)
            .map_err(|_| arg_error())?
            .children();

        let actual = interpret_no_error(self.space.clone(), actual_atom, self.settings.clone())?;

        assert_alpha_equal(&actual, &expected.into())
    }
}


pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    let trace_op = Atom::gnd(TraceOp{});
    tref.register_token(regex(r"trace!"), move |_| { trace_op.clone() });
    let print_alternatives_op = Atom::gnd(PrintAlternativesOp{});
    tref.register_token(regex(r"print-alternatives!"), move |_| { print_alternatives_op.clone() });
    let alpha_eq_op = Atom::gnd(AlphaEqOp{});
    tref.register_token(regex(r"=alpha"), move |_| { alpha_eq_op.clone() });
}

pub(super) fn register_context_dependent_tokens(tref: &mut Tokenizer, space: &DynSpace, metta: &Metta) {
    let assert_alpha_equal_to_result_op = Atom::gnd(AssertAlphaEqualToResultOp::new(space.clone(), metta.settings().clone()));
    tref.register_token(regex(r"assertAlphaEqualToResult"), move |_| { assert_alpha_equal_to_result_op.clone() });
    let assert_equal_to_result_op = Atom::gnd(AssertEqualToResultOp::new(space.clone(), metta.settings().clone()));
    tref.register_token(regex(r"assertEqualToResult"), move |_| { assert_equal_to_result_op.clone() });
    let assert_alpha_equal_op = Atom::gnd(AssertAlphaEqualOp::new(space.clone(), metta.settings().clone()));
    tref.register_token(regex(r"assertAlphaEqual"), move |_| { assert_alpha_equal_op.clone() });
    let assert_equal_op = Atom::gnd(AssertEqualOp::new(space.clone(), metta.settings().clone()));
    tref.register_token(regex(r"assertEqual"), move |_| { assert_equal_op.clone() });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::{Metta, EnvBuilder, SExprParser};
    use crate::common::test_utils::metta_space;
    use crate::metta::runner::run_program;

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
        let assert = AssertEqualOp::new(metta.space().clone(), metta.settings().clone());
        let program = "
            (= (foo $x) $x)
            (= (bar $x) $x)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar A))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar B))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed results: B\nExcessive results: A")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive results: A")]
        ]));
    }

    #[test]
    fn metta_assert_alpha_equal_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let assert = AssertAlphaEqualOp::new(metta.space().clone(), metta.settings().clone());
        let program = "
            (= (foo $x) $x)
            (= (bar $x) $x)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqual (foo $x) (bar $x))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqual (foo A) (bar B))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed results: B\nExcessive results: A")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive results: A")]
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
        let assert = AssertEqualToResultOp::new(metta.space().clone(), metta.settings().clone());
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
            vec![expr!("Error" ({assert.clone()} ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed results: A\nExcessive results: C")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (baz) (D))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("baz") ("D")) "\nExpected: [D]\nGot: [D, D, D]\nExcessive results: D, D")]
        ]));
    }

    #[test]
    fn metta_assert_alpha_equal_to_result_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let assert = AssertAlphaEqualToResultOp::new(metta.space().clone(), metta.settings().clone());
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
            vec![expr!("Error" ({assert.clone()} ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed results: A\nExcessive results: C")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertAlphaEqualToResult (baz) (D))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("baz") ("D")) "\nExpected: [D]\nGot: [D, D, D]\nExcessive results: D, D")]
        ]));
    }

    #[test]
    fn assert_equal_op() {
        let space = DynSpace::new(metta_space("
            (= (foo) (A B))
            (= (foo) (B C))
            (= (bar) (B C))
            (= (bar) (A B))
            (= (err) (A B))
        "));

        let assert_equal_op = AssertEqualOp::new(space, PragmaSettings::new());

        assert_eq!(assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("bar"))]), unit_result());

        let actual = assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("err"))]);
        let expected = Regex::new("\nExpected: \\[(A B)\\]\nGot: \\[\\((B C)|, |(A B)\\){3}\\]\nExcessive result: (B C)").unwrap();
        assert_runtime_error(actual, expected);

        let actual = assert_equal_op.execute(&mut vec![expr!(("err")), expr!(("foo"))]);
        let expected = Regex::new("\nExpected: \\[\\((B C)|, |(A B)\\){3}\\]\nGot: \\[(A B)\\]\nMissed result: (B C)").unwrap();
        assert_runtime_error(actual, expected);
    }

    #[test]
    fn assert_equal_to_result_op() {
        let space = DynSpace::new(metta_space("
            (= (foo) (A B))
            (= (foo) (B C))
        "));
        let assert_equal_to_result_op = AssertEqualToResultOp::new(space, PragmaSettings::new());

        assert_eq!(assert_equal_to_result_op.execute(&mut vec![
            expr!(("foo")), expr!(("B" "C") ("A" "B"))]),
                   unit_result());
    }

    #[test]
    fn trace_op() {
        assert_eq!(TraceOp{}.execute(&mut vec![sym!("\"Here?\""), sym!("42")]),
                   Ok(vec![sym!("42")]));
    }
}
