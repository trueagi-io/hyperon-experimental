#[macro_use]
pub mod math;
pub mod random;
pub mod atom;
pub mod module;
pub mod package;
pub mod string_mod;
pub mod debug;
pub mod space;

use crate::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::{Tokenizer, SExprParser};
use crate::common::assert::vec_eq_no_order;
use crate::common::shared::Shared;
use crate::common::CachingMapper;
#[cfg(feature = "pkg_mgmt")]
use crate::metta::runner::{Metta, RunContext, ModuleLoader};

use std::convert::TryInto;
use std::collections::HashMap;
use regex::Regex;

use super::{arithmetics::*, string::*};

pub(crate) fn unit_result() -> Result<Vec<Atom>, ExecError> {
    Ok(vec![UNIT_ATOM])
}

pub(crate) fn regex(regex: &str) -> Regex {
    Regex::new(regex).unwrap()
}

macro_rules! grounded_op {
    ($name:ident, $disp:literal) => {
        impl PartialEq for $name {
            fn eq(&self, _other: &Self) -> bool {
                true
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, $disp)
            }
        }
    }
}

pub(crate) use grounded_op;

#[derive(Clone, Debug)]
pub struct NewSpaceOp {}

grounded_op!(NewSpaceOp, "new-space");

impl Grounded for NewSpaceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>()])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NewSpaceOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        if args.len() == 0 {
            let space = Atom::gnd(DynSpace::new(GroundingSpace::new()));
            Ok(vec![space])
        } else {
            Err("new-space doesn't expect arguments".into())
        }
    }
}

#[derive(Clone, Debug)]
pub struct PragmaOp {
    settings: Shared<HashMap<String, Atom>>,
}

grounded_op!(PragmaOp, "pragma!");

impl PragmaOp {
    pub fn new(settings: Shared<HashMap<String, Atom>>) -> Self {
        Self{ settings }
    }
}

impl Grounded for PragmaOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for PragmaOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("pragma! expects key and value as arguments");
        let key = <&SymbolAtom>::try_from(args.get(0).ok_or_else(arg_error)?).map_err(|_| "pragma! expects symbol atom as a key")?.name();
        let value = args.get(1).ok_or_else(arg_error)?;
        self.settings.borrow_mut().insert(key.into(), value.clone());
        unit_result()
    }
}







#[derive(Clone, Debug)]
pub struct NopOp {}

grounded_op!(NopOp, "nop");

impl Grounded for NopOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NopOp {
    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        unit_result()
    }
}



#[derive(Clone, Debug)]
pub struct SealedOp {}

grounded_op!(SealedOp, "sealed");

impl Grounded for SealedOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SealedOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("sealed expects two arguments: var_list and expression");

        let mut term_to_seal = args.get(1).ok_or_else(arg_error)?.clone();
        let var_list = args.get(0).ok_or_else(arg_error)?.clone();

        let mut local_var_mapper = CachingMapper::new(|var: &VariableAtom| var.clone().make_unique());

        var_list.iter().filter_type::<&VariableAtom>()
            .for_each(|var| { let _ = local_var_mapper.replace(var); });

        term_to_seal.iter_mut().filter_type::<&mut VariableAtom>()
            .for_each(|var| match local_var_mapper.mapping().get(var) {
                Some(v) => *var = v.clone(),
                None => {},
            });

        let result = vec![term_to_seal.clone()];
        log::debug!("sealed::execute: var_list: {}, term_to_seal: {}, result: {:?}", var_list, term_to_seal, result);

        Ok(result)
    }
}

#[derive(Clone, Debug)]
pub struct EqualOp {}

grounded_op!(EqualOp, "==");

impl Grounded for EqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!(t), expr!(t), ATOM_TYPE_BOOL])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for EqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from(concat!(stringify!($op), " expects two arguments"));
        let a = args.get(0).ok_or_else(arg_error)?;
        let b = args.get(1).ok_or_else(arg_error)?;

        Ok(vec![Atom::gnd(Bool(a == b))])
    }
}

#[derive(Clone, Debug)]
pub struct MatchOp {}

grounded_op!(MatchOp, "match");

impl Grounded for MatchOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(), ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for MatchOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("match expects three arguments: space, pattern and template");
        let space = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let template = args.get(2).ok_or_else(arg_error)?;
        log::debug!("MatchOp::execute: space: {:?}, pattern: {:?}, template: {:?}", space, pattern, template);
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("match expects a space as the first argument")?;
        Ok(space.borrow().subst(&pattern, &template))
    }
}

pub fn atom_to_string(atom: &Atom) -> String {
    match atom {
        Atom::Grounded(gnd) if gnd.type_() == ATOM_TYPE_STRING => {
            let mut s = gnd.to_string();
            s.remove(0);
            s.pop();
            s
        },
        _ => atom.to_string(),
    }
}

#[derive(Clone, Debug)]
pub struct IfEqualOp { }

grounded_op!(IfEqualOp, "if-equal");

impl Grounded for IfEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IfEqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("if-equal expects <atom> <pattern> <then> <else> as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let then = args.get(2).ok_or_else(arg_error)?;
        let else_ = args.get(3).ok_or_else(arg_error)?;

        if crate::matcher::atoms_are_equivalent(atom, pattern) {
            Ok(vec![then.clone()])
        } else {
            Ok(vec![else_.clone()])
        }
    }
}

// TODO: remove hiding errors completely after making it possible passing
// them to the user
fn interpret_no_error(space: DynSpace, expr: &Atom) -> Result<Vec<Atom>, String> {
    let result = interpret(space, &expr);
    log::debug!("interpret_no_error: interpretation expr: {}, result {:?}", expr, result);
    match result {
        Ok(result) => Ok(result),
        Err(_) => Ok(vec![]),
    }
}

fn interpret(space: DynSpace, expr: &Atom) -> Result<Vec<Atom>, String> {
    let expr = Atom::expr([METTA_SYMBOL, expr.clone(), ATOM_TYPE_UNDEFINED, Atom::gnd(space.clone())]);
    let result = crate::metta::interpreter::interpret(space, &expr);
    result
}

fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>, atom: &Atom) -> Result<Vec<Atom>, ExecError> {
    log::debug!("assert_results_equal: actual: {:?}, expected: {:?}, actual atom: {:?}", actual, expected, atom);
    let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
    match vec_eq_no_order(actual.iter(), expected.iter()) {
        Ok(()) => unit_result(),
        Err(diff) => Err(ExecError::Runtime(format!("{}\n{}", report, diff)))
    }
}

#[derive(Clone, Debug)]
pub struct AssertEqualOp {
    space: DynSpace,
}

grounded_op!(AssertEqualOp, "assertEqual");

impl AssertEqualOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for AssertEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertEqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertEqualOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertEqual expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected_atom = args.get(1).ok_or_else(arg_error)?;

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;
        let expected = interpret_no_error(self.space.clone(), expected_atom)?;

        assert_results_equal(&actual, &expected, actual_atom)
    }
}

#[derive(Clone, Debug)]
pub struct AssertEqualToResultOp {
    space: DynSpace,
}

grounded_op!(AssertEqualToResultOp, "assertEqualToResult");

impl AssertEqualToResultOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for AssertEqualToResultOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertEqualToResultOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertEqualToResultOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertEqualToResult expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)
            .map_err(|_| arg_error())?
            .children();

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;

        assert_results_equal(&actual, &expected.into(), actual_atom)
    }
}

#[derive(Clone, Debug)]
pub struct SuperposeOp {
    space: DynSpace,
}

grounded_op!(SuperposeOp, "superpose");

impl SuperposeOp {
    fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for SuperposeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SuperposeOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("superpose expects single expression as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let expr  = TryInto::<&ExpressionAtom>::try_into(atom).map_err(|_| arg_error())?;

        if expr.children().is_empty() {
            Ok(vec![EMPTY_SYMBOL])
        } else {
            let mut superposed = Vec::new();
            for atom in expr.children() {
                match interpret_no_error(self.space.clone(), atom) {
                    Ok(results) => { superposed.extend(results); },
                    Err(message) => { return Err(format!("Error: {}", message).into()) },
                }
            }
            Ok(superposed)
        }
    }
}

#[derive(Clone, Debug)]
pub struct CollapseOp {
    space: DynSpace,
}

grounded_op!(CollapseOp, "collapse");

impl CollapseOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for CollapseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CollapseOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("collapse expects single executable atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        // TODO: Calling interpreter inside the operation is not too good
        // Could it be done via returning atom for the further interpretation?
        let result = interpret_no_error(self.space.clone(), atom)?;

        Ok(vec![Atom::expr(result)])
    }
}

#[derive(Clone, Debug)]
pub struct CaptureOp {
    space: DynSpace,
}

grounded_op!(CaptureOp, "capture");

impl CaptureOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for CaptureOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CaptureOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("capture expects one argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        interpret(self.space.clone(), &atom).map_err(|e| ExecError::from(e))
    }
}

#[derive(Clone, Debug)]
pub struct CaseOp {
    space: DynSpace,
}

grounded_op!(CaseOp, "case");

impl CaseOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for CaseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CaseOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("case expects two arguments: atom and expression of cases");
        let cases = args.get(1).ok_or_else(arg_error)?;
        let atom = args.get(0).ok_or_else(arg_error)?;
        log::debug!("CaseOp::execute: atom: {}, cases: {:?}", atom, cases);

        let switch = |interpreted: Atom| -> Atom {
            Atom::expr([sym!("switch"), interpreted, cases.clone()])
        };

        // Interpreting argument inside CaseOp is required because otherwise `Empty` result
        // calculated inside interpreter cuts the whole branch of the interpretation. Also we
        // cannot use `unify` in a unit test because after interpreting `(chain... (chain
        // (metta (unify ...) Atom <space>)) ...)` `chain` executes `unify` and also gets
        // `Empty` even if we have `Atom` as a resulting type. It can be solved by different ways.
        // One way is to invent new type `EmptyType` (type of the `Empty` atom) and use this type
        // in a function to allow `Empty` atoms as an input. `EmptyType` type should not be
        // casted to the `%Undefined%` thus one cannot pass `Empty` to the function which accepts
        // `%Undefined%`. Another way is to introduce "call" level. Thus if function called
        // returned the result to the `chain` it should stop reducing it and insert it into the
        // last argument.
        let results = interpret(self.space.clone(), atom);
        log::debug!("CaseOp::execute: atom results: {:?}", results);
        let results = match results {
            Ok(results) if results.is_empty() =>
                vec![switch(EMPTY_SYMBOL)],
            Ok(results) =>
                results.into_iter().map(|atom| switch(atom)).collect(),
            Err(err) => vec![Atom::expr([ERROR_SYMBOL, atom.clone(), Atom::sym(err)])],
        };
        Ok(results)
    }
}

//TODO: The additional arguments are a temporary hack on account of the way the operation atoms store references
// to the runner & module state.  https://github.com/trueagi-io/hyperon-experimental/issues/410
pub fn register_common_tokens(tref: &mut Tokenizer, _tokenizer: Shared<Tokenizer>, space: &DynSpace, metta: &Metta) {
    let is_equivalent = Atom::gnd(IfEqualOp{});
    tref.register_token(regex(r"if-equal"), move |_| { is_equivalent.clone() });
    let new_space_op = Atom::gnd(NewSpaceOp{});
    tref.register_token(regex(r"new-space"), move |_| { new_space_op.clone() });
    let nop_op = Atom::gnd(NopOp{});
    tref.register_token(regex(r"nop"), move |_| { nop_op.clone() });
    let match_op = Atom::gnd(MatchOp{});
    tref.register_token(regex(r"match"), move |_| { match_op.clone() });

    math::register_common_tokens(tref);
    random::register_common_tokens(tref);
    atom::register_common_tokens(tref, space);
    module::register_common_tokens(tref, metta);
    space::register_common_tokens(tref);

    #[cfg(feature = "pkg_mgmt")]
    package::register_pkg_mgmt_tokens(tref, metta);
}

//TODO: The additional arguments are a temporary hack on account of the way the operation atoms store references
// to the runner & module state.  https://github.com/trueagi-io/hyperon-experimental/issues/410
pub fn register_runner_tokens(tref: &mut Tokenizer, tokenizer: Shared<Tokenizer>, space: &DynSpace, metta: &Metta) {
    let assert_equal_op = Atom::gnd(AssertEqualOp::new(space.clone()));
    tref.register_token(regex(r"assertEqual"), move |_| { assert_equal_op.clone() });
    let assert_equal_to_result_op = Atom::gnd(AssertEqualToResultOp::new(space.clone()));
    tref.register_token(regex(r"assertEqualToResult"), move |_| { assert_equal_to_result_op.clone() });
    let superpose_op = Atom::gnd(SuperposeOp::new(space.clone()));
    tref.register_token(regex(r"superpose"), move |_| { superpose_op.clone() });
    let collapse_op = Atom::gnd(CollapseOp::new(space.clone()));
    tref.register_token(regex(r"collapse"), move |_| { collapse_op.clone() });
    let case_op = Atom::gnd(CaseOp::new(space.clone()));
    tref.register_token(regex(r"case"), move |_| { case_op.clone() });
    let capture_op = Atom::gnd(CaptureOp::new(space.clone()));
    tref.register_token(regex(r"capture"), move |_| { capture_op.clone() });
    let pragma_op = Atom::gnd(PragmaOp::new(metta.settings().clone()));
    tref.register_token(regex(r"pragma!"), move |_| { pragma_op.clone() });
    let sealed_op = Atom::gnd(SealedOp{});
    tref.register_token(regex(r"sealed"), move |_| { sealed_op.clone() });

    module::register_runner_tokens(tref, tokenizer.clone(), metta);
    string_mod::register_runner_tokens(tref);
    debug::register_runner_tokens(tref);
    // &self should be updated
    // TODO: adding &self might be done not by stdlib, but by MeTTa itself.
    // TODO: adding &self introduces self referencing and thus prevents space
    // from being freed. There are two options to eliminate this. (1) use weak
    // pointer and somehow use the same type to represent weak and strong
    // pointers to the atomspace. (2) resolve &self in GroundingSpace::query
    // method without adding it into container.
    let self_atom = Atom::gnd(space.clone());
    tref.register_token(regex(r"&self"), move |_| { self_atom.clone() });
}

pub fn register_rust_stdlib_tokens(target: &mut Tokenizer) {
    let mut rust_tokens = Tokenizer::new();
    let tref = &mut rust_tokens;

    tref.register_fallible_token(regex(r"[\-\+]?\d+"),
        |token| { Ok(Atom::gnd(Number::from_int_str(token)?)) });
    tref.register_fallible_token(regex(r"[\-\+]?\d+\.\d+"),
        |token| { Ok(Atom::gnd(Number::from_float_str(token)?)) });
    tref.register_fallible_token(regex(r"[\-\+]?\d+(\.\d+)?[eE][\-\+]?\d+"),
        |token| { Ok(Atom::gnd(Number::from_float_str(token)?)) });
    tref.register_token(regex(r"True|False"),
        |token| { Atom::gnd(Bool::from_str(token)) });
    tref.register_token(regex(r#"(?s)^".*"$"#),
        |token| { let mut s = String::from(token); s.remove(0); s.pop(); Atom::gnd(Str::from_string(s)) });
    let sum_op = Atom::gnd(SumOp{});
    tref.register_token(regex(r"\+"), move |_| { sum_op.clone() });
    let sub_op = Atom::gnd(SubOp{});
    tref.register_token(regex(r"\-"), move |_| { sub_op.clone() });
    let mul_op = Atom::gnd(MulOp{});
    tref.register_token(regex(r"\*"), move |_| { mul_op.clone() });
    let div_op = Atom::gnd(DivOp{});
    tref.register_token(regex(r"/"), move |_| { div_op.clone() });
    let mod_op = Atom::gnd(ModOp{});
    tref.register_token(regex(r"%"), move |_| { mod_op.clone() });
    let lt_op = Atom::gnd(LessOp{});
    tref.register_token(regex(r"<"), move |_| { lt_op.clone() });
    let gt_op = Atom::gnd(GreaterOp{});
    tref.register_token(regex(r">"), move |_| { gt_op.clone() });
    let le_op = Atom::gnd(LessEqOp{});
    tref.register_token(regex(r"<="), move |_| { le_op.clone() });
    let ge_op = Atom::gnd(GreaterEqOp{});
    tref.register_token(regex(r">="), move |_| { ge_op.clone() });
    let eq_op = Atom::gnd(EqualOp{});
    tref.register_token(regex(r"=="), move |_| { eq_op.clone() });
    let and_op = Atom::gnd(AndOp{});
    tref.register_token(regex(r"and"), move |_| { and_op.clone() });
    let or_op = Atom::gnd(OrOp{});
    tref.register_token(regex(r"or"), move |_| { or_op.clone() });
    let not_op = Atom::gnd(NotOp{});
    tref.register_token(regex(r"not"), move |_| { not_op.clone() });
    // NOTE: xor is absent in Python intentionally for conversion testing
    let xor_op = Atom::gnd(XorOp{});
    tref.register_token(regex(r"xor"), move |_| { xor_op.clone() });

    random::register_rust_stdlib_tokens(tref);

    target.move_front(&mut rust_tokens);
}

pub static METTA_CODE: &'static str = include_str!("stdlib.metta");

/// Loader to Initialize the corelib module
///
/// NOTE: the corelib will be loaded automatically if the runner is initialized with one of the high-level
/// init functions such as [Metta::new] and [Metta::new_with_stdlib_loader]
#[derive(Debug)]
pub(crate) struct CoreLibLoader;

impl Default for CoreLibLoader {
    fn default() -> Self {
        CoreLibLoader
    }
}

impl ModuleLoader for CoreLibLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        register_rust_stdlib_tokens(&mut *context.module().tokenizer().borrow_mut());

        let parser = SExprParser::new(METTA_CODE);
        context.push_parser(Box::new(parser));

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::text::SExprParser;
    use crate::metta::runner::EnvBuilder;
    use crate::metta::runner::string::Str;
    use crate::matcher::atoms_are_equivalent;
    use crate::common::Operation;
    use crate::common::test_utils::metta_space;

    use std::convert::TryFrom;
    use std::fmt::Display;
    use regex::Regex;

    pub fn run_program(program: &str) -> Result<Vec<Vec<Atom>>, String> {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.run(SExprParser::new(program))
    }

    #[test]
    fn metta_switch() {
        let result = run_program("!(eval (switch (A $b) ( (($a B) ($b $a)) ((B C) (C B)) )))");
        assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
        let result = run_program("!(eval (switch (A $b) ( ((B C) (C B)) (($a B) ($b $a)) )))");
        assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
        let result = run_program("!(eval (switch (A $b) ( ((B C) (C B)) ((D E) (E B)) )))");
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn metta_case_empty() {
        let result = run_program("!(case Empty ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
        let result = run_program("!(case (unify (C B) (C B) ok Empty) ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("ok")]]));
        let result = run_program("!(case (unify (B C) (C B) ok nok) ( (ok ok) (nok nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
        let result = run_program("!(case (unify (B C) (C B) ok Empty) ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
    }

    #[test]
    fn metta_is_function() {
        let result = run_program("!(eval (is-function (-> $t)))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(true)})]]));
        let result = run_program("!(eval (is-function (A $t)))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
        let result = run_program("!(eval (is-function %Undefined%))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
    }

    #[test]
    fn metta_type_cast() {
        assert_eq!(run_program("(: a A) !(eval (type-cast a A &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a B &self))"), Ok(vec![vec![expr!("Error" "a" "BadType")]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a %Undefined% &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (type-cast a B &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (type-cast 42 Number &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("!(eval (type-cast 42 %Undefined% &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a Atom &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a Symbol &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (type-cast 42 Grounded &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("!(eval (type-cast () Expression &self))"), Ok(vec![vec![expr!()]]));
        assert_eq!(run_program("!(eval (type-cast (a b) Expression &self))"), Ok(vec![vec![expr!("a" "b")]]));
        assert_eq!(run_program("!(eval (type-cast $v Variable &self))"), Ok(vec![vec![expr!(v)]]));
        assert_eq!(run_program("(: a A) (: b B) !(eval (type-cast (a b) (A B) &self))"), Ok(vec![vec![expr!("a" "b")]]));
        assert_eq!(run_program("(: a A) (: a B) !(eval (type-cast a A &self))"), Ok(vec![vec![expr!("a")]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_atom() {
        let result = run_program("!(metta A Atom &self)");
        assert_eq!(result, Ok(vec![vec![expr!("A")]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_meta_type() {
        assert_eq!(run_program("!(metta A Symbol &self)"), Ok(vec![vec![expr!("A")]]));
        assert_eq!(run_program("!(metta $x Variable &self)"), Ok(vec![vec![expr!(x)]]));
        assert_eq!(run_program("!(metta (A B) Expression &self)"), Ok(vec![vec![expr!("A" "B")]]));
        assert_eq!(run_program("!(metta 42 Grounded &self)"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
    }

    #[test]
    fn metta_interpret_symbol_or_grounded_value_as_type() {
        assert_eq!(run_program("(: a A) !(metta a A &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(metta a B &self)"), Ok(vec![vec![expr!("Error" "a" "BadType")]]));
        assert_eq!(run_program("!(metta 42 Number &self)"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
    }

    #[test]
    fn metta_interpret_variable_as_type() {
        assert_eq!(run_program("!(metta $x %Undefined% &self)"), Ok(vec![vec![expr!(x)]]));
        assert_eq!(run_program("!(metta $x SomeType &self)"), Ok(vec![vec![expr!(x)]]));
    }

    #[test]
    fn metta_interpret_empty_expression_as_type() {
        assert_eq!(run_program("!(metta () %Undefined% &self)"), Ok(vec![vec![expr!(())]]));
        assert_eq!(run_program("!(metta () SomeType &self)"), Ok(vec![vec![expr!(())]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_variable_type() {
        let result = run_program("
            (: S Int)
            !(chain (metta S $t &self) $res (: $res $t))
        ");
        assert_eq!(result, Ok(vec![vec![expr!(":" "S" "Int")]]));
    }

    #[test]
    fn metta_interpret_func() {
        let result = run_program("
            (: a T)
            (: foo (-> T T))
            (= (foo $x) $x)
            (= (bar $x) $x)
            !(metta (foo (bar a)) %Undefined% &self)
        ");
        assert_eq!(result, Ok(vec![vec![expr!("a")]]));
        let result = run_program("
            (: b B)
            (: foo (-> T T))
            (= (foo $x) $x)
            !(metta (foo b) %Undefined% &self)
        ");
        assert_eq!(result, Ok(vec![vec![expr!("Error" "b" "BadType")]]));
        let result = run_program("
            (: Nil (List $t))
            (: Z Nat)
            (: S (-> Nat Nat))
            (: Cons (-> $t (List $t) (List $t)))
            !(metta (Cons S (Cons Z Nil)) %Undefined% &self)
        ");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("Cons" "Z" "Nil") "BadType")]]));
    }

    #[test]
    fn metta_interpret_tuple() {
        assert_eq!(run_program("!(metta () %Undefined% &self)"), Ok(vec![vec![expr!(())]]));
        assert_eq!(run_program("!(metta (a) %Undefined% &self)"), Ok(vec![vec![expr!(("a"))]]));
        assert_eq!(run_program("!(metta (a b) %Undefined% &self)"), Ok(vec![vec![expr!(("a" "b"))]]));
        assert_eq!(run_program("
            (= (foo $x) (bar $x))
            (= (bar $x) (baz $x))
            (= (baz $x) $x)
            !(metta ((foo A) (foo B)) %Undefined% &self)
        "), Ok(vec![vec![expr!("A" "B")]]));
    }

    #[test]
    fn metta_interpret_expression_as_type() {
        assert_eq!(run_program("(= (foo $x) $x) !(metta (foo a) %Undefined% &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(metta (foo a) %Undefined% &self)"), Ok(vec![vec![expr!("foo" "a")]]));
        assert_eq!(run_program("!(metta () SomeType &self)"), Ok(vec![vec![expr!(())]]));
    }

    #[test]
    fn metta_interpret_single_atom_with_two_types() {
        let result = run_program("(: a A) (: a B) !(metta a %Undefined% &self)");
        assert_eq!(result, Ok(vec![vec![expr!("a")]]));
    }

    #[test]
    fn metta_assert_equal_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let assert = AssertEqualOp::new(metta.space().clone());
        let program = "
            (= (foo $x) $x)
            (= (bar $x) $x)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar A))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar B))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed result: B")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive result: A")]
        ]));
    }

    #[test]
    fn metta_assert_equal_to_result_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let assert = AssertEqualToResultOp::new(metta.space().clone());
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
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (bar) (A))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed result: A")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (baz) (D))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("baz") ("D")) "\nExpected: [D]\nGot: [D, D]\nExcessive result: D")]
        ]));
    }

    #[test]
    fn metta_superpose() {
        assert_eq_metta_results!(run_program("!(superpose (red yellow green))"),
            Ok(vec![vec![expr!("red"), expr!("yellow"), expr!("green")]]));
        let program = "
            (= (foo) FOO)
            (= (bar) BAR)
            !(superpose ((foo) (bar) BAZ))
        ";
        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!("FOO"), expr!("BAR"), expr!("BAZ")]]));
    }

    #[test]
    fn metta_collapse() {
        let program = "
            (= (color) red)
            (= (color) green)
            (= (color) blue)
            !(collapse (color))
        ";
        let result = run_program(program).expect("Successful result is expected");
        assert_eq!(result.len(), 1);
        let result = result.get(0).unwrap();
        assert_eq!(result.len(), 1);
        let result = result.get(0).unwrap();
        let actual = <&ExpressionAtom>::try_from(result)
            .expect("Expression atom is expected").children();
        assert_eq_no_order!(actual, vec![expr!("red"), expr!("green"), expr!("blue")]);
    }

    #[test]
    fn metta_let_novar() {
        let result = run_program("!(let (P A $b) (P $a B) (P $b $a))");
        assert_eq!(result, Ok(vec![vec![expr!("P" "B" "A")]]));
        let result = run_program("
            (= (foo) (P A B))
            !(let (P A $b) (foo) (P $b A))
            ");
        assert_eq!(result, Ok(vec![vec![expr!("P" "B" "A")]]));
        let result = run_program("
            (= (foo) (P A B))
            !(let (foo) (P A $b) (P $b A))
            ");
        assert_eq!(result, Ok(vec![vec![]]));
        let result = run_program("!(let (P A $b) (P B C) (P C B))");
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn metta_let_var() {
        let result = run_program("!(let* () result)");
        assert_eq!(result, Ok(vec![vec![expr!("result")]]));
        let result = run_program("!(let* ( ((P A $b) (P $a B)) ) (P $b $a))");
        assert_eq!(result, Ok(vec![vec![expr!("P" "B" "A")]]));
        let result = run_program("!(let* ( ((P $a) (P A)) ((P B) (P $b)) ) (P $b $a))");
        assert_eq!(result, Ok(vec![vec![expr!("P" "B" "A")]]));
        let result = run_program("!(let* ( ((P $a) (P A)) ((P B) (P C)) ) (P $b $a))");
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn metta_quote_unquote() {
        let header = "
            (= (foo) A)
            (= (bar $x) $x)
        ";
        assert_eq!(run_program(&format!("{header} !(bar (foo))")), Ok(vec![vec![sym!("A")]]), "sanity check");
        assert_eq!(run_program(&format!("{header} !(bar (quote (foo)))")), Ok(vec![vec![expr!("quote" ("foo"))]]), "quote");
        assert_eq!(run_program(&format!("{header} !(bar (unquote (quote (foo))))")), Ok(vec![vec![expr!("A")]]), "unquote before call");
        assert_eq!(run_program(&format!("{header} !(unquote (bar (quote (foo))))")), Ok(vec![vec![expr!("A")]]), "unquote after call");
    }


    #[test]
    fn test_frog_reasoning() {
        let program = "
            (= (is Fritz croaks) True)
            (= (is Fritz eats-flies) True)

            (= (is Tweety chirps) True)
            (= (is Tweety yellow) True)
            (= (is Tweety eats-flies) True)

            !(metta (if (and (is $x croaks) (is $x eats-flies)) (= (is $x frog) True) Empty) %Undefined% &self)
        ";

        assert_eq!(run_program(program),
            Ok(vec![vec![expr!("=" ("is" "Fritz" "frog") {Bool(true)})]]));
    }

    #[test]
    fn test_match_all() {
        let program = "
            (= (color) blue)
            (= (color) red)
            (= (color) green)

            !(metta (color) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!("blue"), expr!("red"), expr!("green")]]));
    }

    #[test]
    fn test_variable_keeps_value_in_different_sub_expressions() {
        let program = "
            (= (eq $x $x) True)
            (= (plus Z $y) $y)
            (= (plus (S $k) $y) (S (plus $k $y)))

            !(metta (eq (plus Z $n) $n) %Undefined% &self)
            !(metta (eq (plus (S Z) $n) $n) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!({Bool(true)})], vec![expr!("eq" ("S" n) n)]]));
    }

    #[test]
    fn test_variable_defined_via_variable() {
        let program = "
            (= (myif T $y) $y)
            (= (mynot F) T)
            (= (a $z) (mynot (b $z)))
            (= (b d) F)

            !(metta (myif (a $x) $x) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!("d")]]));
    }

    #[test]
    fn test_variable_name_conflict() {
        let program = "
            (= (a ($W)) True)

            !(metta (a $W) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!({Bool(true)})]]));
    }

    #[test]
    fn test_variable_name_conflict_renaming() {
        let program = "
            (= (b ($x $y)) (c $x $y))

            !(metta (a (b $a) $x $y) %Undefined% &self)
        ";

        let result = run_program(program);
        assert!(result.is_ok_and(|res| res.len() == 1 && res[0].len() == 1 &&
            atoms_are_equivalent(&res[0][0], &expr!("a" ("c" a b) c d))));
    }

    #[test]
    fn test_operation_is_expression() {
        let program = "
            (: foo (-> (-> A A)))
            (: a A)
            (= (foo) bar)
            (= (bar $x) $x)

            !(metta ((foo) a) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program), Ok(vec![vec![expr!("a")]]));
    }

    static ID_NUM: &Operation = &Operation{
        name: "id_num",
        execute: |_, args| {
            let arg_error = || ExecError::from("id_num expects one argument: number");
            let num = args.get(0).ok_or_else(arg_error)?;
            Ok(vec![num.clone()])
        },
        typ: "(-> Number Number)",
    };

    #[test]
    fn test_return_bad_type_error() {
        let program1 = "
            (: myAtom myType)
            (: id_a (-> A A))
            (= (id_a $a) $a)

            !(metta (id_a myAtom) %Undefined% &self)
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
            |_| Atom::gnd(ID_NUM));

        assert_eq!(metta.run(SExprParser::new(program1)),
            Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));

        let program2 = "
            !(metta (id_num myAtom) %Undefined% &self)
        ";

        assert_eq!(metta.run(SExprParser::new(program2)),
            Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));
    }

    #[test]
    fn test_return_incorrect_number_of_args_error() {
        let program1 = "
            (: a A)
            (: b B)
            (: c C)
            (: foo (-> A B C))
            (= (foo $a $b) c)

            !(metta (foo a b) %Undefined% &self)
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
            |_| Atom::gnd(ID_NUM));

        assert_eq!(metta.run(SExprParser::new(program1)),
            Ok(vec![vec![expr!("c")]]));

        let program2 = "!(metta (foo a) %Undefined% &self)";

        assert_eq!(metta.run(SExprParser::new(program2)),
            Ok(vec![vec![expr!("Error" ("foo" "a") "IncorrectNumberOfArguments")]]));

        let program3 = "!(metta (foo a b c) %Undefined% &self)";

        assert_eq!(metta.run(SExprParser::new(program3)),
            Ok(vec![vec![expr!("Error" ("foo" "a" "b" "c") "IncorrectNumberOfArguments")]]));
    }

    #[test]
    fn sealed_op_runner() {
        let nested = run_program("!(sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b)))))");
        let simple_replace = run_program("!(sealed ($x $y) (quote (= ($y $z))))");

        assert!(crate::atom::matcher::atoms_are_equivalent(&nested.unwrap()[0][0], &expr!("quote" ("=" (a b c) (z)))));
        assert!(crate::atom::matcher::atoms_are_equivalent(&simple_replace.unwrap()[0][0], &expr!("quote" ("=" (y z)))));
    }

    #[test]
    fn sealed_op_execute() {
        let val = SealedOp{}.execute(&mut vec![expr!(x y), expr!("="(y z))]);
        assert!(crate::atom::matcher::atoms_are_equivalent(&val.unwrap()[0], &expr!("="(y z))));
    }

    #[test]
    fn use_sealed_to_make_scoped_variable() {
        assert_eq!(run_program("!(let $x (input $x) (output $x))"), Ok(vec![vec![]]));
        assert_eq!(run_program("!(let $x (input $y) (output $x))"), Ok(vec![vec![expr!("output" ("input" y))]]));
        assert_eq!(run_program("!(let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
               (let $sv (input $x) $st))"), Ok(vec![vec![expr!("output" ("input" x))]]));
    }

    #[test]
    fn test_pragma_interpreter_bare_minimal() {
        let program = "
            (= (bar) baz)
            (= (foo) (bar))
            !(foo)
            !(pragma! interpreter bare-minimal)
            !(foo)
            !(eval (foo))
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![
                vec![expr!("baz")],
                vec![UNIT_ATOM],
                vec![expr!(("foo"))],
                vec![expr!(("bar"))],
            ]));
    }

    #[derive(Clone, PartialEq, Debug)]
    pub struct SomeGndAtom { }

    impl Display for SomeGndAtom {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "some-gnd-atom")
        }
    }

    impl Grounded for SomeGndAtom {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, sym!("Arg1Type"), sym!("Arg2Type"), sym!("RetType")])
        }
    }

    #[test]
    fn test_get_doc_func() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (: Arg1Type Type)
            (: Arg2Type Type)
            (: RetType Type)
            (: some-func (-> Arg1Type Arg2Type RetType))
            (@doc some-func
              (@desc "Test function")
              (@params (
                (@param "First argument")
                (@param "Second argument")
              ))
              (@return "Return value")
            )
            
            !(get-doc some-func)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" "some-func")
                ("@kind" "function")
                ("@type" ("->" "Arg1Type" "Arg2Type" "RetType"))
                ("@desc" {Str::from_str("Test function")})
                ("@params" (
                    ("@param" ("@type" "Arg1Type") ("@desc" {Str::from_str("First argument")}))
                    ("@param" ("@type" "Arg2Type") ("@desc" {Str::from_str("Second argument")})) ))
                ("@return" ("@type" "RetType") ("@desc" {Str::from_str("Return value")})) )],
        ]));
    }

    #[test]
    fn test_get_doc_atom() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (: SomeAtom SomeType)
            (@doc SomeAtom (@desc "Test symbol atom having specific type"))

            !(get-doc SomeAtom)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" "SomeAtom")
                ("@kind" "atom")
                ("@type" "SomeType")
                ("@desc" {Str::from_str("Test symbol atom having specific type")}) )],
        ]));
    }

    #[test]
    fn test_get_doc_gnd_func() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut()
            .register_token(regex::Regex::new(r"some-gnd-atom").unwrap(), |_| Atom::gnd(SomeGndAtom{}));
        let parser = SExprParser::new(r#"
            (@doc some-gnd-atom
              (@desc "Test function")
              (@params (
                (@param "First argument")
                (@param "Second argument")
              ))
              (@return "Return value")
            )
            !(get-doc some-gnd-atom)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" {SomeGndAtom{}})
                ("@kind" "function")
                ("@type" ("->" "Arg1Type" "Arg2Type" "RetType"))
                ("@desc" {Str::from_str("Test function")})
                ("@params" (
                    ("@param" ("@type" "Arg1Type") ("@desc" {Str::from_str("First argument")}))
                    ("@param" ("@type" "Arg2Type") ("@desc" {Str::from_str("Second argument")})) ))
                ("@return" ("@type" "RetType") ("@desc" {Str::from_str("Return value")})) )],
        ]));
    }

    #[test]
    fn test_get_doc_no_doc() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            !(get-doc NoSuchAtom)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" "NoSuchAtom")
                ("@kind" "atom")
                ("@type" "%Undefined%")
                ("@desc" {Str::from_str("No documentation")}) )],
        ]));
    }

    #[test]
    fn test_get_doc_function_call() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (: Arg1Type Type)
            (: Arg2Type Type)
            (: RetType Type)
            (: some-func (-> Arg1Type Arg2Type RetType))
            (@doc some-func
              (@desc "Test function")
              (@params (
                (@param "First argument")
                (@param "Second argument")
              ))
              (@return "Return value")
            )

            !(get-doc (some-func arg1 arg2))
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" ("some-func" "arg1" "arg2"))
                ("@kind" "atom")
                ("@type" "RetType")
                ("@desc" {Str::from_str("No documentation")}) )],
        ]));
    }

    #[test]
    fn test_get_doc_no_type() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (@doc some-func-no-type
              (@desc "Test function")
              (@params (
                (@param "First argument")
                (@param "Second argument")
              ))
              (@return "Return value")
            )

            !(get-doc some-func-no-type)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" "some-func-no-type")
                ("@kind" "function")
                ("@type" "%Undefined%")
                ("@desc" {Str::from_str("Test function")})
                ("@params" (
                    ("@param" ("@type" "%Undefined%") ("@desc" {Str::from_str("First argument")}))
                    ("@param" ("@type" "%Undefined%") ("@desc" {Str::from_str("Second argument")})) ))
                ("@return" ("@type" "%Undefined%") ("@desc" {Str::from_str("Return value")})) )],
        ]));
    }

    #[test]
    fn test_string_parsing() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            !(id "test")
            !(id "te st")
            !(id "te\"st")
            !(id "")
            !(id "te\nst")
            !("te\nst"test)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!({Str::from_str("test")})],
            vec![expr!({Str::from_str("te st")})],
            vec![expr!({Str::from_str("te\"st")})],
            vec![expr!({Str::from_str("")})],
            vec![expr!({Str::from_str("te\nst")})],
            vec![expr!({Str::from_str("te\nst")} "test")],
        ]));
    }

    #[test]
    fn match_op() {
        let space = DynSpace::new(metta_space("(A B)"));
        let match_op = MatchOp{};
        assert_eq!(match_op.execute(&mut vec![expr!({space}), expr!("A" "B"), expr!("B" "A")]),
            Ok(vec![expr!("B" "A")]));
    }

    #[test]
    fn match_op_issue_530() {
        let space = DynSpace::new(metta_space("(A $a $a)"));
        let match_op = MatchOp{};
        let result = match_op.execute(&mut vec![expr!({space}), expr!("A" x y), expr!("A" x y)]).unwrap();
        assert_eq!(result.len(), 1);
        assert!(atoms_are_equivalent(&result[0], &expr!("A" x x)),
            "atoms are not equivalent: expected: {}, actual: {}", expr!("A" x x), result[0]);
    }


    #[test]
    fn new_space_op() {
        let res = NewSpaceOp{}.execute(&mut vec![]).expect("No result returned");
        let space = res.get(0).expect("Result is empty");
        let space = space.as_gnd::<DynSpace>().expect("Result is not space");
        let space_atoms: Vec<Atom> = space.borrow().as_space().atom_iter().unwrap().cloned().collect();
        assert_eq_no_order!(space_atoms, Vec::<Atom>::new());
    }



    fn assert_runtime_error(actual: Result<Vec<Atom>, ExecError>, expected: Regex) {
        match actual {
            Err(ExecError::Runtime(msg)) => assert!(expected.is_match(msg.as_str()),
                "Incorrect error message:\nexpected: {:?}\n  actual: {:?}", expected.to_string(), msg),
            _ => assert!(false, "Error is expected as result, {:?} returned", actual),
        }
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

        let assert_equal_op = AssertEqualOp::new(space);

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
        let assert_equal_to_result_op = AssertEqualToResultOp::new(space);

        assert_eq!(assert_equal_to_result_op.execute(&mut vec![
                expr!(("foo")), expr!(("B" "C") ("A" "B"))]),
                unit_result());
    }


    #[test]
    fn nop_op() {
        assert_eq!(NopOp{}.execute(&mut vec![]), unit_result());
    }

    #[test]
    fn let_op_keep_variables_equalities_issue290() {
        assert_eq_metta_results!(run_program("!(let* (($f f) ($f $x)) $x)"), Ok(vec![vec![expr!("f")]]));
        assert_eq_metta_results!(run_program("!(let* (($f $x) ($f f)) $x)"), Ok(vec![vec![expr!("f")]]));
        assert_eq_metta_results!(run_program("!(let (quote ($x $x)) (quote ($z $y)) (let $y A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
        assert_eq_metta_results!(run_program("!(let (quote ($x $x)) (quote ($z $y)) (let $z A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
    }

    #[test]
    fn let_op_variables_visibility_pr262() {
        let program = "
            ;; Knowledge
            (→ P Q)
            (→ Q R)

            ;; Rule
            (= (rule (→ $p $q) (→ $q $r)) (→ $p $r))

            ;; Query (does not work as expected)
            (= (query $kb)
               (let* (($pq (→ $p $q))
                      ($qr (→ $q $r)))
                 (match $kb
                   ;; Premises
                   (, $pq $qr)
                   ;; Conclusion
                   (rule $pq $qr))))

            ;; Call
            !(query &self)
            ;; [(→ P R)]
        ";
        assert_eq_metta_results!(run_program(program), Ok(vec![vec![expr!("→" "P" "R")]]));
    }

    #[test]
    fn test_stdlib_uses_rust_grounded_tokens() {
        assert_eq!(run_program("!(if True ok nok)"), Ok(vec![vec![Atom::sym("ok")]]));
    }

    #[test]
    fn test_let_op_inside_other_operation() {
        assert_eq!(run_program("!(and True (let $x False $x))"), Ok(vec![vec![expr!({Bool(false)})]]));
    }

    #[test]
    fn test_quote() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            (= (foo) a)
            (= (foo) b)
            !(foo)
            !(quote (foo))
        ");

        assert_eq_metta_results!(metta.run(parser),
            Ok(vec![
                vec![expr!("a"), expr!("b")],
                vec![expr!("quote" ("foo"))],
            ]));
    }

    #[test]
    fn test_unify() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            !(unify (a $b 1 (d)) (a $a 1 (d)) ok nok)
            !(unify (a $b c) (a b $c) (ok $b $c) nok)
            !(unify $a (a b c) (ok $a) nok)
            !(unify (a b c) $a (ok $a) nok)
            !(unify (a b c) (a b d) ok nok)
            !(unify ($x a) (b $x) ok nok)
        ");

        assert_eq_metta_results!(metta.run(parser),
            Ok(vec![
                vec![expr!("ok")],
                vec![expr!("ok" "b" "c")],
                vec![expr!("ok" ("a" "b" "c"))],
                vec![expr!("ok" ("a" "b" "c"))],
                vec![expr!("nok")],
                vec![expr!("nok")]
            ]));
    }

    #[test]
    fn test_empty() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            !(empty)
        ");

        assert_eq_metta_results!(metta.run(parser),
            Ok(vec![vec![]]));
    }
}