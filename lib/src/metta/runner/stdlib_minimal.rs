use crate::*;
use crate::matcher::MatchResultIter;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::runner::Metta;
use crate::metta::types::get_atom_types;
use crate::common::assert::vec_eq_no_order;
use crate::common::shared::Shared;
use crate::metta::runner::stdlib;

use std::fmt::Display;
use regex::Regex;
use std::convert::TryInto;

use super::arithmetics::*;
use super::string::*;

fn unit_result() -> Result<Vec<Atom>, ExecError> {
    Ok(vec![UNIT_ATOM()])
}

#[derive(Clone, PartialEq, Debug)]
pub struct PrintAlternativesOp {}

impl Display for PrintAlternativesOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "print-alternatives!")
    }
}

impl Grounded for PrintAlternativesOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("print-alternatives! expects format string as a first argument and expression as a second argument");
        let atom = atom_to_string(args.get(0).ok_or_else(arg_error)?);
        let args = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?;
        let args: Vec<String> = args.children().iter()
            .map(|atom| atom_to_string(atom))
            .collect();
        println!("{} {}:", args.len(), atom);
        args.iter().for_each(|arg| println!("    {}", arg));
        Ok(vec![UNIT_ATOM()])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

fn atom_to_string(atom: &Atom) -> String {
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
#[derive(Clone, PartialEq, Debug)]
pub struct GetTypeOp {
    // TODO: MINIMAL this is temporary compatibility fix to be removed after
    // migration to the minimal MeTTa
    space: DynSpace,
}

impl GetTypeOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Display for GetTypeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "get-type")
    }
}

impl Grounded for GetTypeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-type expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let space = match args.get(1) {
            Some(space) => Atom::as_gnd::<DynSpace>(space)
                .ok_or("match expects a space as the first argument"),
            None => Ok(&self.space),
        }?;
        let types = get_atom_types(space, atom);
        if types.is_empty() {
            Ok(vec![EMPTY_SYMBOL])
        } else {
            Ok(types)
        }
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct IfEqualOp { }

impl Display for IfEqualOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if-equal")
    }
}

impl Grounded for IfEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

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

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
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
    let expr = Atom::expr([EVAL_SYMBOL, Atom::expr([INTERPRET_SYMBOL, expr.clone(), ATOM_TYPE_UNDEFINED, Atom::gnd(space.clone())])]);
    crate::metta::interpreter_minimal::interpret(space, &expr)
}

fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>, atom: &Atom) -> Result<Vec<Atom>, ExecError> {
    log::debug!("assert_results_equal: actual: {:?}, expected: {:?}, actual atom: {:?}", actual, expected, atom);
    let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
    match vec_eq_no_order(actual.iter(), expected.iter()) {
        Ok(()) => unit_result(),
        Err(diff) => Err(ExecError::Runtime(format!("{}\n{}", report, diff)))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct AssertEqualOp {
    space: DynSpace,
}

impl AssertEqualOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Display for AssertEqualOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "assertEqual")
    }
}

impl Grounded for AssertEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertEqualOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertEqual expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected_atom = args.get(1).ok_or_else(arg_error)?;

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;
        let expected = interpret_no_error(self.space.clone(), expected_atom)?;

        assert_results_equal(&actual, &expected, actual_atom)
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct AssertEqualToResultOp {
    space: DynSpace,
}

impl AssertEqualToResultOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Display for AssertEqualToResultOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "assertEqualToResult")
    }
}

impl Grounded for AssertEqualToResultOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertEqualToResultOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertEqualToResult expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)
            .map_err(|_| arg_error())?
            .children();

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;

        assert_results_equal(&actual, expected, actual_atom)
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct SuperposeOp {
    space: DynSpace,
}

impl SuperposeOp {
    fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Display for SuperposeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "superpose")
    }
}

impl Grounded for SuperposeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_UNDEFINED])
    }

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

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct CollapseOp {
    space: DynSpace,
}

impl CollapseOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Display for CollapseOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "collapse")
    }
}

impl Grounded for CollapseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("collapse expects single executable atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        // TODO: Calling interpreter inside the operation is not too good
        // Could it be done via returning atom for the further interpretation?
        let result = interpret_no_error(self.space.clone(), atom)?;

        Ok(vec![Atom::expr(result)])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct CaptureOp {
    space: DynSpace,
}

impl CaptureOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Display for CaptureOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "capture")
    }
}

impl Grounded for CaptureOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("capture expects one argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        interpret(self.space.clone(), &atom).map_err(|e| ExecError::from(e))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct CaseOp {
    space: DynSpace,
}

impl CaseOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Display for CaseOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "case")
    }
}

impl Grounded for CaseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

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
        // cannot use `unify` in a unit test because after interpreting `(chain... (chain (eval
        // (interpret (unify ...) Atom <space>))) ...)` `chain` executes `unify` and also gets
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

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

fn regex(regex: &str) -> Regex {
    Regex::new(regex).unwrap()
}

//TODO: The additional arguments are a temporary hack on account of the way the operation atoms store references
// to the runner & module state.  https://github.com/trueagi-io/hyperon-experimental/issues/410
pub fn register_common_tokens(tref: &mut Tokenizer, _tokenizer: Shared<Tokenizer>, space: &DynSpace, metta: &Metta) {

    let get_type_op = Atom::gnd(GetTypeOp::new(space.clone()));
    tref.register_token(regex(r"get-type"), move |_| { get_type_op.clone() });
    let get_type_space_op = Atom::gnd(stdlib::GetTypeSpaceOp{});
    tref.register_token(regex(r"get-type-space"), move |_| { get_type_space_op.clone() });
    let get_meta_type_op = Atom::gnd(stdlib::GetMetaTypeOp{});
    tref.register_token(regex(r"get-metatype"), move |_| { get_meta_type_op.clone() });
    let is_equivalent = Atom::gnd(IfEqualOp{});
    tref.register_token(regex(r"if-equal"), move |_| { is_equivalent.clone() });
    let new_space_op = Atom::gnd(stdlib::NewSpaceOp{});
    tref.register_token(regex(r"new-space"), move |_| { new_space_op.clone() });
    let add_atom_op = Atom::gnd(stdlib::AddAtomOp{});
    tref.register_token(regex(r"add-atom"), move |_| { add_atom_op.clone() });
    let remove_atom_op = Atom::gnd(stdlib::RemoveAtomOp{});
    tref.register_token(regex(r"remove-atom"), move |_| { remove_atom_op.clone() });
    let get_atoms_op = Atom::gnd(stdlib::GetAtomsOp{});
    tref.register_token(regex(r"get-atoms"), move |_| { get_atoms_op.clone() });
    let new_state_op = Atom::gnd(stdlib::NewStateOp{});
    tref.register_token(regex(r"new-state"), move |_| { new_state_op.clone() });
    let change_state_op = Atom::gnd(stdlib::ChangeStateOp{});
    tref.register_token(regex(r"change-state!"), move |_| { change_state_op.clone() });
    let get_state_op = Atom::gnd(stdlib::GetStateOp{});
    tref.register_token(regex(r"get-state"), move |_| { get_state_op.clone() });
    let nop_op = Atom::gnd(stdlib::NopOp{});
    tref.register_token(regex(r"nop"), move |_| { nop_op.clone() });
    let match_op = Atom::gnd(stdlib::MatchOp{});
    tref.register_token(regex(r"match"), move |_| { match_op.clone() });
    let register_module_op = Atom::gnd(stdlib::RegisterModuleOp::new(metta.clone()));
    tref.register_token(regex(r"register-module!"), move |_| { register_module_op.clone() });
    let mod_space_op = Atom::gnd(stdlib::ModSpaceOp::new(metta.clone()));
    tref.register_token(regex(r"mod-space!"), move |_| { mod_space_op.clone() });
    let print_mods_op = Atom::gnd(stdlib::PrintModsOp::new(metta.clone()));
    tref.register_token(regex(r"print-mods!"), move |_| { print_mods_op.clone() });
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
    let pragma_op = Atom::gnd(stdlib::PragmaOp::new(metta.settings().clone()));
    tref.register_token(regex(r"pragma!"), move |_| { pragma_op.clone() });
    let import_op = Atom::gnd(stdlib::ImportOp::new(metta.clone()));
    tref.register_token(regex(r"import!"), move |_| { import_op.clone() });
    let include_op = Atom::gnd(stdlib::IncludeOp::new(metta.clone()));
    tref.register_token(regex(r"include"), move |_| { include_op.clone() });
    let bind_op = Atom::gnd(stdlib::BindOp::new(tokenizer.clone()));
    tref.register_token(regex(r"bind!"), move |_| { bind_op.clone() });
    let trace_op = Atom::gnd(stdlib::TraceOp{});
    tref.register_token(regex(r"trace!"), move |_| { trace_op.clone() });
    let println_op = Atom::gnd(stdlib::PrintlnOp{});
    tref.register_token(regex(r"println!"), move |_| { println_op.clone() });
    let format_args_op = Atom::gnd(stdlib::FormatArgsOp{});
    tref.register_token(regex(r"format-args"), move |_| { format_args_op.clone() });
    let print_alternatives_op = Atom::gnd(PrintAlternativesOp{});
    tref.register_token(regex(r"print-alternatives!"), move |_| { print_alternatives_op.clone() });
    let sealed_op = Atom::gnd(stdlib::SealedOp{});
    tref.register_token(regex(r"sealed"), move |_| { sealed_op.clone() });
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
    tref.register_token(regex(r#""[^"]+""#),
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
    let eq_op = Atom::gnd(stdlib::EqualOp{});
    tref.register_token(regex(r"=="), move |_| { eq_op.clone() });

    target.move_front(&mut rust_tokens);
}

pub static METTA_CODE: &'static str = include_str!("stdlib_minimal.metta");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::text::SExprParser;
    use crate::metta::runner::EnvBuilder;
    use crate::matcher::atoms_are_equivalent;
    use crate::common::Operation;
    use crate::common::test_utils::metta_space;

    use std::convert::TryFrom;

    fn run_program(program: &str) -> Result<Vec<Vec<Atom>>, String> {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.run(SExprParser::new(program))
    }

    #[test]
    fn get_type_op() {
        let space = DynSpace::new(metta_space("
            (: B Type)
            (: C Type)
            (: A B)
            (: A C)
        "));

        let get_type_op = GetTypeOp::new(space.clone());
        assert_eq_no_order!(get_type_op.execute(&mut vec![sym!("A"), expr!({space.clone()})]).unwrap(),
            vec![sym!("B"), sym!("C")]);
    }

    #[test]
    fn get_type_op_non_valid_atom() {
        let space = DynSpace::new(metta_space("
            (: f (-> Number String))
            (: 42 Number)
            (: \"test\" String)
        "));

        let get_type_op = GetTypeOp::new(space.clone());
        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "42"), expr!({space.clone()})]).unwrap(),
            vec![sym!("String")]);
        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "\"test\""), expr!({space.clone()})]).unwrap(),
            vec![EMPTY_SYMBOL]);
    }


    #[test]
    fn metta_car_atom() {
        let result = run_program("!(eval (car-atom (A $b)))");
        assert_eq!(result, Ok(vec![vec![expr!("A")]]));
        let result = run_program("!(eval (car-atom ($a B)))");
        //assert_eq!(result, Ok(vec![vec![expr!(a)]]));
        assert!(result.is_ok_and(|res| res.len() == 1 && res[0].len() == 1 &&
            atoms_are_equivalent(&res[0][0], &expr!(a))));
        let result = run_program("!(eval (car-atom ()))");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("car-atom" ()) {Str::from_str("car-atom expects a non-empty expression as an argument")})]]));
        let result = run_program("!(eval (car-atom A))");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("car-atom" "A") {Str::from_str("car-atom expects a non-empty expression as an argument")})]]));
    }

    #[test]
    fn metta_cdr_atom() {
        assert_eq!(run_program(&format!("!(cdr-atom (a b c))")), Ok(vec![vec![expr!("b" "c")]]));
        assert_eq!(run_program(&format!("!(cdr-atom ($a $b $c))")), Ok(vec![vec![expr!(b c)]]));
        assert_eq!(run_program(&format!("!(cdr-atom ())")), Ok(vec![vec![expr!("Error" ("cdr-atom" ()) {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
        assert_eq!(run_program(&format!("!(cdr-atom a)")), Ok(vec![vec![expr!("Error" ("cdr-atom" "a") {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
        assert_eq!(run_program(&format!("!(cdr-atom $a)")), Ok(vec![vec![expr!("Error" ("cdr-atom" a) {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
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
    fn metta_filter_atom() {
        assert_eq!(run_program("!(eval (filter-atom () $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!()]]));
        assert_eq!(run_program("!(eval (filter-atom (a (b) $c) $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!("a" ("b") c)]]));
        assert_eq!(run_program("!(eval (filter-atom (a (Error (b) \"Test error\") $c) $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!("a" c)]]));
    }

    #[test]
    fn metta_map_atom() {
        assert_eq!(run_program("!(eval (map-atom () $x ($x mapped)))"), Ok(vec![vec![expr!()]]));
        assert_eq!(run_program("!(eval (map-atom (a (b) $c) $x (mapped $x)))"), Ok(vec![vec![expr!(("mapped" "a") ("mapped" ("b")) ("mapped" c))]]));
    }

    #[test]
    fn metta_foldl_atom() {
        assert_eq!(run_program("!(eval (foldl-atom () 1 $a $b (eval (+ $a $b))))"), Ok(vec![vec![expr!({Number::Integer(1)})]]));
        assert_eq!(run_program("!(eval (foldl-atom (1 2 3) 0 $a $b (eval (+ $a $b))))"), Ok(vec![vec![expr!({Number::Integer(6)})]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_atom() {
        let result = run_program("!(eval (interpret A Atom &self))");
        assert_eq!(result, Ok(vec![vec![expr!("A")]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_meta_type() {
        assert_eq!(run_program("!(eval (interpret A Symbol &self))"), Ok(vec![vec![expr!("A")]]));
        assert_eq!(run_program("!(eval (interpret $x Variable &self))"), Ok(vec![vec![expr!(x)]]));
        assert_eq!(run_program("!(eval (interpret (A B) Expression &self))"), Ok(vec![vec![expr!("A" "B")]]));
        assert_eq!(run_program("!(eval (interpret 42 Grounded &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
    }

    #[test]
    fn metta_interpret_symbol_or_grounded_value_as_type() {
        assert_eq!(run_program("(: a A) !(eval (interpret a A &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(eval (interpret a B &self))"), Ok(vec![vec![expr!("Error" "a" "BadType")]]));
        assert_eq!(run_program("!(eval (interpret 42 Number &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
    }

    #[test]
    fn metta_interpret_variable_as_type() {
        assert_eq!(run_program("!(eval (interpret $x %Undefined% &self))"), Ok(vec![vec![expr!(x)]]));
        assert_eq!(run_program("!(eval (interpret $x SomeType &self))"), Ok(vec![vec![expr!(x)]]));
    }

    #[test]
    fn metta_interpret_empty_expression_as_type() {
        assert_eq!(run_program("!(eval (interpret () %Undefined% &self))"), Ok(vec![vec![expr!(())]]));
        assert_eq!(run_program("!(eval (interpret () SomeType &self))"), Ok(vec![vec![expr!(())]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_variable_type() {
        let result = run_program("
            (: S Int)
            !(chain (eval (interpret S $t &self)) $res (: $res $t))
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
            !(eval (interpret (foo (bar a)) %Undefined% &self))
        ");
        assert_eq!(result, Ok(vec![vec![expr!("a")]]));
        let result = run_program("
            (: b B)
            (: foo (-> T T))
            (= (foo $x) $x)
            !(eval (interpret (foo b) %Undefined% &self))
        ");
        assert_eq!(result, Ok(vec![vec![expr!("Error" "b" "BadType")]]));
        let result = run_program("
            (: Nil (List $t))
            (: Z Nat)
            (: S (-> Nat Nat))
            (: Cons (-> $t (List $t) (List $t)))
            !(eval (interpret (Cons S (Cons Z Nil)) %Undefined% &self))
        ");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("Cons" "Z" "Nil") "BadType")]]));
    }

    #[test]
    fn metta_interpret_tuple() {
        assert_eq!(run_program("!(eval (interpret-tuple () &self))"), Ok(vec![vec![expr!(())]]));
        assert_eq!(run_program("!(eval (interpret-tuple (a) &self))"), Ok(vec![vec![expr!(("a"))]]));
        assert_eq!(run_program("!(eval (interpret-tuple (a b) &self))"), Ok(vec![vec![expr!(("a" "b"))]]));
        assert_eq!(run_program("
            (= (foo $x) (bar $x))
            (= (bar $x) (baz $x))
            (= (baz $x) $x)
            !(eval (interpret-tuple ((foo A) (foo B)) &self))
        "), Ok(vec![vec![expr!("A" "B")]]));
    }

    #[test]
    fn metta_interpret_expression_as_type() {
        assert_eq!(run_program("(= (foo $x) $x) !(eval (interpret (foo a) %Undefined% &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (interpret (foo a) %Undefined% &self))"), Ok(vec![vec![expr!("foo" "a")]]));
        assert_eq!(run_program("!(eval (interpret () SomeType &self))"), Ok(vec![vec![expr!(())]]));
    }

    #[test]
    fn metta_interpret_single_atom_with_two_types() {
        let result = run_program("(: a A) (: a B) !(eval (interpret a %Undefined% &self))");
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
            vec![UNIT_ATOM()],
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
            vec![UNIT_ATOM()],
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

            !(eval (interpret (if (and (is $x croaks) (is $x eats-flies)) (= (is $x frog) True) Empty) %Undefined% &self))
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

            !(eval (interpret (color) %Undefined% &self))
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

            !(eval (interpret (eq (plus Z $n) $n) %Undefined% &self))
            !(eval (interpret (eq (plus (S Z) $n) $n) %Undefined% &self))
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

            !(eval (interpret (myif (a $x) $x) %Undefined% &self))
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!("d")]]));
    }

    #[test]
    fn test_variable_name_conflict() {
        let program = "
            (= (a ($W)) True)

            !(eval (interpret (a $W) %Undefined% &self))
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!({Bool(true)})]]));
    }

    #[test]
    fn test_variable_name_conflict_renaming() {
        let program = "
            (= (b ($x $y)) (c $x $y))

            !(eval (interpret (a (b $a) $x $y) %Undefined% &self))
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

            !(eval (interpret ((foo) a) %Undefined% &self))
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

            !(eval (interpret (id_a myAtom) %Undefined% &self))
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
            |_| Atom::gnd(ID_NUM));

        assert_eq!(metta.run(SExprParser::new(program1)),
            Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));

        let program2 = "
            !(eval (interpret (id_num myAtom) %Undefined% &self))
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

            !(eval (interpret (foo a b) %Undefined% &self))
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
            |_| Atom::gnd(ID_NUM));

        assert_eq!(metta.run(SExprParser::new(program1)),
            Ok(vec![vec![expr!("c")]]));

        let program2 = "!(eval (interpret (foo a) %Undefined% &self))";

        assert_eq!(metta.run(SExprParser::new(program2)),
            Ok(vec![vec![expr!("Error" ("foo" "a") "IncorrectNumberOfArguments")]]));

        let program3 = "!(eval (interpret (foo a b c) %Undefined% &self))";

        assert_eq!(metta.run(SExprParser::new(program3)),
            Ok(vec![vec![expr!("Error" ("foo" "a" "b" "c") "IncorrectNumberOfArguments")]]));
    }

    #[test]
    fn use_sealed_to_make_scoped_variable() {
        assert_eq!(run_program("!(let $x (input $x) (output $x))"), Ok(vec![vec![]]));
        assert_eq!(run_program("!(let ($sv $st) (sealed ($x) ($x (output $x)))
               (let $sv (input $x) $st))"), Ok(vec![vec![expr!("output" ("input" x))]]));
    }

    #[test]
    fn test_pragma_interpreter_bare_minimal() {
        let program = "
            (= (bar) baz)
            (= (foo) (bar))
            !(eval (foo))
            !(pragma! interpreter bare-minimal)
            !(eval (foo))
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![
                vec![expr!("baz")],
                vec![UNIT_ATOM()],
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

        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            execute_not_executable(self)
        }

        fn match_(&self, other: &Atom) -> MatchResultIter {
            match_by_equality(self, other)
        }
    }

    #[ignore = "Test is slow"]
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

    #[ignore = "Test is slow"]
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

    #[ignore = "Test is slow"]
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

    #[ignore = "Test is slow"]
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

    #[ignore = "Test is slow"]
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

    #[ignore = "Test is slow"]
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
}
