use crate::*;
use crate::matcher::MatchResultIter;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::runner::Metta;
use crate::metta::types::{get_atom_types, get_meta_type};
use crate::common::assert::vec_eq_no_order;
use crate::metta::runner::interpret;
use crate::common::shared::Shared;

use std::fmt::Display;
use regex::Regex;
use std::convert::TryInto;
use std::collections::HashMap;

use super::arithmetics::*;

#[derive(Clone, PartialEq, Debug)]
pub struct GetTypeOp {}

impl Display for GetTypeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "get-type")
    }
}

impl Grounded for GetTypeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-type expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let space = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("match expects a space as the first argument")?;
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
pub struct GetMetaTypeOp { }

impl Display for GetMetaTypeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "get-metatype")
    }
}

impl Grounded for GetMetaTypeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-metatype expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        Ok(vec![get_meta_type(&atom)])
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
    let expr = Atom::expr([EVAL_SYMBOL, Atom::expr([Atom::sym("interpret"), expr.clone(), ATOM_TYPE_UNDEFINED, Atom::gnd(space.clone())])]);
    let result = interpret(space, &expr);
    log::debug!("interpret_no_error: interpretation expr: {}, result {:?}", expr, result);
    match result {
        Ok(result) => Ok(result),
        Err(_) => Ok(vec![]),
    }
}

fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>, atom: &Atom) -> Result<Vec<Atom>, ExecError> {
    log::debug!("assert_results_equal: actual: {:?}, expected: {:?}, actual atom: {:?}", actual, expected, atom);
    let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
    match vec_eq_no_order(actual.iter(), expected.iter()) {
        Ok(()) => Ok(vec![VOID_SYMBOL]),
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
        // Could it be done via StepResult?
        let result = interpret_no_error(self.space.clone(), atom)?;

        Ok(vec![Atom::expr(result)])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct PragmaOp {
    settings: Shared<HashMap<String, String>>,
}

impl PragmaOp {
    pub fn new(settings: Shared<HashMap<String, String>>) -> Self {
        Self{ settings }
    }
}

impl Display for PragmaOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "pragma!")
    }
}

impl Grounded for PragmaOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("pragma! expects key and value as arguments");
        let key = TryInto::<&SymbolAtom>::try_into(args.get(0).ok_or_else(arg_error)?)
            .map_err(|_| "pragma! expects symbol atom as a key")?.name();
        let value = TryInto::<&SymbolAtom>::try_into(args.get(1).ok_or_else(arg_error)?)
            .map_err(|_| "pragma! expects symbol atom as a value")?.name();

        // TODO: add support for Grounded values when needed
        self.settings.borrow_mut().insert(key.into(), value.into());

        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

fn regex(regex: &str) -> Regex {
    Regex::new(regex).unwrap()
}

pub fn register_common_tokens(metta: &Metta) {
    let tokenizer = metta.tokenizer();
    let mut tref = tokenizer.borrow_mut();

    let get_type_op = Atom::gnd(GetTypeOp{});
    tref.register_token(regex(r"get-type"), move |_| { get_type_op.clone() });
    let get_meta_type_op = Atom::gnd(GetMetaTypeOp{});
    tref.register_token(regex(r"get-metatype"), move |_| { get_meta_type_op.clone() });
    let is_equivalent = Atom::gnd(IfEqualOp{});
    tref.register_token(regex(r"if-equal"), move |_| { is_equivalent.clone() });
}

pub fn register_runner_tokens(metta: &Metta) {
    let space = metta.space();
    let tokenizer = metta.tokenizer();

    let mut tref = tokenizer.borrow_mut();

    let assert_equal_op = Atom::gnd(AssertEqualOp::new(space.clone()));
    tref.register_token(regex(r"assertEqual"), move |_| { assert_equal_op.clone() });
    let assert_equal_to_result_op = Atom::gnd(AssertEqualToResultOp::new(space.clone()));
    tref.register_token(regex(r"assertEqualToResult"), move |_| { assert_equal_to_result_op.clone() });
    let superpose_op = Atom::gnd(SuperposeOp::new(space.clone()));
    tref.register_token(regex(r"superpose"), move |_| { superpose_op.clone() });
    let collapse_op = Atom::gnd(CollapseOp::new(space.clone()));
    tref.register_token(regex(r"collapse"), move |_| { collapse_op.clone() });
    let pragma_op = Atom::gnd(PragmaOp::new(metta.settings().clone()));
    tref.register_token(regex(r"pragma!"), move |_| { pragma_op.clone() });
    // &self should be updated
    // TODO: adding &self might be done not by stdlib, but by MeTTa itself.
    // TODO: adding &self introduces self referencing and thus prevents space
    // from being freed. There are two options to eliminate this. (1) use weak
    // pointer and somehow use the same type to represent weak and strong
    // pointers to the atomspace. (2) resolve &self in GroundingSpace::query
    // method without adding it into container.
    let self_atom = Atom::gnd(metta.space().clone());
    tref.register_token(regex(r"&self"), move |_| { self_atom.clone() });
}

pub fn register_rust_tokens(metta: &Metta) {
    let mut rust_tokens = Tokenizer::new();
    let tref = &mut rust_tokens;

    tref.register_token(regex(r"\d+"),
        |token| { Atom::gnd(Number::from_int_str(token)) });
    tref.register_token(regex(r"\d+(.\d+)([eE][\-\+]?\d+)?"),
        |token| { Atom::gnd(Number::from_float_str(token)) });
    tref.register_token(regex(r"True|False"),
        |token| { Atom::gnd(Bool::from_str(token)) });
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

    metta.tokenizer().borrow_mut().move_front(&mut rust_tokens);
}

pub static METTA_CODE: &'static str = include_str!("stdlib.metta");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::new_metta_rust;
    use crate::matcher::atoms_are_equivalent;

    use std::convert::TryFrom;

    fn run_program(program: &str) -> Result<Vec<Vec<Atom>>, String> {
        let metta = new_metta_rust();
        metta.run(&mut SExprParser::new(program))
    }

    #[test]
    fn get_type_op() {
        let space = DynSpace::new(metta_space("
            (: B Type)
            (: C Type)
            (: A B)
            (: A C)
        "));

        let get_type_op = GetTypeOp{};
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

        let get_type_op = GetTypeOp{};
        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "42"), expr!({space.clone()})]).unwrap(),
            vec![sym!("String")]);
        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "\"test\""), expr!({space.clone()})]).unwrap(),
            vec![EMPTY_SYMBOL]);
    }


    #[test]
    fn metta_car() {
        let result = run_program("!(eval (car (A $b)))");
        assert_eq!(result, Ok(vec![vec![expr!("A")]]));
        let result = run_program("!(eval (car ($a B)))");
        //assert_eq!(result, Ok(vec![vec![expr!(a)]]));
        assert!(result.is_ok_and(|res| res.len() == 1 && res[0].len() == 1 &&
            atoms_are_equivalent(&res[0][0], &expr!(a))));
        let result = run_program("!(eval (car ()))");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("car" ()) "\"car expects a non-empty expression as an argument\"")]]));
        let result = run_program("!(eval (car A))");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("car" "A") "\"car expects a non-empty expression as an argument\"")]]));
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
    fn metta_is_function() {
        let result = run_program("!(eval (is-function (-> $t)))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(true)})]]));
        let result = run_program("!(eval (is-function (A $t)))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
        let result = run_program("!(eval (is-function %Undefined%))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
    }

    #[test]
    fn metta_reduce_chain() {
        assert_eq!(run_program("
            (= (foo $x) (bar $x))
            (= (bar $x) (baz $x))
            (= (baz $x) $x)
            !(eval (reduce (foo A) $x (reduced $x)))
        "), Ok(vec![vec![expr!("reduced" "A")]]));
        assert_eq!(run_program("
            !(eval (reduce (foo A) $x (reduced $x)))
        "), Ok(vec![vec![expr!("reduced" ("foo" "A"))]]));
        assert_eq!(run_program("
            (= (foo A) (Error (foo A) \"Test error\"))
            !(eval (reduce (foo A) $x (reduced $x)))
        "), Ok(vec![vec![expr!("Error" ("foo" "A") "\"Test error\"")]]));
    }

    #[test]
    fn metta_reduce_reduce() {
        assert_eq!(run_program("
            (= (foo $x) (reduce (bar $x) $t $t))
            (= (bar $x) $x)
            !(eval (reduce (foo A) $x $x))
        "), Ok(vec![vec![expr!("A")]]));
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
    fn metta_assert_equal_op() {
        let metta = new_metta_rust();
        let assert = AssertEqualOp::new(metta.space().clone());
        let program = "
            (= (foo $x) $x)
            (= (bar $x) $x)
        ";
        assert_eq!(metta.run(&mut SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(&mut SExprParser::new("!(assertEqual (foo A) (bar A))")), Ok(vec![
            vec![VOID_SYMBOL],
        ]));
        assert_eq!(metta.run(&mut SExprParser::new("!(assertEqual (foo A) (bar B))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed result: B")],
        ]));
        assert_eq!(metta.run(&mut SExprParser::new("!(assertEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive result: A")]
        ]));
    }

    #[test]
    fn metta_assert_equal_to_result_op() {
        let metta = new_metta_rust();
        let assert = AssertEqualToResultOp::new(metta.space().clone());
        let program = "
            (= (foo) A)
            (= (foo) B)
            (= (bar) C)
            (= (baz) D)
            (= (baz) D)
        ";
        assert_eq!(metta.run(&mut SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(&mut SExprParser::new("!(assertEqualToResult (foo) (A B))")), Ok(vec![
            vec![VOID_SYMBOL],
        ]));
        assert_eq!(metta.run(&mut SExprParser::new("!(assertEqualToResult (bar) (A))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed result: A")],
        ]));
        assert_eq!(metta.run(&mut SExprParser::new("!(assertEqualToResult (baz) (D))")), Ok(vec![
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
    fn metta_let() {
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
    fn test_frog_reasoning() {
        let program = "
            (= (and True True) True)

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
    fn test_variable_defined_via_variabe() {
        let program = "
            (= (myif T $y) $y)
            (= (not F) T)
            (= (a $z) (not (b $z)))
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

        let metta = new_metta_rust();
        metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
            |_| Atom::gnd(ID_NUM));

        assert_eq!(metta.run(&mut SExprParser::new(program1)),
            Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));

        let program2 = "
            !(eval (interpret (id_num myAtom) %Undefined% &self))
        ";

        assert_eq!(metta.run(&mut SExprParser::new(program2)),
            Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));
    }
}
