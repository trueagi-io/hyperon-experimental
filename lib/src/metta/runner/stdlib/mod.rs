#[macro_use]
pub mod math;
pub mod atom;
pub mod module;
#[cfg(feature = "pkg_mgmt")]
pub mod package;
pub mod string;
pub mod debug;
pub mod space;
pub mod core;
pub mod arithmetics;

use hyperon_atom::*;
use hyperon_space::*;
use crate::metta::*;
use crate::metta::text::{Tokenizer, SExprParser};
use hyperon_common::shared::Shared;
use crate::metta::runner::{Metta, RunContext, ModuleLoader, PragmaSettings};
use crate::metta::runner::modules::MettaMod;

use regex::Regex;

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

pub(crate) fn unit_result() -> Result<Vec<Atom>, ExecError> {
    Ok(vec![UNIT_ATOM])
}

pub(crate) fn regex(regex: &str) -> Regex {
    Regex::new(regex).unwrap()
}

pub fn interpret(space: DynSpace, expr: &Atom, settings: PragmaSettings) -> Result<Vec<Atom>, String> {
    let expr = Atom::expr([METTA_SYMBOL, expr.clone(), ATOM_TYPE_UNDEFINED, Atom::gnd(space.clone())]);
    let mut state = crate::metta::interpreter::interpret_init(space, &expr);
    
    if let Some(depth) = settings.get_string("max-stack-depth") {
        let depth = depth.parse::<usize>().unwrap();
        state.set_max_stack_depth(depth);
    }

    while state.has_next() {
        state = crate::metta::interpreter::interpret_step(state);
    }
    state.into_result()
}

//TODO: The additional arguments are a temporary hack on account of the way the operation atoms store references
// to the runner & module state.  https://github.com/trueagi-io/hyperon-experimental/issues/410
fn register_context_dependent_tokens(tref: &mut Tokenizer, tokenizer: Shared<Tokenizer>, space: &DynSpace, metta: &Metta) {

    atom::register_context_dependent_tokens(tref, space);
    core::register_context_dependent_tokens(tref, space, metta);
    module::register_context_dependent_tokens(tref, tokenizer.clone(), metta);
    #[cfg(feature = "pkg_mgmt")]
    package::register_context_dependent_tokens(tref, metta);

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

fn register_context_independent_tokens(tref: &mut Tokenizer) {
    atom::register_context_independent_tokens(tref);
    core::register_context_independent_tokens(tref);
    debug::register_context_independent_tokens(tref);
    math::register_context_independent_tokens(tref);
    arithmetics::register_context_independent_tokens(tref);
    string::register_context_independent_tokens(tref);
    space::register_context_independent_tokens(tref);
}


pub(crate) fn register_all_corelib_tokens(tref: &mut Tokenizer, tokenizer: Shared<Tokenizer>, space: &DynSpace, metta: &Metta){
    register_context_dependent_tokens(tref, tokenizer.clone(),space, metta);
    register_context_independent_tokens(tref);
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
        let space = GroundingSpace::new();
        context.init_self_module(space.into(), None);

        self.load_tokens(context.module(), context.metta.clone())?;

        let parser = SExprParser::new(METTA_CODE);
        context.push_parser(Box::new(parser));
        Ok(())
    }

    fn load_tokens(&self, target: &MettaMod, metta: Metta) -> Result<(), String> {
        let tokenizer = target.tokenizer();
        register_all_corelib_tokens(tokenizer.borrow_mut().deref_mut(),
            tokenizer.clone(), &target.space(), &metta);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::text::SExprParser;
    use crate::metta::runner::EnvBuilder;
    use crate::metta::runner::str::Str;
    use hyperon_atom::matcher::atoms_are_equivalent;
    use crate::metta::runner::bool::Bool;
    use crate::metta::runner::number::Number;
    use crate::metta::runner::run_program;
    use hyperon_atom::gnd::GroundedFunctionAtom;
    use hyperon_common::assert_eq_metta_results;

    use std::fmt::Display;

    #[test]
    fn metta_switch() {
        let result = run_program("!(switch (A $b) ( (($a B) ($b $a)) ((B C) (C B)) ))");
        assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
        let result = run_program("!(switch (A $b) ( ((B C) (C B)) (($a B) ($b $a)) ))");
        assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
        let result = run_program("!(switch (A $b) ( ((B C) (C B)) ((D E) (E B)) ))");
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn metta_is_function() {
        let result = run_program("!(is-function (-> $t))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(true)})]]));
        let result = run_program("!(is-function (A $t))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
        let result = run_program("!(is-function %Undefined%)");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
    }

    #[test]
    fn metta_type_cast() {
        assert_eq!(run_program("(: a A) !(type-cast a A &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(type-cast a B &self)"), Ok(vec![vec![expr!("Error" "a" "BadType")]]));
        assert_eq!(run_program("(: a A) !(type-cast a %Undefined% &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(type-cast a B &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(type-cast 42 Number &self)"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("!(type-cast 42 %Undefined% &self)"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("(: a A) !(type-cast a Atom &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(type-cast a Symbol &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(type-cast 42 Grounded &self)"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("!(type-cast () Expression &self)"), Ok(vec![vec![expr!()]]));
        assert_eq!(run_program("!(type-cast (a b) Expression &self)"), Ok(vec![vec![expr!("a" "b")]]));
        assert_eq!(run_program("!(type-cast $v Variable &self)"), Ok(vec![vec![expr!(v)]]));
        assert_eq!(run_program("(: a A) (: b B) !(type-cast (a b) (A B) &self)"), Ok(vec![vec![expr!("a" "b")]]));
        assert_eq!(run_program("(: a A) (: a B) !(type-cast a A &self)"), Ok(vec![vec![expr!("a")]]));
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
            !(assertEqual
                (metta (foo b) %Undefined% &self)
                (Error (foo b) (BadType 1 T B)))
        ");
        assert_eq!(result, Ok(vec![vec![UNIT_ATOM]]));
        let result = run_program("
            (: Nil (List $t))
            (: Z Nat)
            (: S (-> Nat Nat))
            (: Cons (-> $t (List $t) (List $t)))
            !(assertEqual
                (metta (Cons S (Cons Z Nil)) %Undefined% &self)
                (Error (Cons S (Cons Z Nil)) (BadType 2 (List (-> Nat Nat)) (List Nat))))
        ");
        assert_eq!(result, Ok(vec![vec![UNIT_ATOM]]));
        let result = run_program("
            (: (a b) (C D))

            (: foo (-> (A B) %Undefined%))
            (= (foo $x) succ)

            !(assertEqual
                (foo (a b))
                (Error (foo (a b)) (BadType 1 (A B) (C D))))
        ");
        assert_eq!(result, Ok(vec![vec![UNIT_ATOM]]));
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
    fn metta_issue_872() {
        assert_eq!(run_program("!(bind! &i 0) !(assertEqual (bind! &i 1) (Error (bind! 0 1) (BadType 1 Symbol Number)))"),
                   Ok(vec![vec![UNIT_ATOM], vec![UNIT_ATOM]]));
    }

    #[test]
    fn metta_interpret_single_atom_with_two_types() {
        let result = run_program("(: a A) (: a B) !(metta a %Undefined% &self)");
        assert_eq!(result, Ok(vec![vec![expr!("a")]]));
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
    fn metta_add_reducts() {
        assert_eq!(run_program(&format!("!(let $newspace (new-space) (add-reducts k1))")), Ok(vec![vec![expr!("Error" ("add-reducts" "k1") "IncorrectNumberOfArguments")]]));

        assert_eq!(run_program(&format!("!(let $newspace (new-space)
                                                 (assertEqual
                                                     (add-reducts $newspace ((k1 v1) (k2 v2) (k3 v3)))
                                                     ()))")),
                   Ok(vec![vec![UNIT_ATOM]]));

        assert_eq!(run_program(&format!("!(let $newspace (new-space)
                                                    (let $f (add-reducts $newspace ((k1 v1) (k2 v2) (k3 v3)))
                                                        (assertEqual
                                                            (match $newspace (k1 $v) $v)
                                                            v1)))")),
                   Ok(vec![vec![UNIT_ATOM]]));

        assert_eq!(run_program(&format!("(= (pair1) (k1 v1))
                                                 (= (pair2) (k2 v2))
                                                 (= (pair3) (k3 v3))
                                                 !(let $newspace (new-space)
                                                    (let $f (add-reducts $newspace ((pair1) (pair2) (pair3)))
                                                        (assertEqual
                                                            (match $newspace (k1 $v) $v)
                                                            v1)))")),
                   Ok(vec![vec![UNIT_ATOM]]));
    }

    #[test]
    fn metta_add_atoms() {
        assert_eq!(run_program(&format!("!(let $newspace (new-space) (add-atoms k1))")), Ok(vec![vec![expr!("Error" ("add-atoms" "k1") "IncorrectNumberOfArguments")]]));

        assert_eq!(run_program(&format!("!(let $newspace (new-space)
                                                 (assertEqual
                                                     (add-atoms $newspace ((k1 v1) (k2 v2) (k3 v3)))
                                                     ()))")),
                   Ok(vec![vec![UNIT_ATOM]]));

        assert_eq!(run_program(&format!("!(let $newspace (new-space)
                                                    (let $f (add-atoms $newspace ((k1 v1) (k2 v2) (k3 v3)))
                                                        (assertEqual
                                                            (match $newspace (k1 $v) $v)
                                                            v1)))")),
                   Ok(vec![vec![UNIT_ATOM]]));

        assert_eq!(run_program(&format!("(= (pair1) (k1 v1))
                                                 (= (pair2) (k2 v2))
                                                 (= (pair3) (k3 v3))
                                                 !(let $newspace (new-space)
                                                    (let $f (add-atoms $newspace ((pair1) (pair2) (pair3)))
                                                        (assertEqual
                                                            (match $newspace (k1 $v) $v)
                                                            (empty))))")),
                   Ok(vec![vec![UNIT_ATOM]]));
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

    fn id_num(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("id_num expects one argument: number");
        let num = args.get(0).ok_or_else(arg_error)?;
        Ok(vec![num.clone()])
    }

    #[test]
    fn test_return_bad_type_error() {
        let program1 = "
            (: myAtom myType)
            (: id_a (-> A A))
            (= (id_a $a) $a)

            !(assertEqual
                (metta (id_a myAtom) %Undefined% &self)
                (Error (id_a myAtom) (BadType 1 A myType)))
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_function(
            GroundedFunctionAtom::new("id_num".into(), expr!("->" "Number" "Number"), id_num));

        assert_eq!(metta.run(SExprParser::new(program1)),
            Ok(vec![vec![UNIT_ATOM]]));

        let program2 = "
            !(assertEqual
                (metta (id_num myAtom) %Undefined% &self)
                (Error (id_num myAtom) (BadType 1 Number myType)))
        ";

        assert_eq!(metta.run(SExprParser::new(program2)),
            Ok(vec![vec![UNIT_ATOM]]));
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
        metta.tokenizer().borrow_mut().register_function(
            GroundedFunctionAtom::new("id_num".into(), expr!("->" "Number" "Number"), id_num));

        assert_eq!(metta.run(SExprParser::new(program1)),
            Ok(vec![vec![expr!("c")]]));

        let program2 = "!(metta (foo a) %Undefined% &self)";

        assert_eq!(metta.run(SExprParser::new(program2)),
            Ok(vec![vec![expr!("Error" ("foo" "a") "IncorrectNumberOfArguments")]]));

        let program3 = "!(metta (foo a b c) %Undefined% &self)";

        assert_eq!(metta.run(SExprParser::new(program3)),
            Ok(vec![vec![expr!("Error" ("foo" "a" "b" "c") "IncorrectNumberOfArguments")]]));
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
    fn let_op_keep_variables_equalities_issue290() {
        assert_eq_metta_results!(run_program("!(let* (($f f) ($f $x)) $x)"), Ok(vec![vec![expr!("f")]]));
        assert_eq_metta_results!(run_program("!(let* (($f $x) ($f f)) $x)"), Ok(vec![vec![expr!("f")]]));
        assert_eq_metta_results!(run_program("!(let (quote ($x $x)) (quote ($z $y)) (let $y A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
        assert_eq_metta_results!(run_program("!(let (quote ($x $x)) (quote ($z $y)) (let $z A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
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
