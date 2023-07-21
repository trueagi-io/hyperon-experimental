use crate::*;
use crate::matcher::MatchResultIter;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::runner::Metta;
use crate::metta::types::{get_atom_types, get_meta_type};

use std::fmt::Display;
use std::path::PathBuf;
use regex::Regex;
use std::convert::TryInto;

use super::arithmetics::*;

#[derive(Clone, PartialEq, Debug)]
pub struct CarAtomOp {}

impl Display for CarAtomOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "car-atom")
    }
}

impl Grounded for CarAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("car-atom expects one argument: expression");
        let expr = args.get(0).ok_or_else(arg_error)?;
        let expr: &ExpressionAtom = expr.try_into().map_err(|_| arg_error())?;
        let chld = expr.children();
        let car = chld.get(0).ok_or("car-atom expects non-empty expression")?;
        Ok(vec![car.clone()])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

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
        Ok(get_atom_types(space, atom))
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


fn regex(regex: &str) -> Regex {
    Regex::new(regex).unwrap()
}

pub fn register_common_tokens(metta: &Metta) {
    let tokenizer = &metta.tokenizer;
    let mut tref = tokenizer.borrow_mut();

    let car_atom_op = Atom::gnd(CarAtomOp{});
    tref.register_token(regex(r"car-atom"), move |_| { car_atom_op.clone() });
    let get_type_op = Atom::gnd(GetTypeOp{});
    tref.register_token(regex(r"get-type"), move |_| { get_type_op.clone() });
    let get_meta_type_op = Atom::gnd(GetMetaTypeOp{});
    tref.register_token(regex(r"get-metatype"), move |_| { get_meta_type_op.clone() });
}

pub fn register_runner_tokens(metta: &Metta, _cwd: PathBuf) {
    let _space = &metta.space;
    let tokenizer = &metta.tokenizer;

    let mut tref = tokenizer.borrow_mut();

    // &self should be updated
    // TODO: adding &self might be done not by stdlib, but by MeTTa itself.
    // TODO: adding &self introduces self referencing and thus prevents space
    // from being freed. There are two options to eliminate this. (1) use weak
    // pointer and somehow use the same type to represent weak and strong
    // pointers to the atomspace. (2) resolve &self in GroundingSpace::query
    // method without adding it into container.
    let self_atom = Atom::gnd(metta.space.clone());
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

    metta.tokenizer.borrow_mut().move_front(&mut rust_tokens);
}

pub static METTA_CODE: &'static str = "

;`$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)

(: Error (-> Atom Atom ErrorType))

(= (switch $atom $cases)
  (chain (decons $cases) $list (eval (switch-internal $atom $list))))
(= (switch-internal $atom (($pattern $template) $tail))
  (match $atom $pattern $template (eval (switch $atom $tail))))

(= (subst $atom $var $templ)
  (match $atom $var $templ
    (Error (subst $atom $var $templ)
      \"subst expects a variable as a second argument\") ))

(= (reduce $atom $var $templ)
  (chain (eval $atom) $res
    (match $res (Error $a $m)
      (Error $a $m)
      (match $res Empty
        (eval (subst $atom $var $templ))
        (eval (reduce $res $var $templ)) ))))

(= (type-cast $atom $type $space)
  (chain (eval (get-type $atom $space)) $actual-type
    (eval (switch ($actual-type $type)
      (
        ((%Undefined% $_) $atom)
        (($_ %Undefined%) $atom)
        (($type $_) $atom)
        ($_ Empty) )))))

(= (is-function $type)
  (chain (eval (get-metatype $type)) $meta
    (eval (switch ($type $meta)
      (
        (($_ Expression)
          (chain (eval (car-atom $type)) $head
            (match $head -> True False) ))
        ($_ False) )))))

(= (interpret $atom $type $space)
  (chain (eval (get-metatype $atom)) $meta
    (eval (switch ($type $meta)
      (
        ((Atom $_meta) $atom)
        (($meta $meta) $atom)
        (($_type Variable) $atom)

        (($_type Symbol) (eval (type-cast $atom $type $space)))
        (($_type Grounded) (eval (type-cast $atom $type $space)))
        (($_type Expression) (eval (interpret-expression $atom $type $space))) )))))

(= (interpret-expression $atom $type $space)
  (chain (decons $atom) $list
    (match $list ($op $args)
      (chain (eval (get-type $op $space)) $op-type
        (chain (eval (is-function $op-type)) $is-func
          (match $is-func True
            (chain (eval (interpret-children $args $space)) $reduced_args
              (chain (cons $op $reduced_args) $reduced_atom (eval (call $reduced_atom %Undefined% $space))) )
            (chain (eval (interpret-children $atom $space)) $reduced_atom (eval (call $reduced_atom $type $space))) )))
      (eval (type-cast $atom $type $space)) )))

(= (interpret-children $atom $space)
  (chain (decons $atom) $list
    (match $list ($head $tail)
      (chain (eval (interpret $head %Undefined% $space)) $rhead
        (chain (eval (interpret-children $tail $space)) $rtail
          (chain (cons $rhead $rtail) $cons $cons)))
      $atom )))

(= (call $atom $type $space)
  (chain (eval $atom) $result
    (match $result Empty $atom
      (match $result (Error $_a $_m) $result
        (eval (interpret $result $type $space))) )))

";

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::new_metta_rust;

    fn run_program(program: &str) -> Result<Vec<Vec<Atom>>, String> {
        let metta = new_metta_rust();
        metta.run(&mut SExprParser::new(program))
    }

    #[test]
    fn car_atom_op() {
        let res = CarAtomOp{}.execute(&mut vec![expr!(("A" "C") "B")]).expect("No result returned");
        assert_eq!(res, vec![expr!("A" "C")]);
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
            Vec::<Atom>::new());
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
        assert_eq!(run_program("(: a A) !(eval (type-cast a B &self))"), Ok(vec![vec![]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a %Undefined% &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (type-cast a B &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (type-cast 42 Number &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("!(eval (type-cast 42 %Undefined% &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
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
        assert_eq!(run_program("(: a A) !(eval (interpret a B &self))"), Ok(vec![vec![]]));
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
    fn metta_interpret_children() {
        assert_eq!(run_program("!(eval (interpret-children () &self))"), Ok(vec![vec![expr!(())]]));
        assert_eq!(run_program("!(eval (interpret-children (a) &self))"), Ok(vec![vec![expr!(("a"))]]));
        assert_eq!(run_program("!(eval (interpret-children (a b) &self))"), Ok(vec![vec![expr!(("a" "b"))]]));
        assert_eq!(run_program("
            (= (foo $x) (bar $x))
            (= (bar $x) (baz $x))
            (= (baz $x) $x)
            !(eval (interpret-children ((foo A) (foo B)) &self))
        "), Ok(vec![vec![expr!("A" "B")]]));
    }

    #[test]
    fn metta_interpret_expression_as_type() {
        assert_eq!(run_program("(= (foo $x) $x) !(eval (interpret (foo a) %Undefined% &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (interpret (foo a) %Undefined% &self))"), Ok(vec![vec![expr!("foo" "a")]]));
        assert_eq!(run_program("!(eval (interpret () SomeType &self))"), Ok(vec![vec![expr!(())]]));
    }

    #[test]
    fn metta_frog_reasoning() {
        let program = "
            (= (and True True) True)

            (= (is Fritz croaks) True)
            (= (is Fritz eats-flies) True)

            (= (is Tweety chirps) True)
            (= (is Tweety yellow) True)
            (= (is Tweety eats-flies) True)

            !(eval (interpret (if (and (is $x croaks) (is $x eats-flies)) (= (is $x frog) True) Empty) %Undefined% &self))
        ";

        let result = run_program(program);
        assert_eq!(result, Ok(vec![vec![expr!("=" ("is" "Fritz" "frog") {Bool(true)})]]));
    }
}
