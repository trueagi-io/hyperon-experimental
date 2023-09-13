//! MeTTa assembly language implementation.
//!
//! # Algorithm
//!
//! FIXME: explain an algorithm

use crate::*;
use crate::atom::matcher::*;
use crate::space::*;
use crate::space::grounding::*;
use crate::metta::*;

use std::fmt::{Debug, Display, Formatter};
use std::convert::TryFrom;

/// Result of atom interpretation plus variable bindings found
#[derive(Clone, PartialEq)]
pub struct InterpretedAtom(Atom, Bindings);

impl Display for InterpretedAtom {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.1.is_empty() {
            write!(f, "{}", self.0)
        } else {
            // TODO: it is possible to cleanup all bindings for nested
            // expressions which were introduced by matching when all
            // sub-expressions are interpreted. This will simplify
            // textual representation. For example in test_air_humidity_regulator
            // (make air wet) leads to (start kettle), {$y: kettle}) result
            // but $y is not present in the expression after interpreting
            // (make air wet) and can be removed.
            write!(f, "{}|{}", self.0, self.1)
        }
    }
}

impl Debug for InterpretedAtom {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

use std::marker::PhantomData;

pub trait SpaceRef<'a> : Space + 'a {}
impl<'a, T: Space + 'a> SpaceRef<'a> for T {}

struct InterpreterContext<'a, T: SpaceRef<'a>> {
    space: T,
    phantom: PhantomData<&'a GroundingSpace>,
}

impl<'a, T: SpaceRef<'a>> InterpreterContext<'a, T> {
    fn new(space: T) -> Self {
        Self{ space, phantom: PhantomData }
    }
}

pub struct InterpreterState<'a, T: SpaceRef<'a>> {
    plan: Vec<InterpretedAtom>,
    finished: Vec<Atom>,
    context: InterpreterContext<'a, T>,
}

fn atom_as_slice(atom: &Atom) -> Option<&[Atom]> {
    <&[Atom]>::try_from(atom).ok()
}

// FIXME: return incorrect length as error_atom() from caller
fn atom_into_array<const N: usize>(atom: Atom) -> Option<[Atom; N]> {
    <[Atom; N]>::try_from(atom).ok()
}

impl<'a, T: SpaceRef<'a>> InterpreterState<'a, T> {

    /// INTERNAL USE ONLY. Create an InterpreterState that is ready to yield results
    #[allow(dead_code)] //TODO: only silence the warning until interpreter2 replaces interpreter
    pub(crate) fn new_finished(space: T, results: Vec<Atom>) -> Self {
        Self {
            plan: vec![],
            finished: results,
            context: InterpreterContext::new(space),
        }
    }

    pub fn has_next(&self) -> bool {
        !self.plan.is_empty()
    }

    pub fn into_result(self) -> Result<Vec<Atom>, String> {
        if self.has_next() {
            Err("Evaluation is not finished".into())
        } else {
            Ok(self.finished)
        }
    }

    fn pop(&mut self) -> Option<InterpretedAtom> {
        self.plan.pop()
    }

    fn push(&mut self, atom: InterpretedAtom) {
        if is_embedded_op(&atom.0) {
            self.plan.push(atom);
        } else {
            let InterpretedAtom(atom, _bindings) = atom;
            if atom != EMPTY_SYMBOL {
                self.finished.push(atom);
            }
        }
    }
}

impl<'a, T: SpaceRef<'a>> std::fmt::Display for InterpreterState<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}\n", self.plan)
    }
}


/// Initialize interpreter and returns the result of the zero step.
/// It can be error, immediate result or interpretation plan to be executed.
/// See [crate::metta::interpreter] for algorithm explanation.
///
/// # Arguments
/// * `space` - atomspace to query for interpretation
/// * `expr` - atom to interpret
pub fn interpret_init<'a, T: Space + 'a>(space: T, expr: &Atom) -> InterpreterState<'a, T> {
    let context = InterpreterContext::new(space);
    InterpreterState {
        plan: vec![InterpretedAtom(expr.clone(), Bindings::new())],
        finished: vec![],
        context
    }
}

//TODO: These docs are out of date for the new interpreter
/// Perform next step of the interpretation plan and return the result. Panics
/// when [StepResult::Return] or [StepResult::Error] are passed as input.
/// See [crate::metta::interpreter] for algorithm explanation.
///
/// # Arguments
/// * `step` - [StepResult::Execute] result from the previous step.
pub fn interpret_step<'a, T: Space + 'a>(mut state: InterpreterState<'a, T>) -> InterpreterState<'a, T> {
    let interpreted_atom = state.pop().unwrap();
    log::debug!("interpret_step: {:?}", interpreted_atom);
    for result in interpret_atom(&state.context.space, interpreted_atom) {
        state.push(result);
    }
    state
}

/// Interpret passed atom and return a new plan, result or error. This function
/// blocks until result is calculated. For step by step interpretation one
/// should use [interpret_init] and [interpret_step] functions.
/// # Arguments
/// * `space` - atomspace to query for interpretation
/// * `expr` - atom to interpret
pub fn interpret<T: Space>(space: T, expr: &Atom) -> Result<Vec<Atom>, String> {
    let mut state = interpret_init(space, expr);
    while state.has_next() {
        state = interpret_step(state);
    }
    state.into_result()
}

fn is_embedded_op(atom: &Atom) -> bool {
    let expr = atom_as_slice(&atom);
    match expr {
        Some([op, ..]) => *op == EVAL_SYMBOL
            || *op == CHAIN_SYMBOL
            || *op == UNIFY_SYMBOL
            || *op == CONS_SYMBOL
            || *op == DECONS_SYMBOL,
        _ => false,
    }
}

fn interpret_atom<'a, T: SpaceRef<'a>>(space: T, interpreted_atom: InterpretedAtom) -> Vec<InterpretedAtom> {
    interpret_atom_root(space, interpreted_atom, true)
}

fn interpret_atom_root<'a, T: SpaceRef<'a>>(space: T, interpreted_atom: InterpretedAtom, root: bool) -> Vec<InterpretedAtom> {
    let InterpretedAtom(atom, bindings) = interpreted_atom;
    let expr = atom_as_slice(&atom);
    let mut result = match expr {
        Some([op, args @ ..]) if *op == EVAL_SYMBOL => {
            match args {
                [_atom] => {
                    // atom is matched twice to not reconstruct atom in case of
                    // error (see error branch below), don't think it is a best
                    // way, but don't see a better solution
                    match atom_into_array(atom) {
                        Some([_, atom]) => eval(space, atom, bindings),
                        _ => panic!("Unexpected state"),
                    }
                },
                _ => {
                    let error: String = format!("expected: ({} <atom>), found: {}", EVAL_SYMBOL, atom);
                    vec![InterpretedAtom(error_atom(atom, error), bindings)]
                },
            }
        },
        Some([op, args @ ..]) if *op == CHAIN_SYMBOL => {
            match args {
                [_nested, Atom::Variable(_var), _templ] => {
                    match atom_into_array(atom) {
                        Some([_, nested, Atom::Variable(var), templ]) =>
                            chain(space, bindings, nested, var, templ),
                        _ => panic!("Unexpected state"),
                    }
                },
                _ => {
                    let error: String = format!("expected: ({} <nested> (: <var> Variable) <templ>), found: {}", CHAIN_SYMBOL, atom);
                    vec![InterpretedAtom(error_atom(atom, error), bindings)]
                },
            }
        },
        Some([op, args @ ..]) if *op == UNIFY_SYMBOL => {
            match args {
                [atom, pattern, then, else_] => match_(bindings, atom, pattern, then, else_),
                _ => {
                    let error: String = format!("expected: ({} <atom> <pattern> <then> <else>), found: {}", UNIFY_SYMBOL, atom);
                    vec![InterpretedAtom(error_atom(atom, error), bindings)]
                },
            }
        },
        Some([op, args @ ..]) if *op == DECONS_SYMBOL => {
            match args {
                [Atom::Expression(tail)] if tail.children().len() > 0 => {
                    match atom_into_array(atom) {
                        Some([_, Atom::Expression(expr)]) => decons(bindings, expr),
                        _ => panic!("Unexpected state"),
                    }
                },
                _ => {
                    let error: String = format!("expected: ({} (: <expr> Expression)), found: {}", DECONS_SYMBOL, atom);
                    vec![InterpretedAtom(error_atom(atom, error), bindings)]
                },
            }
        },
        Some([op, args @ ..]) if *op == CONS_SYMBOL => {
            match args {
                [_head, Atom::Expression(_tail)] => {
                    match atom_into_array(atom) {
                        Some([_, head, Atom::Expression(tail)]) => cons(bindings, head, tail),
                        _ => panic!("Unexpected state"),
                    }
                },
                _ => {
                    let error: String = format!("expected: ({} <head> (: <tail> Expression)), found: {}", CONS_SYMBOL, atom);
                    vec![InterpretedAtom(error_atom(atom, error), bindings)]
                },
            }
        },
        _ => {
            vec![InterpretedAtom(return_atom(atom), bindings)]
        },
    };
    if root {
        result.iter_mut().for_each(|interpreted| {
            let InterpretedAtom(atom, bindings) = interpreted;
            bindings.cleanup(&atom.iter().filter_type::<&VariableAtom>().collect());
        });
    }
    result
}

fn return_unit() -> Atom {
    VOID_SYMBOL
}

fn return_not_reducible() -> Atom {
    NOT_REDUCIBLE_SYMBOL
}

fn error_atom(atom: Atom, err: String) -> Atom {
    Atom::expr([Atom::sym("Error"), atom, Atom::sym(err)])
}

fn return_atom(atom: Atom) -> Atom {
    atom
}

fn eval<'a, T: SpaceRef<'a>>(space: T, atom: Atom, bindings: Bindings) -> Vec<InterpretedAtom> {
    match atom_as_slice(&atom) {
        Some([Atom::Grounded(op), args @ ..]) => {
            match op.execute(args) {
                Ok(results) => {
                    if results.is_empty() {
                        // TODO: This is an open question how to interpret empty results
                        // which are returned by grounded function. There is no
                        // case to return empty result for now. If alternative
                        // should be remove from plan Empty is a proper result.
                        // If grounded atom returns no value Void should be returned.
                        // NotReducible or Exec::NoReduce can be returned to
                        // let a caller know that function is not defined on a
                        // passed input data. Thus we can interpreter empty result
                        // by any way we like.
                        vec![InterpretedAtom(return_unit(), bindings)]
                    } else {
                        results.into_iter()
                            .map(|atom| InterpretedAtom(atom, bindings.clone()))
                            .collect()
                    }
                },
                Err(ExecError::Runtime(err)) =>
                    vec![InterpretedAtom(error_atom(atom, err), bindings)],
                Err(ExecError::NoReduce) =>
                    // TODO: we could remove ExecError::NoReduce and explicitly
                    // return NOT_REDUCIBLE_SYMBOL from the grounded function instead.
                    vec![InterpretedAtom(return_not_reducible(), bindings)],
            }
        },
        _ => query(space, atom, bindings),
    }
}

fn query<'a, T: SpaceRef<'a>>(space: T, atom: Atom, bindings: Bindings) -> Vec<InterpretedAtom> {
    let var = VariableAtom::new("X").make_unique();
    let query = Atom::expr([EQUAL_SYMBOL, atom, Atom::Variable(var.clone())]);
    let results = space.query(&query);
    if results.is_empty() {
        vec![InterpretedAtom(return_not_reducible(), bindings)]
    } else {
        results.into_iter()
            .flat_map(|mut b| {
                let atom = b.resolve_and_remove(&var).unwrap();
                let bindings = b.merge_v2(&bindings);
                bindings.into_iter().map(move |b| {
                    let atom = apply_bindings_to_atom(&atom, &b);
                    InterpretedAtom(atom, b)
                })
            })
            .collect()
    }
}

fn chain<'a, T: SpaceRef<'a>>(space: T, bindings: Bindings, nested: Atom, var: VariableAtom, templ: Atom) -> Vec<InterpretedAtom> {
    if is_embedded_op(&nested) {
        let mut result = interpret_atom_root(space, InterpretedAtom(nested, bindings), false);
        if result.len() == 1 {
            let InterpretedAtom(r, b) = result.pop().unwrap();
            vec![InterpretedAtom(Atom::expr([CHAIN_SYMBOL, r, Atom::Variable(var), templ]), b)]
        } else {
            result.into_iter()
                .map(|InterpretedAtom(r, b)| {
                    InterpretedAtom(Atom::expr([CHAIN_SYMBOL, r, Atom::Variable(var.clone()), templ.clone()]), b)
                })
            .collect()
        }
    } else {
        let b = Bindings::new().add_var_binding_v2(var, nested).unwrap();
        let result = apply_bindings_to_atom(&templ, &b);
        vec![InterpretedAtom(result, bindings)]
    }
}

fn match_(bindings: Bindings, atom: &Atom, pattern: &Atom, then: &Atom, else_: &Atom) -> Vec<InterpretedAtom> {
    // TODO: Should match_() be symmetrical or not. While it is symmetrical then
    // if variable is matched by variable then both variables have the same
    // priority. Thus interpreter can use any of them further. This sometimes
    // looks unexpected. For example see `metta_car` unit test where variable
    // from car's argument is replaced.
    let matches: Vec<Bindings> = match_atoms(atom, pattern).collect();
    if matches.is_empty() {
        let result = apply_bindings_to_atom(else_, &bindings);
        vec![InterpretedAtom(result, bindings)]
    } else {
        matches.into_iter()
            .flat_map(|b| {
                let then = apply_bindings_to_atom(then, &b);
                b.merge_v2(&bindings).into_iter().map(move |b| {
                    InterpretedAtom(then.clone(), b)
                })
            })
        .collect()
    }
}

fn decons(bindings: Bindings, expr: ExpressionAtom) -> Vec<InterpretedAtom> {
    let result = match expr.children().len() {
        0 => InterpretedAtom(Atom::expr([]), bindings),
        _ => {
            let mut children = expr.into_children();
            let head = children.remove(0);
            let tail = children;
            InterpretedAtom(Atom::expr([head, Atom::expr(tail)]), bindings)
        },
    };
    vec![result]
}

fn cons(bindings: Bindings, head: Atom, tail: ExpressionAtom) -> Vec<InterpretedAtom> {
    let mut children = vec![head];
    children.extend(tail.into_children());
    vec![InterpretedAtom(Atom::expr(children), bindings)]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interpret_atom_evaluate_incorrect_args() {
        assert_eq!(interpret_atom(&space(""), atom("(eval)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("eval") "expected: (eval <atom>), found: (eval)"), bind!{})]);
        assert_eq!(interpret_atom(&space(""), atom("(eval a b)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("eval" "a" "b") "expected: (eval <atom>), found: (eval a b)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_atom() {
        let result = interpret_atom(&space("(= a b)"), atom("(eval a)", bind!{}));
        assert_eq!(result, vec![atom("b", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_atom_no_definition() {
        let result = interpret_atom(&space(""), atom("(eval a)", bind!{}));
        assert_eq!(result, vec![atom("NotReducible", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_empty_expression() {
        let result = interpret_atom(&space(""), atom("(eval ())", bind!{}));
        assert_eq!(result, vec![atom("NotReducible", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_value() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!("eval" {6}), bind!{}));
        assert_eq!(result, vec![atom("NotReducible", bind!{})]);
    }


    #[test]
    fn interpret_atom_evaluate_pure_expression() {
        let space = space("(= (foo $a B) $a)");
        let result = interpret_atom(&space, atom("(eval (foo A $b))", bind!{}));
        assert_eq!(result, vec![atom("A", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_non_determinism() {
        let space = space("
            (= color red)
            (= color green)
            (= color blue)
        ");
        let result = interpret_atom(&space, atom("(eval color)", bind!{}));
        assert_eq_no_order!(result, vec![
            atom("red", bind!{}),
            atom("green", bind!{}),
            atom("blue", bind!{})
        ]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_no_definition() {
        let result = interpret_atom(&space(""), atom("(eval (foo A))", bind!{}));
        assert_eq!(result, vec![atom("NotReducible", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_variable_name_conflict() {
        let space = space("(= (foo ($W)) True)");
        let result = interpret_atom(&space, atom("(eval (foo $W))", bind!{}));
        assert_eq!(result[0].0, sym!("True"));
    }


    #[test]
    fn interpret_atom_evaluate_grounded_expression() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!("eval" ({MulXUndefinedType(7)} {6})), bind!{}));
        assert_eq!(result, vec![InterpretedAtom(expr!({42}), bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_empty() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!("eval" ({ReturnNothing()} {6})), bind!{}));
        assert_eq!(result, vec![atom("Void", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_noreduce() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!("eval" ({NonReducible()} {6})), bind!{}));
        assert_eq!(result, vec![InterpretedAtom(expr!("NotReducible"), bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_error() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!("eval" ({ThrowError()} {"Test error"})), bind!{}));
        assert_eq!(result, vec![InterpretedAtom(expr!("Error" ({ThrowError()} {"Test error"}) "Test error"), bind!{})]);
    }


    #[test]
    fn interpret_atom_chain_incorrect_args() {
        assert_eq!(interpret_atom(&space(""), atom("(chain n $v t o)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("chain" "n" v "t" "o") "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v t o)"), bind!{})]);
        assert_eq!(interpret_atom(&space(""), atom("(chain n v t)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("chain" "n" "v" "t") "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n v t)"), bind!{})]);
        assert_eq!(interpret_atom(&space(""), atom("(chain n $v)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("chain" "n" v) "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_chain_atom() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!("chain" ("A" () {6} y) x ("bar" x)), bind!{}));
        assert_eq!(result, vec![InterpretedAtom(expr!("bar" ("A" () {6} y)), bind!{})]);
    }


    #[test]
    fn interpret_atom_chain_evaluation() {
        let space = space("(= (foo $a B) $a)");
        let result = interpret_atom(&space, atom("(chain (eval (foo A $b)) $x (bar $x))", bind!{}));
        assert_eq!(result, vec![atom("(chain A $x (bar $x))", bind!{})]);
    }

    #[test]
    fn interpret_atom_chain_nested_evaluation() {
        let space = space("(= (foo $a B) $a)");
        let result = interpret_atom(&space, atom("(chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y))", bind!{}));
        assert_eq!(result, vec![atom("(chain (chain A $x (bar $x)) $y (baz $y))", bind!{})]);
    }

    #[test]
    fn interpret_atom_chain_nested_value() {
        let result = interpret_atom(&space(""), atom("(chain (chain A $x (bar $x)) $y (baz $y))", bind!{}));
        assert_eq!(result, vec![atom("(chain (bar A) $y (baz $y))", bind!{})]);
    }

    #[test]
    fn interpret_atom_chain_expression_non_determinism() {
        let space = space("
            (= (color) red)
            (= (color) green)
            (= (color) blue)
        ");
        let result = interpret_atom(&space, atom("(chain (eval (color)) $x (bar $x))", bind!{}));
        assert_eq_no_order!(result, vec![
            atom("(chain red $x (bar $x))", bind!{}),
            atom("(chain green $x (bar $x))", bind!{}),
            atom("(chain blue $x (bar $x))", bind!{})
        ]);
    }

    #[test]
    fn interpret_atom_chain_return() {
        let result = interpret_atom(&space(""), atom("(chain Empty $x (bar $x))", bind!{}));
        assert_eq!(result, vec![atom("(bar Empty)", bind!{})]);
    }


    #[test]
    fn interpret_atom_match_incorrect_args() {
        assert_eq!(interpret_atom(&space(""), atom("(unify a p t e o)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("unify" "a" "p" "t" "e" "o") "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t e o)"), bind!{})]);
        assert_eq!(interpret_atom(&space(""), atom("(unify a p t)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("unify" "a" "p" "t") "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_match_then() {
        let result = interpret_atom(&space(""), atom("(unify (A $b) ($a B) ($a $b) Empty)", bind!{}));
        assert_eq!(result, vec![atom("(A B)", bind!{})]);
    }

    #[test]
    fn interpret_atom_match_else() {
        let result = interpret_atom(&space(""), atom("(unify (A $b C) ($a B D) ($a $b) Empty)", bind!{}));
        assert_eq!(result, vec![atom("Empty", bind!{})]);
    }


    #[test]
    fn interpret_atom_decons_incorrect_args() {
        assert_eq!(interpret_atom(&space(""), atom("(decons a)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("decons" "a") "expected: (decons (: <expr> Expression)), found: (decons a)"), bind!{})]);
        assert_eq!(interpret_atom(&space(""), atom("(decons (a) (b))", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("decons" ("a") ("b")) "expected: (decons (: <expr> Expression)), found: (decons (a) (b))"), bind!{})]);
        assert_eq!(interpret_atom(&space(""), atom("(decons)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("decons") "expected: (decons (: <expr> Expression)), found: (decons)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_decons_empty() {
        let result = interpret_atom(&space(""), atom("(decons ())", bind!{}));
        assert_eq!(result, vec![InterpretedAtom(expr!("Error" ("decons" ()) "expected: (decons (: <expr> Expression)), found: (decons ())"), bind!{})]);
    }

    #[test]
    fn interpret_atom_decons_single() {
        let result = interpret_atom(&space(""), atom("(decons (a))", bind!{}));
        assert_eq!(result, vec![atom("(a ())", bind!{})]);
    }

    #[test]
    fn interpret_atom_decons_list() {
        let result = interpret_atom(&space(""), atom("(decons (a b c))", bind!{}));
        assert_eq!(result, vec![atom("(a (b c))", bind!{})]);
    }


    #[test]
    fn interpret_atom_cons_incorrect_args() {
        assert_eq!(interpret_atom(&space(""), atom("(cons a (e) o)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("cons" "a" ("e") "o") "expected: (cons <head> (: <tail> Expression)), found: (cons a (e) o)"), bind!{})]);
        assert_eq!(interpret_atom(&space(""), atom("(cons a e)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("cons" "a" "e") "expected: (cons <head> (: <tail> Expression)), found: (cons a e)"), bind!{})]);
        assert_eq!(interpret_atom(&space(""), atom("(cons a)", bind!{})),
            vec![InterpretedAtom(expr!("Error" ("cons" "a") "expected: (cons <head> (: <tail> Expression)), found: (cons a)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_cons_empty() {
        let result = interpret_atom(&space(""), atom("(cons a ())", bind!{}));
        assert_eq!(result, vec![atom("(a)", bind!{})]);
    }

    #[test]
    fn interpret_atom_cons_single() {
        let result = interpret_atom(&space(""), atom("(cons a (b))", bind!{}));
        assert_eq!(result, vec![atom("(a b)", bind!{})]);
    }

    #[test]
    fn interpret_atom_cons_list() {
        let result = interpret_atom(&space(""), atom("(cons a (b c))", bind!{}));
        assert_eq!(result, vec![atom("(a b c)", bind!{})]);
    }

    #[test]
    fn metta_turing_machine() {
        let space = space("
            (= (tm $rule $state $tape)
              (unify $state HALT
                $tape
                (chain (eval (read $tape)) $char
                  (chain (eval ($rule $state $char)) $res
                    (unify $res ($next-state $next-char $dir)
                      (chain (eval (move $tape $next-char $dir)) $next-tape
                        (eval (tm $rule $next-state $next-tape)) )
                      (Error (tm $rule $state $tape) \"Incorrect state\") )))))

            (= (read ($head $hole $tail)) $hole)

            (= (move ($head $hole $tail) $char N) ($head $char $tail))
            (= (move ($head $hole $tail) $char L)
              (chain (cons $char $head) $next-head
                (chain (decons $tail) $list
                  (unify $list ($next-hole $next-tail)
                    ($next-head $next-hole $next-tail)
                    ($next-head 0 ()) ))))
            (= (move ($head $hole $tail) $char R)
              (chain (cons $char $tail) $next-tail
                (chain (decons $head) $list
                  (unify $list ($next-hole $next-head)
                    ($next-head $next-hole $next-tail)
                    (() 0 $next-tail) ))))

            (= (busy-beaver A 0) (B 1 R))
            (= (busy-beaver A 1) (C 1 L))

            (= (busy-beaver B 0) (A 1 L))
            (= (busy-beaver B 1) (B 1 R))

            (= (busy-beaver C 0) (B 1 L))
            (= (busy-beaver C 1) (HALT 1 N))

        ");
        let result = interpret(space, &metta_atom("(eval (tm busy-beaver A (() 0 ())))"));
        assert_eq!(result, Ok(vec![metta_atom("((1 1) 1 (1 1 1))")]));
    }


    fn atom(text: &str, bindings: Bindings) -> InterpretedAtom {
        InterpretedAtom(metta_atom(text), bindings)
    }

    fn space(text: &str) -> GroundingSpace {
        metta_space(text)
    }

    #[derive(PartialEq, Clone, Debug)]
    struct ThrowError();

    impl Grounded for ThrowError {
        fn type_(&self) -> Atom {
            expr!("->" "&str" "Error")
        }
        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Err((*args[0].as_gnd::<&str>().unwrap()).into())
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match_by_equality(self, other)
        }
    }

    impl Display for ThrowError {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "throw-error")
        }
    }

    #[derive(PartialEq, Clone, Debug)]
    struct NonReducible();

    impl Grounded for NonReducible {
        fn type_(&self) -> Atom {
            expr!("->" "u32" "u32")
        }
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Err(ExecError::NoReduce)
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match_by_equality(self, other)
        }
    }

    impl Display for NonReducible {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "non-reducible")
        }
    }

    #[derive(PartialEq, Clone, Debug)]
    struct MulXUndefinedType(i32);

    impl Grounded for MulXUndefinedType {
        fn type_(&self) -> Atom {
            ATOM_TYPE_UNDEFINED
        }
        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![Atom::value(self.0 * args.get(0).unwrap().as_gnd::<i32>().unwrap())])
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match_by_equality(self, other)
        }
    }

    impl Display for MulXUndefinedType {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "x{}", self.0)
        }
    }

    #[derive(PartialEq, Clone, Debug)]
    struct ReturnNothing();

    impl Grounded for ReturnNothing {
        fn type_(&self) -> Atom {
            ATOM_TYPE_UNDEFINED
        }
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![])
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match_by_equality(self, other)
        }
    }

    impl Display for ReturnNothing {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "return-nothing")
        }
    }
}
