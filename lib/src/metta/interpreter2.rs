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

use std::ops::Deref;
use std::rc::Rc;
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

struct InterpreterContextRef<'a, T: SpaceRef<'a>>(Rc<InterpreterContext<'a, T>>);

impl<'a, T: SpaceRef<'a>> InterpreterContextRef<'a, T> {
    fn new(space: T) -> Self {
        Self(Rc::new(InterpreterContext{ space, phantom: PhantomData }))
    }
}

impl<'a, T: SpaceRef<'a>> Deref for InterpreterContextRef<'a, T> {
    type Target = InterpreterContext<'a, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T: SpaceRef<'a>> Clone for InterpreterContextRef<'a, T> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

pub struct InterpreterState<'a, T: SpaceRef<'a>> {
    plan: Vec<InterpretedAtom>,
    finished: Vec<Atom>,
    context: InterpreterContextRef<'a, T>,
}

fn atom_as_slice(atom: &Atom) -> Option<&[Atom]> {
    <&[Atom]>::try_from(atom).ok()
}

// FIXME: return incorrect length as error_atom() from caller
fn atom_into_array<const N: usize>(atom: Atom) -> Option<[Atom; N]> {
    <[Atom; N]>::try_from(atom).ok()
}

impl<'a, T: SpaceRef<'a>> InterpreterState<'a, T> {

    fn has_next(&self) -> bool {
        !self.plan.is_empty()
    }

    fn into_result(self) -> Result<Vec<Atom>, String> {
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
    let context = InterpreterContextRef::new(space);
    InterpreterState {
        plan: vec![InterpretedAtom(expr.clone(), Bindings::new())],
        finished: vec![],
        context
    }
}

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
            || *op == MATCH_SYMBOL
            || *op == CONS_SYMBOL
            || *op == DECONS_SYMBOL,
        _ => false,
    }
}

fn interpret_atom<'a, T: SpaceRef<'a>>(space: T, interpreted_atom: InterpretedAtom) -> Vec<InterpretedAtom> {
    let InterpretedAtom(atom, bindings) = interpreted_atom;
    let expr = atom_as_slice(&atom);
    match expr {
        Some([op, ..]) if *op == EVAL_SYMBOL => {
            let [_, atom]: [Atom; 2] = atom_into_array(atom.clone()).unwrap();
            evaluate_atom(space, atom, bindings)
        },
        Some([op, ..]) if *op == CHAIN_SYMBOL => {
            let [_, nested, var, templ]: [Atom; 4] = atom_into_array(atom.clone()).unwrap();
            if is_embedded_op(&nested) {
                let result = interpret_atom(space, InterpretedAtom(nested, bindings.clone()));
                result.into_iter()
                    .flat_map(|InterpretedAtom(r, b)| {
                        let var = var.clone();
                        let templ = templ.clone();
                        b.merge_v2(&bindings)
                            .into_iter()
                            .map(move |bindings| {
                                // TODO: we could eliminate bindings application here
                                // and below after pretty print for debug is ready,
                                // before that it is difficult to look at the plans
                                // with the variables and bindings separated.
                                let result = apply_bindings_to_atom(&r, &bindings);
                                InterpretedAtom(Atom::expr([CHAIN_SYMBOL, result, var.clone(), templ.clone()]), bindings)
                            })
                    })
                .collect()
            } else {
                let var = VariableAtom::try_from(var).unwrap();
                let b = Bindings::new().add_var_binding_v2(&var, nested).unwrap();
                let result = apply_bindings_to_atom(&templ, &b);
                vec![InterpretedAtom(result, bindings)]
            }
        },
        Some([op, ..]) if *op == MATCH_SYMBOL => {
            let [_, atom, pattern, then, else_]: [Atom; 5] = atom_into_array(atom).unwrap();
            let matches: Vec<Bindings> = match_atoms(&atom, &pattern).collect();
            if matches.is_empty() {
                let result = apply_bindings_to_atom(&else_, &bindings);
                vec![InterpretedAtom(result, bindings)]
            } else {
                matches.into_iter()
                    .flat_map(|b| {
                        b.merge_v2(&bindings).into_iter()
                            .map(|b| {
                                let then = apply_bindings_to_atom(&then, &b);
                                InterpretedAtom(then.clone(), b)
                            })
                    })
                .collect()
            }
        },
        Some([op, ..]) if *op == DECONS_SYMBOL => {
            // FIXME: add a macros to check the types of the arguments and return array of args.
            // If check is performed before deconstruction then cloning should not be needed.
            let [_, expr]: [Atom; 2] = atom_into_array(atom.clone()).unwrap();
            let result = match atom_as_slice(&expr) {
                Some([]) => InterpretedAtom(Atom::expr([]), bindings),
                Some([_, ..]) => {
                    let mut children = ExpressionAtom::try_from(expr).unwrap().into_children();
                    let head = children.remove(0);
                    let tail = children;
                    InterpretedAtom(Atom::expr([head, Atom::expr(tail)]), bindings)
                },
                None => {
                    InterpretedAtom(error_atom(atom, "decons expects an ExpressionAtom as an argument".into()), bindings)
                },
            };
            vec![result]
        },
        Some([op, ..]) if *op == CONS_SYMBOL => {
            let [_, head, tail]: [Atom; 3] = atom_into_array(atom.clone()).unwrap();
            let mut children = vec![head];
            children.extend(ExpressionAtom::try_from(tail).unwrap().into_children());
            vec![InterpretedAtom(Atom::expr(children), bindings)]
        },
        _ => {
            vec![InterpretedAtom(return_atom(atom), bindings)]
        },
    }
}

fn return_empty() -> Atom {
    EMPTY_SYMBOL
}

fn error_atom(atom: Atom, err: String) -> Atom {
    Atom::expr([Atom::sym("Error"), atom, Atom::sym(err)])
}

fn return_atom(atom: Atom) -> Atom {
    atom
}

fn evaluate_atom<'a, T: SpaceRef<'a>>(space: T, atom: Atom, bindings: Bindings) -> Vec<InterpretedAtom> {
    match atom_as_slice(&atom) {
        Some([Atom::Grounded(op), args @ ..]) => {
            match op.execute(args) {
                Ok(results) => {
                    if results.is_empty() {
                        vec![InterpretedAtom(return_empty(), bindings)]
                    } else {
                        results.into_iter()
                            .map(|atom| InterpretedAtom(atom, bindings.clone()))
                            .collect()
                    }
                },
                Err(ExecError::Runtime(err)) =>
                    vec![InterpretedAtom(error_atom(atom, err), bindings)],
                Err(ExecError::NoReduce) =>
                    vec![InterpretedAtom(return_atom(atom), bindings)],
            }
        },
        _ => query_atom(space, atom, bindings),
    }
}

fn query_atom<'a, T: SpaceRef<'a>>(space: T, atom: Atom, bindings: Bindings) -> Vec<InterpretedAtom> {
    let var = VariableAtom::new("X").make_unique();
    let query = Atom::expr([EQUAL_SYMBOL, atom, Atom::Variable(var.clone())]);
    let results = space.query(&query);
    if results.is_empty() {
        vec![InterpretedAtom(return_empty(), bindings)]
    } else {
        results.into_iter()
            .flat_map(|mut b| {
                let atom = b.resolve_and_remove(&var).unwrap();
                let bindings = b.merge_v2(&bindings);
                bindings.into_iter().map(move |b| (atom.clone(), b))
            })
        .map(|(atom, bindings)| {
            let atom = apply_bindings_to_atom(&atom, &bindings);
            InterpretedAtom(atom, bindings)
        })
        .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interpret_atom_evaluate_atom() {
        let result = interpret_atom(&space("(= a b)"), atom("(eval a)", bind!{}));
        assert_eq!(result, vec![atom("b", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_atom_no_definition() {
        let result = interpret_atom(&space(""), atom("(eval a)", bind!{}));
        assert_eq!(result, vec![atom("Empty", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_empty_expression() {
        let result = interpret_atom(&space(""), atom("(eval ())", bind!{}));
        assert_eq!(result, vec![atom("Empty", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_value() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!("eval" {6}), bind!{}));
        assert_eq!(result, vec![atom("Empty", bind!{})]);
    }


    #[test]
    fn interpret_atom_evaluate_pure_expression() {
        let space = space("(= (foo $a B) $a)");
        let result = interpret_atom(&space, atom("(eval (foo A $b))", bind!{}));
        assert_eq!(result, vec![atom("A", bind!{b: expr!("B")})]);
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
        assert_eq!(result, vec![atom("Empty", bind!{})]);
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
        assert_eq!(result, vec![atom("Empty", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_noreduce() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!({NonReducible()} {6}), bind!{}));
        assert_eq!(result, vec![InterpretedAtom(expr!({NonReducible()} {6}), bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_error() {
        let result = interpret_atom(&space(""), InterpretedAtom(expr!("eval" ({ThrowError()} {"Test error"})), bind!{}));
        assert_eq!(result, vec![InterpretedAtom(expr!("Error" ({ThrowError()} {"Test error"}) "Test error"), bind!{})]);
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
        assert_eq!(result, vec![atom("(chain A $x (bar $x))", bind!{b: expr!("B")})]);
    }

    #[test]
    fn interpret_atom_chain_nested_evaluation() {
        let space = space("(= (foo $a B) $a)");
        let result = interpret_atom(&space, atom("(chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y))", bind!{}));
        assert_eq!(result, vec![atom("(chain (chain A $x (bar $x)) $y (baz $y))", bind!{b: expr!("B")})]);
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
    fn interpret_atom_match_then() {
        let result = interpret_atom(&space(""), atom("(match (A $b) ($a B) ($a $b) Empty)", bind!{}));
        assert_eq!(result, vec![atom("(A B)", bind!{a: expr!("A"), b: expr!("B")})]);
    }

    #[test]
    fn interpret_atom_match_else() {
        let result = interpret_atom(&space(""), atom("(match (A $b C) ($a B D) ($a $b) Empty)", bind!{}));
        assert_eq!(result, vec![atom("Empty", bind!{})]);
    }


    #[test]
    fn interpret_atom_decons_empty() {
        let result = interpret_atom(&space(""), atom("(decons ())", bind!{}));
        assert_eq!(result, vec![atom("()", bind!{})]);
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
            Err(args[0].as_gnd::<&str>().unwrap().deref().into())
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
