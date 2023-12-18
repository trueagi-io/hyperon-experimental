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
use std::collections::HashSet;

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

#[derive(Debug)]
struct InterpreterContext<'a, T: SpaceRef<'a>> {
    space: T,
    phantom: PhantomData<&'a GroundingSpace>,
}

impl<'a, T: SpaceRef<'a>> InterpreterContext<'a, T> {
    fn new(space: T) -> Self {
        Self{ space, phantom: PhantomData }
    }
}

#[derive(Debug)]
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
            let InterpretedAtom(atom, bindings) = atom;
            if atom != EMPTY_SYMBOL {
                let atom = apply_bindings_to_atom(&atom, &bindings);
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

use std::fmt::Write;

fn print_stack(atom: &InterpretedAtom) -> String {
    let InterpretedAtom(atom, bindings) = atom;
    let mut levels = Vec::new();
    print_level(&mut levels, atom, bindings);
    let mut buffer = String::new();
    let mut i = levels.len();
    writeln!(buffer, "=> {:05} {}", i, levels.pop().unwrap()).unwrap();
    while !levels.is_empty() {
        i = i - 1;
        writeln!(buffer, "   {:05} {}", i, levels.pop().unwrap()).unwrap();
    }
    buffer
}

fn print_level(levels: &mut Vec<String>, atom: &Atom, bindings: &Bindings) {
    levels.push(String::new());
    let output = levels.last_mut().unwrap();
    match atom {
        Atom::Expression(expr) => match expr.children().as_slice() {
            [op, args @ ..] if *op == FUNCTION_SYMBOL => {
                match args {
                    [Atom::Expression(_body)] => {
                        write!(output, "{}", atom).unwrap();
                    },
                    [body @ Atom::Expression(_body), call] => {
                        write!(output, "{}", call).unwrap();
                        print_level(levels, body, bindings);
                    },
                    _ => panic!(),
                }
            },
            [op, args @ ..] if *op == CHAIN_SYMBOL => {
                match args {
                    [nested, Atom::Variable(var), templ] => {
                        write!(output, "(chain <result> {} {})", var, templ).unwrap();
                        print_level(levels, nested, bindings);
                    },
                    _ => panic!(),
                }
            },
            [op, args @ ..] if *op == CHECK_ALTERNATIVES_SYMBOL => {
                match args {
                    [_atom] => {
                        write!(output, "{}", atom).unwrap();
                    },
                    [Atom::Expression(current), Atom::Expression(finished)] => {
                        let current_len = current.children().len();
                        if current_len > 0 {
                            let next = &current.children()[current_len - 1];
                            let (atom, bindings) = atom_as_interpreted_atom(next);
                            write!(output, "(check-alternatives {} {})", current, finished).unwrap();
                            print_level(levels, atom, bindings);
                        } else {
                            write!(output, "{}", atom).unwrap();
                        }
                    },
                    _ => panic!(),
                }
            },
            _ => {
                write!(output, "{}", atom).unwrap();
            },
        },
        _ => write!(output, "{}", atom).unwrap(),
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
    log::debug!("interpret_step: {}", interpreted_atom);
    log::debug!("stack:\n{}", print_stack(&interpreted_atom));
    for result in interpret_root_atom(&state.context, interpreted_atom) {
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
            || *op == DECONS_SYMBOL
            || *op == FUNCTION_SYMBOL
            || *op == CHECK_ALTERNATIVES_SYMBOL,
        _ => false,
    }
}

fn is_op(atom: &Atom, op: &Atom) -> bool {
    let expr = atom_as_slice(&atom);
    match expr {
        Some([opp, ..]) => opp == op,
        _ => false,
    }
}

fn is_function_op(atom: &Atom) -> bool {
    is_op(atom, &FUNCTION_SYMBOL)
}

fn is_eval_op(atom: &Atom) -> bool {
    is_op(atom, &EVAL_SYMBOL)
}

fn is_chain_op(atom: &Atom) -> bool {
    is_op(atom, &CHAIN_SYMBOL)
}

type Variables = HashSet<VariableAtom>;

fn interpret_root_atom<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, interpreted_atom: InterpretedAtom) -> Vec<InterpretedAtom> {
    let InterpretedAtom(atom, bindings) = interpreted_atom;
    let vars: Variables = atom.iter().filter_type::<&VariableAtom>().cloned().collect();
    let mut result = interpret_nested_atom(context, atom, bindings, &vars);
    result.iter_mut().for_each(|interpreted| {
        let InterpretedAtom(_atom, bindings) = interpreted;
        *bindings = bindings.narrow_vars(&vars);
    });
    result
}

fn interpret_nested_atom<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, atom: Atom, bindings: Bindings, vars: &Variables) -> Vec<InterpretedAtom> {
    let expr = atom_as_slice(&atom);
    let result = match expr {
        Some([op, args @ ..]) if *op == EVAL_SYMBOL => {
            match args {
                [_atom] => {
                    // atom is matched twice to not reconstruct atom in case of
                    // error (see error branch below), don't think it is a best
                    // way, but don't see a better solution
                    match atom_into_array(atom) {
                        Some([_, atom]) => eval(context, atom, bindings, vars),
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
                            chain(context, bindings, nested, var, templ, vars),
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
                [atom, pattern, then, else_] => unify(bindings, atom, pattern, then, else_, vars),
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
        Some([op, args @ ..]) if *op == FUNCTION_SYMBOL => {
            match args {
                [Atom::Expression(_body)] => {
                    match atom_into_array(atom) {
                        Some([_, body]) =>
                            function(context, bindings, body.clone(), body, vars),
                        _ => panic!("Unexpected state"),
                    }
                },
                [Atom::Expression(_body), _call] => {
                    match atom_into_array(atom) {
                        Some([_, body, call]) =>
                            function(context, bindings, body, call, vars),
                        _ => panic!("Unexpected state"),
                    }
                },
                _ => {
                    let error: String = format!("expected: ({} (: <body> Expression)), found: {}", FUNCTION_SYMBOL, atom);
                    vec![InterpretedAtom(error_atom(atom, error), bindings)]
                },
            }
        },
        Some([op, args @ ..]) if *op == CHECK_ALTERNATIVES_SYMBOL => {
            match args {
                [_atom] => {
                    match atom_into_array(atom) {
                        Some([_, atom]) => {
                            let current = vec![interpreted_atom_into_atom(InterpretedAtom(atom, bindings.clone()))];
                            check_alternatives(context, ExpressionAtom::new(current), ExpressionAtom::new(vec![]), vars)
                        },
                        _ => panic!("Unexpected state"),
                    }
                },
                [Atom::Expression(_current), Atom::Expression(_finished)] => {
                    match atom_into_array(atom) {
                        Some([_, Atom::Expression(current), Atom::Expression(finished)]) =>
                            check_alternatives(context, current, finished, vars),
                        _ => panic!("Unexpected state"),
                    }
                },
                _ => {
                    let error: String = format!("expected: ({} (: <current> Expression) [(: <finished> Expression)]), found: {}", CHECK_ALTERNATIVES_SYMBOL, atom);
                    vec![InterpretedAtom(error_atom(atom, error), bindings)]
                },
            }
        },
        _ => {
            vec![InterpretedAtom(return_atom(atom), bindings)]
        },
    };
    result
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

fn eval<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, atom: Atom, bindings: Bindings, vars: &Variables) -> Vec<InterpretedAtom> {
    match atom_as_slice(&atom) {
        Some([Atom::Grounded(op), args @ ..]) => {
            match op.execute(args) {
                Ok(results) => {
                    if results.is_empty() {
                        // TODO: This is an open question how to interpret empty results
                        // which are returned by grounded function. There is no
                        // case to return empty result for now. If alternative
                        // should be removed from the plan then Empty is a proper result.
                        // If grounded atom returns no value then unit should be returned.
                        // NotReducible or Exec::NoReduce can be returned to
                        // let a caller know that function is not defined on a
                        // passed input data. Thus we can interpreter empty result
                        // by any way we like.
                        vec![]
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
        _ if is_embedded_op(&atom) => {
            interpret_nested_atom(context, atom, bindings, vars)
        },
        _ => query(&context.space, atom, bindings, vars),
    }
}

fn query<'a, T: SpaceRef<'a>>(space: T, atom: Atom, bindings: Bindings, vars: &Variables) -> Vec<InterpretedAtom> {
    let var_x = VariableAtom::new("X").make_unique();
    let query = Atom::expr([EQUAL_SYMBOL, atom.clone(), Atom::Variable(var_x.clone())]);
    let results = space.query(&query);
    let atom_x = Atom::Variable(var_x);
    let results: Vec<InterpretedAtom> = {
        log::debug!("interpreter2::query: query: {}", query);
        log::debug!("interpreter2::query: results.len(): {} bindings.len(): {} results: {} bindings: {}",
            results.len(), bindings.len(), results, bindings);
        results.into_iter()
            .flat_map(|mut b| {
                let mut res = apply_bindings_to_atom(&atom_x, &b);
                if is_function_op(&res) {
                    match res {
                        Atom::Expression(ref mut expr) => {
                            expr.children_mut().push(atom.clone());
                        }
                        _ => {},
                    }
                }
                b.cleanup(vars);
                log::debug!("interpreter2::query: b: {}", b);
                b.merge_v2(&bindings).into_iter().filter_map(move |b| {
                    if b.has_loops() {
                        None
                    } else {
                        Some(InterpretedAtom(res.clone(), b))
                    }
                })
            })
            .collect()
    };
    if results.is_empty() {
        vec![InterpretedAtom(return_not_reducible(), bindings)]
    } else {
        results
    }
}

fn chain<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, bindings: Bindings, nested: Atom, var: VariableAtom, templ: Atom, vars: &Variables) -> Vec<InterpretedAtom> {
    fn apply(bindings: Bindings, nested: Atom, var: VariableAtom, templ: &Atom) -> InterpretedAtom {
        let b = Bindings::new().add_var_binding_v2(var, nested).unwrap();
        let result = apply_bindings_to_atom(templ, &b);
        InterpretedAtom(result, bindings)
    }

    let is_eval = is_eval_op(&nested);
    if is_function_op(&nested) {
      let mut result = interpret_nested_atom(context, nested, bindings, vars);
      if result.len() == 1 {
          let InterpretedAtom(r, b) = result.pop().unwrap();
          if is_function_op(&r) {
              vec![InterpretedAtom(Atom::expr([CHAIN_SYMBOL, r, Atom::Variable(var), templ]), b)]
          } else {
              vec![apply(b, r, var.clone(), &templ)]
          }
      } else {
          result.into_iter()
              .map(|InterpretedAtom(r, b)| {
                  if is_function_op(&r) {
                      InterpretedAtom(Atom::expr([CHAIN_SYMBOL, r, Atom::Variable(var.clone()), templ.clone()]), b)
                  } else {
                      apply(b, r, var.clone(), &templ)
                  }
              })
          .collect()
      }
    } else if is_embedded_op(&nested) {
        let result = interpret_nested_atom(context, nested.clone(), bindings, vars);
        let result = result.into_iter()
            .map(|InterpretedAtom(r, b)| {
                if is_eval && is_function_op(&r) {
                    InterpretedAtom(Atom::expr([CHAIN_SYMBOL, r, Atom::Variable(var.clone()), templ.clone()]), b)
                } else if is_chain_op(&r) {
                    InterpretedAtom(Atom::expr([CHAIN_SYMBOL, r, Atom::Variable(var.clone()), templ.clone()]), b)
                } else {
                    apply(b, r, var.clone(), &templ)
                }
            })
        .collect();
        result
    } else {
        vec![apply(bindings, nested, var, &templ)]
    }
}

fn function<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, bindings: Bindings, body: Atom, call: Atom, vars: &Variables) -> Vec<InterpretedAtom> {
    match atom_as_slice(&body) {
        Some([op, _result]) if *op == RETURN_SYMBOL => {
            if let Some([_, result]) = atom_into_array(body) {
                // FIXME: check return arguments size
                vec![InterpretedAtom(result, bindings)]
            } else {
                panic!("Unexpected state");
            }
        },
        _ if is_embedded_op(&body) => {
            let mut result = interpret_nested_atom(context, body, bindings, vars);
            if result.len() == 1 {
                let InterpretedAtom(r, b) = result.pop().unwrap();
                vec![InterpretedAtom(Atom::expr([FUNCTION_SYMBOL, r, call]), b)]
            } else {
                result.into_iter()
                    .map(|InterpretedAtom(r, b)| {
                        InterpretedAtom(Atom::expr([FUNCTION_SYMBOL, r, call.clone()]), b)
                    })
                .collect()
            }
        },
        _ => {
            let error = format!("function doesn't have return statement, last atom: {}", body);
            vec![InterpretedAtom(error_atom(call, error), bindings)]
        },
    }
}

fn atom_into_interpreted_atom(atom: Atom) -> InterpretedAtom {
    match atom {
        Atom::Expression(_) => match atom_into_array(atom) {
            Some([atom, bindings]) => {
                match bindings.as_gnd::<Bindings>() {
                    Some(bindings) => {
                        // TODO: cloning is ineffective, but it is not possible
                        // to convert grounded atom into internal value at the
                        // moment
                        InterpretedAtom(atom, bindings.clone())
                    },
                    _ => panic!("Unexpected state: second item cannot be converted to Bindings"),
                }
            }
            _ => panic!("Unexpected state: atom is not a pair"),
        },
        _ => panic!("Unexpected state: atom is not an expression"),
    }
}

fn atom_as_interpreted_atom(atom: &Atom) -> (&Atom, &Bindings) {
    match atom_as_slice(atom) {
        Some([atom, bindings]) => {
            match bindings.as_gnd::<Bindings>() {
                Some(bindings) => {
                    (atom, bindings)
                },
                _ => panic!("Unexpected state: second item cannot be converted to Bindings"),
            }
        }
        _ => panic!("Unexpected state: atom is not a pair"),
    }
}

fn interpreted_atom_into_atom(interpreted: InterpretedAtom) -> Atom {
    let InterpretedAtom(atom, bindings) = interpreted;
    Atom::expr([atom, Atom::value(bindings)])
}

fn check_alternatives<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, current: ExpressionAtom, finished: ExpressionAtom, vars: &Variables) -> Vec<InterpretedAtom> {
    let mut current = current.into_children();
    let mut finished = finished.into_children();
    if current.is_empty() {
        let mut error = Vec::new();
        let mut success = Vec::new();
        finished.into_iter()
            .map(atom_into_interpreted_atom)
            .for_each(|InterpretedAtom(atom, bindings)| {
                let is_error = atom_is_error(&atom);
                let interpreted = InterpretedAtom(Atom::expr([RETURN_SYMBOL, atom]), bindings);
                if is_error {
                    error.push(interpreted);
                } else {
                    success.push(interpreted);
                }
            });
        if success.is_empty() {
            error
        } else {
            success
        }
    } else {
        let next = current.pop().unwrap();
        let interpreted = atom_into_interpreted_atom(next);
        let InterpretedAtom(atom, bindings) = interpreted;
        if is_embedded_op(&atom) {
            interpret_nested_atom(context, atom, bindings, vars).into_iter()
                .map(interpreted_atom_into_atom)
                .for_each(|atom| current.push(atom));
        } else {
            finished.push(interpreted_atom_into_atom(InterpretedAtom(atom, bindings)));
        }
        vec![InterpretedAtom(Atom::expr([CHECK_ALTERNATIVES_SYMBOL, Atom::expr(current), Atom::expr(finished)]), Bindings::new())]
    }
}

fn unify(bindings: Bindings, atom: &Atom, pattern: &Atom, then: &Atom, else_: &Atom, vars: &Variables) -> Vec<InterpretedAtom> {
    // TODO: Should unify() be symmetrical or not. While it is symmetrical then
    // if variable is matched by variable then both variables have the same
    // priority. Thus interpreter can use any of them further. This sometimes
    // looks unexpected. For example see `metta_car` unit test where variable
    // from car's argument is replaced.
    let matches: Vec<Bindings> = match_atoms(atom, pattern).collect();
    if matches.is_empty() {
        let bindings = bindings.narrow_vars(vars);
        let result = apply_bindings_to_atom(else_, &bindings);
        vec![InterpretedAtom(result, bindings)]
    } else {
        matches.into_iter()
            .flat_map(|b| {
                let b = b.narrow_vars(vars);
                b.merge_v2(&bindings).into_iter().filter_map(move |b| {
                    if b.has_loops() {
                        None
                    } else {
                        let then = apply_bindings_to_atom(then, &b);
                        Some(InterpretedAtom(then, b))
                    }
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
    use crate::common::test_utils::{metta_atom, metta_space};

    #[test]
    fn interpret_atom_evaluate_incorrect_args() {
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(eval)")),
            vec![InterpretedAtom(expr!("Error" ("eval") "expected: (eval <atom>), found: (eval)"), bind!{})]);
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(eval a b)")),
            vec![InterpretedAtom(expr!("Error" ("eval" "a" "b") "expected: (eval <atom>), found: (eval a b)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_atom() {
        let result = call_interpret_atom(&space("(= a b)"), &metta_atom("(eval a)"));
        assert_eq!(result, vec![atom("b", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_atom_no_definition() {
        let result = call_interpret_atom(&space(""), &metta_atom("(eval a)"));
        assert_eq!(result, vec![atom("NotReducible", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_empty_expression() {
        let result = call_interpret_atom(&space(""), &metta_atom("(eval ())"));
        assert_eq!(result, vec![atom("NotReducible", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_value() {
        let result = call_interpret_atom(&space(""), &expr!("eval" {6}));
        assert_eq!(result, vec![atom("NotReducible", bind!{})]);
    }


    #[test]
    fn interpret_atom_evaluate_pure_expression() {
        let space = space("(= (foo $a B) $a)");
        let result = call_interpret_atom(&space, &metta_atom("(eval (foo A $b))"));
        assert_eq!(result, vec![atom("A", bind!{ b: expr!("B") })]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_non_determinism() {
        let space = space("
            (= color red)
            (= color green)
            (= color blue)
        ");
        let result = call_interpret_atom(&space, &metta_atom("(eval color)"));
        assert_eq_no_order!(result, vec![
            atom("red", bind!{}),
            atom("green", bind!{}),
            atom("blue", bind!{})
        ]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_no_definition() {
        let result = call_interpret_atom(&space(""), &metta_atom("(eval (foo A))"));
        assert_eq!(result, vec![atom("NotReducible", bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_variable_name_conflict() {
        let space = space("(= (foo ($W)) True)");
        let result = call_interpret_atom(&space, &metta_atom("(eval (foo $W))"));
        assert_eq!(result[0].0, sym!("True"));
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression() {
        let result = call_interpret_atom(&space(""), &expr!("eval" ({MulXUndefinedType(7)} {6})));
        assert_eq!(result, vec![InterpretedAtom(expr!({42}), bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_empty() {
        let result = call_interpret_atom(&space(""), &expr!("eval" ({ReturnNothing()} {6})));
        assert_eq!(result, vec![]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_noreduce() {
        let result = call_interpret_atom(&space(""), &expr!("eval" ({NonReducible()} {6})));
        assert_eq!(result, vec![InterpretedAtom(expr!("NotReducible"), bind!{})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_error() {
        let result = call_interpret_atom(&space(""), &expr!("eval" ({ThrowError()} {"Test error"})));
        assert_eq!(result, vec![InterpretedAtom(expr!("Error" ({ThrowError()} {"Test error"}) "Test error"), bind!{})]);
    }


    #[test]
    fn interpret_atom_chain_incorrect_args() {
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(chain n $v t o)")),
            vec![InterpretedAtom(expr!("Error" ("chain" "n" v "t" "o") "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v t o)"), bind!{})]);
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(chain n v t)")),
            vec![InterpretedAtom(expr!("Error" ("chain" "n" "v" "t") "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n v t)"), bind!{})]);
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(chain n $v)")),
            vec![InterpretedAtom(expr!("Error" ("chain" "n" v) "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_chain_atom() {
        let result = call_interpret_atom(&space(""), &expr!("chain" ("A" () {6} y) x ("bar" x)));
        assert_eq!(result, vec![InterpretedAtom(expr!("bar" ("A" () {6} y)), bind!{})]);
    }


    #[test]
    fn interpret_atom_chain_evaluation() {
        let space = space("(= (foo $a B) $a)");
        let result = call_interpret_atom(&space, &metta_atom("(chain (eval (foo A $b)) $x (bar $x))"));
        assert_eq!(result, vec![atom("(bar A)", bind!{ b: expr!("B") })]);
    }

    #[test]
    fn interpret_atom_chain_nested_evaluation() {
        let space = space("(= (foo $a B) $a)");
        let result = call_interpret_atom(&space, &metta_atom("(chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y))"));
        assert_eq!(result, vec![atom("(baz (bar A))", bind!{ b: expr!("B") })]);
    }

    #[test]
    fn interpret_atom_chain_nested_value() {
        let result = call_interpret_atom(&space(""), &metta_atom("(chain (chain A $x (bar $x)) $y (baz $y))"));
        assert_eq!(result, vec![atom("(baz (bar A))", bind!{})]);
    }

    #[test]
    fn interpret_atom_chain_expression_non_determinism() {
        let space = space("
            (= (color) red)
            (= (color) green)
            (= (color) blue)
        ");
        let result = call_interpret_atom(&space, &metta_atom("(chain (eval (color)) $x (bar $x))"));
        assert_eq_no_order!(result, vec![
            atom("(bar red)", bind!{}),
            atom("(bar green)", bind!{}),
            atom("(bar blue)", bind!{})
        ]);
    }

    #[test]
    fn interpret_atom_chain_return() {
        let result = call_interpret_atom(&space(""), &metta_atom("(chain Empty $x (bar $x))"));
        assert_eq!(result, vec![atom("(bar Empty)", bind!{})]);
    }


    #[test]
    fn interpret_atom_match_incorrect_args() {
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(unify a p t e o)")),
            vec![InterpretedAtom(expr!("Error" ("unify" "a" "p" "t" "e" "o") "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t e o)"), bind!{})]);
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(unify a p t)")),
            vec![InterpretedAtom(expr!("Error" ("unify" "a" "p" "t") "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_match_then() {
        let result = call_interpret_atom(&space(""), &metta_atom("(unify (A $b) ($a B) ($a $b) Empty)"));
        assert_eq!(result, vec![atom("(A B)", bind!{ a: expr!("A"), b: expr!("B") })]);
    }

    #[test]
    fn interpret_atom_match_else() {
        let result = call_interpret_atom(&space(""), &metta_atom("(unify (A $b C) ($a B D) ($a $b) Empty)"));
        assert_eq!(result, vec![atom("Empty", bind!{})]);
    }


    #[test]
    fn interpret_atom_decons_incorrect_args() {
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(decons a)")),
            vec![InterpretedAtom(expr!("Error" ("decons" "a") "expected: (decons (: <expr> Expression)), found: (decons a)"), bind!{})]);
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(decons (a) (b))")),
            vec![InterpretedAtom(expr!("Error" ("decons" ("a") ("b")) "expected: (decons (: <expr> Expression)), found: (decons (a) (b))"), bind!{})]);
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(decons)")),
            vec![InterpretedAtom(expr!("Error" ("decons") "expected: (decons (: <expr> Expression)), found: (decons)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_decons_empty() {
        let result = call_interpret_atom(&space(""), &metta_atom("(decons ())"));
        assert_eq!(result, vec![InterpretedAtom(expr!("Error" ("decons" ()) "expected: (decons (: <expr> Expression)), found: (decons ())"), bind!{})]);
    }

    #[test]
    fn interpret_atom_decons_single() {
        let result = call_interpret_atom(&space(""), &metta_atom("(decons (a))"));
        assert_eq!(result, vec![atom("(a ())", bind!{})]);
    }

    #[test]
    fn interpret_atom_decons_list() {
        let result = call_interpret_atom(&space(""), &metta_atom("(decons (a b c))"));
        assert_eq!(result, vec![atom("(a (b c))", bind!{})]);
    }


    #[test]
    fn interpret_atom_cons_incorrect_args() {
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(cons a (e) o)")),
            vec![InterpretedAtom(expr!("Error" ("cons" "a" ("e") "o") "expected: (cons <head> (: <tail> Expression)), found: (cons a (e) o)"), bind!{})]);
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(cons a e)")),
            vec![InterpretedAtom(expr!("Error" ("cons" "a" "e") "expected: (cons <head> (: <tail> Expression)), found: (cons a e)"), bind!{})]);
        assert_eq!(call_interpret_atom(&space(""), &metta_atom("(cons a)")),
            vec![InterpretedAtom(expr!("Error" ("cons" "a") "expected: (cons <head> (: <tail> Expression)), found: (cons a)"), bind!{})]);
    }

    #[test]
    fn interpret_atom_cons_empty() {
        let result = call_interpret_atom(&space(""), &metta_atom("(cons a ())"));
        assert_eq!(result, vec![atom("(a)", bind!{})]);
    }

    #[test]
    fn interpret_atom_cons_single() {
        let result = call_interpret_atom(&space(""), &metta_atom("(cons a (b))"));
        assert_eq!(result, vec![atom("(a b)", bind!{})]);
    }

    #[test]
    fn interpret_atom_cons_list() {
        let result = call_interpret_atom(&space(""), &metta_atom("(cons a (b c))"));
        assert_eq!(result, vec![atom("(a b c)", bind!{})]);
    }

    #[test]
    fn metta_turing_machine() {
        let space = space("
            (= (if-embedded-op $atom $then $else)
              (chain (decons $atom) $list
                (unify $list (cons $_) $then
                  (unify $list (decons $_) $then
                    (unify $list (chain $_) $then
                      (unify $list (eval $_) $then
                        (unify $list (unify $_) $then
                          $else )))))))

            (= (chain-loop $atom $var $templ)
              (chain $atom $x
                (eval (if-embedded-op $x
                  (eval (chain-loop $x $var $templ))
                  (chain $x $var $templ) ))))

            (= (tm $rule $state $tape)
              (unify $state HALT
                $tape
                (chain (eval (read $tape)) $char
                  (chain (eval ($rule $state $char)) $res
                    (unify $res ($next-state $next-char $dir)
                      (eval (chain-loop (eval (move $tape $next-char $dir)) $next-tape
                        (eval (tm $rule $next-state $next-tape)) ))
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

    fn call_interpret_atom<'a, T: SpaceRef<'a>>(space: T, atom: &Atom) -> Vec<InterpretedAtom> {
        let mut state = interpret_init(space, atom);
        let interpreted = state.pop().unwrap();
        interpret_root_atom(&state.context, interpreted)
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
