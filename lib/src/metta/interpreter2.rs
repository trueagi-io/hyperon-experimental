//! MeTTa assembly language implementation.
//!
//! # Algorithm
//!
//! TODO: explain an algorithm

use crate::*;
use crate::atom::matcher::*;
use crate::space::*;
use crate::space::grounding::*;
use crate::metta::*;

use std::fmt::{Debug, Display, Formatter};
use std::convert::TryFrom;
use std::collections::HashSet;
use std::rc::Rc;
use std::marker::PhantomData;
use std::fmt::Write;
use std::cell::RefCell;

macro_rules! match_atom {
    ($atom:tt ~ $pattern:tt => $succ:tt , _ => $error:tt) => {
        match_atom!{ $atom ~ $pattern if true => $succ , _ => $error }
    };
    ($atom:tt ~ $pattern:tt if $cond:expr => $succ:tt , _ => $error:tt) => {
        match atom_as_slice(&$atom) {
            #[allow(unused_variables)]
            Some($pattern) if $cond => {
                match atom_into_array($atom) {
                    Some($pattern) => $succ,
                    _ => panic!("Unexpected state"),
                }
            }
            _ => $error,
        }
    };
}
type ReturnHandler = fn(Rc<RefCell<Stack>>, Atom, Bindings) -> Option<Stack>;

#[derive(Debug, Clone)]
struct Stack {
    // Internal mutability is required to implement collapse-bind. All alternatives
    // reference the same collapse-bind Stack instance. When some alternative
    // finishes it modifies the collapse-bind state adding the result to the
    // collapse-bind list of results.
    // TODO: Try representing Option via Stack::Bottom
    prev: Option<Rc<RefCell<Self>>>,
    atom: Atom,
    ret: ReturnHandler,
    // TODO: Could it be replaced by calling a return handler when setting the flag?
    finished: bool,
    vars: Variables,
}

fn no_handler(_stack: Rc<RefCell<Stack>>, _atom: Atom, _bindings: Bindings) -> Option<Stack> {
    panic!("Unexpected state");
}

impl Stack {
    fn from_prev_add_vars(prev: Option<Rc<RefCell<Self>>>, atom: Atom, ret: ReturnHandler) -> Self {
        // TODO: vars are introduced in specific locations of the atom thus
        // in theory it is possible to optimize vars search for eval, unify and chain
        let vars = Self::vars(&prev, &atom);
        Self{ prev, atom, ret, finished: false, vars }
    }

    fn from_prev_keep_vars(prev: Option<Rc<RefCell<Self>>>, atom: Atom, ret: ReturnHandler) -> Self {
        let vars = Self::vars_copy(&prev);
        Self{ prev, atom, ret, finished: false, vars }
    }

    fn finished(prev: Option<Rc<RefCell<Self>>>, atom: Atom) -> Self {
        let vars = Self::vars_copy(&prev);
        Self{ prev, atom, ret: no_handler, finished: true, vars }
    }

    fn len(&self) -> usize {
        self.fold(0, |len, _stack| len + 1)
    }

    // TODO: should it be replaced by Iterator implementation?
    fn fold<T, F: FnMut(T, &Stack) -> T>(&self, mut val: T, mut app: F) -> T {
        val = app(val, self);
        match &self.prev {
            None => val,
            Some(prev) => prev.borrow().fold(val, app),
        }
    }

    fn vars_copy(prev: &Option<Rc<RefCell<Self>>>) -> Variables {
        match prev {
            Some(prev) => prev.borrow().vars.clone(),
            None => Variables::new(),
        }
    }

    fn vars(prev: &Option<Rc<RefCell<Self>>>, atom: &Atom) -> Variables {
        // TODO: nested atoms are visited twice: first time when outer atom
        // is visited, next time when internal atom is visited.
        let vars: Variables = atom.iter().filter_type::<&VariableAtom>().cloned().collect();
        match (prev, vars.is_empty()) {
            (Some(prev), true) => prev.borrow().vars.clone(),
            (Some(prev), false) => prev.borrow().vars.clone().union(vars),
            (None, _) => vars,
        }
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        fn print_level(buffer: &mut String, level: usize, last: bool, stack: &Stack) -> std::fmt::Result {
            let prefix = if last { "=> " } else { "   " };
            let ret = if stack.finished { "return " } else { "" };
            write!(buffer, "{}{:05} {}{}\n", prefix, level, ret, stack.atom)
        }

        let buffer = &mut String::new();
        let last_level = self.len();
        let res = print_level(buffer, last_level, true, self);
        self.prev.as_ref().map_or(res, |prev| {
            prev.borrow().fold((res, last_level - 1), |(res, level), top| {
                (res.and_then(|_| print_level(buffer, level, false, top)), level - 1)
            }).0
        }).and_then(|_| write!(f, "{}", buffer))
    }
}

#[derive(Debug)]
struct InterpretedAtom(Stack, Bindings);

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
            write!(f, "{}\n{}", self.1, self.0)
        }
    }
}

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
    vars: HashSet<VariableAtom>,
}

fn atom_as_slice(atom: &Atom) -> Option<&[Atom]> {
    <&[Atom]>::try_from(atom).ok()
}

fn atom_as_slice_mut(atom: &mut Atom) -> Option<&mut [Atom]> {
    <&mut [Atom]>::try_from(atom).ok()
}

fn atom_into_array<const N: usize>(atom: Atom) -> Option<[Atom; N]> {
    <[Atom; N]>::try_from(atom).ok()
}

impl<'a, T: SpaceRef<'a>> InterpreterState<'a, T> {

    /// INTERNAL USE ONLY. Create an InterpreterState that is ready to yield results
    #[allow(dead_code)] //TODO: MINIMAL only silence the warning until interpreter2 replaces interpreter
    pub(crate) fn new_finished(space: T, results: Vec<Atom>) -> Self {
        Self {
            plan: vec![],
            finished: results,
            context: InterpreterContext::new(space),
            vars: HashSet::new(),
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
        if atom.0.prev.is_none() && atom.0.finished {
            let InterpretedAtom(stack, bindings) = atom;
            if stack.atom != EMPTY_SYMBOL {
                let bindings = bindings.convert_var_equalities_to_bindings(&self.vars);
                let atom = apply_bindings_to_atom(&stack.atom, &bindings);
                self.finished.push(atom);
            }
        } else {
            self.plan.push(atom);
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
        plan: vec![InterpretedAtom(atom_to_stack(expr.clone(), None), Bindings::new())],
        finished: vec![],
        context,
        vars: expr.iter().filter_type::<&VariableAtom>().cloned().collect(),
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
    log::debug!("interpret_step:\n{}", interpreted_atom);
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
            || *op == CONS_ATOM_SYMBOL
            || *op == DECONS_ATOM_SYMBOL
            || *op == FUNCTION_SYMBOL
            || *op == COLLAPSE_BIND_SYMBOL
            || *op == SUPERPOSE_BIND_SYMBOL,
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

#[derive(Debug, Clone)]
struct Variables(im::HashSet<VariableAtom>);

impl Variables {
    fn new() -> Self {
        Self(im::HashSet::new())
    }
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    fn union(self, other: Self) -> Self {
        Variables(self.0.union(other.0))
    }
}

impl FromIterator<VariableAtom> for Variables {
    fn from_iter<I: IntoIterator<Item=VariableAtom>>(iter: I) -> Self {
        Self(im::HashSet::from_iter(iter))
    }
}

impl VariableSet for Variables {
    type Iter<'a> = im::hashset::Iter<'a, atom::VariableAtom> where Self: 'a;

    fn contains(&self, var: &VariableAtom) -> bool {
        self.0.contains(var)
    }
    fn iter(&self) -> Self::Iter<'_> {
        self.0.iter()
    }
}

fn interpret_root_atom<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, interpreted_atom: InterpretedAtom) -> Vec<InterpretedAtom> {
    let InterpretedAtom(stack, bindings) = interpreted_atom;
    interpret_nested_atom(context, stack, bindings)
}

fn interpret_nested_atom<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, mut stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    if stack.finished {
        // first executed minimal operation returned error
        if stack.prev.is_none() {
            return vec![InterpretedAtom(stack, bindings)];
        }
        let Stack{ prev, atom, ret: _, finished: _, vars: _ } = stack;
        let prev = match prev {
            Some(prev) => prev,
            None => panic!("Unexpected state"),
        };
        let ret = prev.borrow().ret;
        // TODO: two copies of bindings here; we could try to include bindings
        // into Stack and eliminate copying
        ret(prev, atom, bindings.clone())
            .map_or(vec![], |stack| vec![InterpretedAtom(stack, bindings)])
    } else {
        let expr = atom_as_slice(&stack.atom);
        let result = match expr {
            Some([op, ..]) if *op == EVAL_SYMBOL => {
                eval(context, stack, bindings)
            },
            Some([op, ..]) if *op == CHAIN_SYMBOL => {
                chain(stack, bindings)
            },
            Some([op, ..]) if *op == FUNCTION_SYMBOL => {
                panic!("Unexpected state")
            },
            Some([op, ..]) if *op == COLLAPSE_BIND_SYMBOL => {
                collapse_bind(stack, bindings)
            },
            Some([op, ..]) if *op == UNIFY_SYMBOL => {
                unify(stack, bindings)
            },
            Some([op, ..]) if *op == DECONS_ATOM_SYMBOL => {
                decons_atom(stack, bindings)
            },
            Some([op, ..]) if *op == CONS_ATOM_SYMBOL => {
                cons_atom(stack, bindings)
            },
            Some([op, ..]) if *op == SUPERPOSE_BIND_SYMBOL => {
                superpose_bind(stack, bindings)
            },
            _ => {
                stack.finished = true;
                vec![InterpretedAtom(stack, bindings)]
            },
        };
        result
    }
}

fn return_not_reducible() -> Atom {
    NOT_REDUCIBLE_SYMBOL
}

fn error_atom(atom: Atom, err: String) -> Atom {
    Atom::expr([Atom::sym("Error"), atom, Atom::sym(err)])
}

fn finished_result(atom: Atom, bindings: Bindings, prev: Option<Rc<RefCell<Stack>>>) -> Vec<InterpretedAtom> {
    vec![InterpretedAtom(Stack::finished(prev, atom), bindings)]
}

fn eval<'a, T: SpaceRef<'a>>(context: &InterpreterContext<'a, T>, stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: eval, ret: _, finished: _, vars} = stack;
    let query_atom = match_atom!{
        eval ~ [_op, query] => query,
        _ => {
            let error = format!("expected: ({} <atom>), found: {}", EVAL_SYMBOL, eval);
            return finished_result(error_atom(eval, error), bindings, prev);
        }
    };
    log::debug!("eval: query_atom: {}", query_atom);
    match atom_as_slice(&query_atom) {
        Some([Atom::Grounded(op), args @ ..]) => {
            let exec_res = op.execute(args);
            log::debug!("eval: execution results: {:?}", exec_res);
            match exec_res {
                Ok(results) => {
                    if results.is_empty() {
                        // There is no valid reason to return empty result from
                        // the grounded function. If alternative should be removed
                        // from the plan then EMPTY_SYMBOL is a proper result.
                        // If grounded atom returns no value then UNIT_ATOM()
                        // should be returned. NotReducible or Exec::NoReduce
                        // can be returned to let a caller know that function
                        // is not defined on a passed input data. Thus we can
                        // interpreter empty result by any way we like.
                        finished_result(EMPTY_SYMBOL, bindings, prev)
                    } else {
                        results.into_iter()
                            .map(|atom| {
                                let stack = if is_function_op(&atom) {
                                    let call = Stack::from_prev_keep_vars(prev.clone(), query_atom.clone(), call_ret);
                                    atom_to_stack(atom, Some(Rc::new(RefCell::new(call))))
                                } else {
                                    Stack::finished(prev.clone(), atom)
                                };
                                InterpretedAtom(stack, bindings.clone())
                            })
                            .collect()
                    }
                },
                Err(ExecError::Runtime(err)) =>
                    finished_result(error_atom(query_atom, err), bindings, prev),
                Err(ExecError::NoReduce) =>
                    // TODO: we could remove ExecError::NoReduce and explicitly
                    // return NOT_REDUCIBLE_SYMBOL from the grounded function instead.
                    finished_result(return_not_reducible(), bindings, prev),
            }
        },
        _ if is_embedded_op(&query_atom) =>
            vec![InterpretedAtom(atom_to_stack(query_atom, prev), bindings)],
        _ => query(&context.space, prev, query_atom, bindings, &vars),
    }
}

fn query<'a, T: SpaceRef<'a>>(space: T, prev: Option<Rc<RefCell<Stack>>>, atom: Atom, bindings: Bindings, vars: &Variables) -> Vec<InterpretedAtom> {
    let var_x = VariableAtom::new("X").make_unique();
    let query = Atom::expr([EQUAL_SYMBOL, atom.clone(), Atom::Variable(var_x.clone())]);
    let results = space.query(&query);
    let atom_x = Atom::Variable(var_x);
    let results: Vec<InterpretedAtom> = {
        log::debug!("interpreter2::query: query: {}", query);
        log::debug!("interpreter2::query: results.len(): {}, bindings.len(): {}, results: {} bindings: {}",
            results.len(), bindings.len(), results, bindings);
        results.into_iter()
            .flat_map(|mut b| {
                let res = apply_bindings_to_atom(&atom_x, &b);
                let stack = if is_function_op(&res) {
                    let call = Stack::from_prev_keep_vars(prev.clone(), atom.clone(), call_ret);
                    atom_to_stack(res, Some(Rc::new(RefCell::new(call))))
                } else {
                    Stack::finished(prev.clone(), res)
                };
                b.retain(|v| vars.contains(v));
                log::debug!("interpreter2::query: b: {}", b);
                b.merge_v2(&bindings).into_iter().filter_map(move |b| {
                    if b.has_loops() {
                        None
                    } else {
                        Some(InterpretedAtom(stack.clone(), b))
                    }
                })
            })
            .collect()
    };
    if results.is_empty() {
        finished_result(return_not_reducible(), bindings, prev)
    } else {
        results
    }
}

fn atom_to_stack(atom: Atom, prev: Option<Rc<RefCell<Stack>>>) -> Stack {
    let expr = atom_as_slice(&atom);
    let result = match expr {
        Some([op, ..]) if *op == CHAIN_SYMBOL => {
            chain_to_stack(atom, prev)
        },
        Some([op, ..]) if *op == FUNCTION_SYMBOL => {
            function_to_stack(atom, prev)
        },
        Some([op, ..]) if *op == COLLAPSE_BIND_SYMBOL => {
            collapse_bind_to_stack(atom, prev)
        },
        Some([op, ..]) if *op == EVAL_SYMBOL
                       || *op == UNIFY_SYMBOL => {
            Stack::from_prev_add_vars(prev, atom, no_handler)
        },
        _ => {
            Stack::from_prev_keep_vars(prev, atom, no_handler)
        },
    };
    result
}

fn chain_to_stack(mut atom: Atom, prev: Option<Rc<RefCell<Stack>>>) -> Stack {
    let mut nested = Atom::sym("%Nested%");
    let nested_arg = match atom_as_slice_mut(&mut atom) {
        Some([_op, nested, Atom::Variable(_var), _templ]) => nested,
        _ => {
            let error: String = format!("expected: ({} <nested> (: <var> Variable) <templ>), found: {}", CHAIN_SYMBOL, atom);
            return Stack::finished(prev, error_atom(atom, error));
        },
    };
    std::mem::swap(nested_arg, &mut nested);
    let cur = Stack::from_prev_add_vars(prev, atom, chain_ret);
    atom_to_stack(nested, Some(Rc::new(RefCell::new(cur))))
}

fn chain_ret(stack: Rc<RefCell<Stack>>, atom: Atom, _bindings: Bindings) -> Option<Stack> {
    let mut stack = (*stack.borrow()).clone();
    let nested = atom;
    let Stack{ prev: _, atom: chain, ret: _, finished: _, vars: _} = &mut stack;
    let arg = match atom_as_slice_mut(chain) {
        Some([_op, nested, Atom::Variable(_var), _templ]) => nested,
        _ => panic!("Unexpected state"),
    };
    *arg = nested;
    Some(stack)
}

fn chain(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: chain, ret: _, finished: _, vars: _} = stack;
    let (nested, var, templ) = match_atom!{
        chain ~ [_op, nested, Atom::Variable(var), templ] => (nested, var, templ),
        _ => {
            panic!("Unexpected state")
        }
    };
    let b = Bindings::new().add_var_binding_v2(var, nested).unwrap();
    let result = apply_bindings_to_atom(&templ, &b);
    vec![InterpretedAtom(atom_to_stack(result, prev), bindings)]
}

fn function_to_stack(mut atom: Atom, prev: Option<Rc<RefCell<Stack>>>) -> Stack {
    let mut nested = Atom::sym("%Nested%");
    let nested_arg = match atom_as_slice_mut(&mut atom) {
        Some([_op, nested @ Atom::Expression(_)]) => nested,
        _ => {
            let error: String = format!("expected: ({} (: <body> Expression)), found: {}", FUNCTION_SYMBOL, atom);
            return Stack::finished(prev, error_atom(atom, error));
        },
    };
    std::mem::swap(nested_arg, &mut nested);
    let cur = Stack::from_prev_keep_vars(prev, atom, function_ret);
    atom_to_stack(nested, Some(Rc::new(RefCell::new(cur))))
}

fn call_ret(stack: Rc<RefCell<Stack>>, atom: Atom, _bindings: Bindings) -> Option<Stack> {
    let mut stack = (*stack.borrow()).clone();
    stack.atom = atom;
    stack.finished = true;
    Some(stack)
}

fn function_ret(stack: Rc<RefCell<Stack>>, atom: Atom, _bindings: Bindings) -> Option<Stack> {
    match_atom!{
        atom ~ [op, result] if *op == RETURN_SYMBOL => {
            let mut stack = (*stack.borrow()).clone();
            stack.atom = result;
            stack.finished = true;
            Some(stack)
        },
        _ => {
            Some(atom_to_stack(atom, Some(stack)))
        }
    }
}

fn collapse_bind_to_stack(mut atom: Atom, prev: Option<Rc<RefCell<Stack>>>) -> Stack {
    let mut nested = Atom::expr([]);
    let nested_arg = match atom_as_slice_mut(&mut atom) {
        Some([_op, nested @ Atom::Expression(_)]) => nested,
        _ => {
            let error: String = format!("expected: ({} (: <current> Expression)), found: {}", COLLAPSE_BIND_SYMBOL, atom);
            return Stack::finished(prev, error_atom(atom, error));
        },
    };
    std::mem::swap(nested_arg, &mut nested);
    let cur = Stack::from_prev_keep_vars(prev, atom, collapse_bind_ret);
    atom_to_stack(nested, Some(Rc::new(RefCell::new(cur))))
}

fn collapse_bind_ret(stack: Rc<RefCell<Stack>>, atom: Atom, bindings: Bindings) -> Option<Stack> {
    let nested = atom;
    {
        let stack = &mut *stack.borrow_mut();
        let Stack{ prev: _, atom: collapse, ret: _, finished: _, vars: _ } = stack;
        let finished = match atom_as_slice_mut(collapse) {
            Some([_op, Atom::Expression(finished)]) => finished,
            _ => panic!("Unexpected state"),
        };
        finished.children_mut().push(atom_bindings_into_atom(nested, bindings));
    }
    Rc::into_inner(stack).map(RefCell::into_inner)
}

fn atom_bindings_into_atom(atom: Atom, bindings: Bindings) -> Atom {
    Atom::expr([atom, Atom::value(bindings)])
}

fn collapse_bind(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: collapse, ret: _, finished: _, vars: _ } = stack;
    let result = match_atom!{
        collapse ~ [_op, finished @ Atom::Expression(_)] => finished,
        _ => {
            panic!("Unexpected state")
        }
    };
    vec![InterpretedAtom(atom_to_stack(result, prev), bindings)]
}

fn unify(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: unify, ret: _, finished: _, vars } = stack;
    let (atom, pattern, then, else_) = match atom_as_slice(&unify) {
        Some([_op, atom, pattern, then, else_]) => (atom, pattern, then, else_),
        _ => {
            let error: String = format!("expected: ({} <atom> <pattern> <then> <else>), found: {}", UNIFY_SYMBOL, unify);
            return finished_result(error_atom(unify, error), bindings, prev);
        },
    };

    // TODO: Should unify() be symmetrical or not. While it is symmetrical then
    // if variable is matched by variable then both variables have the same
    // priority. Thus interpreter can use any of them further. This sometimes
    // looks unexpected. For example see `metta_car` unit test where variable
    // from car's argument is replaced.
    let matches: Vec<Bindings> = match_atoms(atom, pattern).collect();
    if matches.is_empty() {
        let bindings = bindings.narrow_vars(&vars);
        let result = apply_bindings_to_atom(else_, &bindings);
        finished_result(result, bindings, prev)
    } else {
        matches.into_iter()
            .flat_map(move |b| {
                let b = b.narrow_vars(&vars);
                let prev = prev.clone();
                b.merge_v2(&bindings).into_iter().filter_map(move |b| {
                    if b.has_loops() {
                        None
                    } else {
                        let then = apply_bindings_to_atom(then, &b);
                        Some(InterpretedAtom(Stack::finished(prev.clone(), then), b))
                    }
                })
            })
        .collect()
    }
}

fn decons_atom(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: decons, ret: _, finished: _, vars: _ } = stack;
    let expr = match_atom!{
        decons ~ [_op, Atom::Expression(expr)] if expr.children().len() > 0 => expr,
        _ => {
            let error: String = format!("expected: ({} (: <expr> Expression)), found: {}", DECONS_ATOM_SYMBOL, decons);
            return finished_result(error_atom(decons, error), bindings, prev);
        }
    };
    let mut children = expr.into_children();
    let head = children.remove(0);
    let tail = children;
    finished_result(Atom::expr([head, Atom::expr(tail)]), bindings, prev)
}

fn cons_atom(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: cons, ret: _, finished: _, vars: _ } = stack;
    let (head, tail) = match_atom!{
        cons ~ [_op, head, Atom::Expression(tail)] => (head, tail),
        _ => {
            let error: String = format!("expected: ({} <head> (: <tail> Expression)), found: {}", CONS_ATOM_SYMBOL, cons);
            return finished_result(error_atom(cons, error), bindings, prev);
        }
    };
    let mut children = vec![head];
    children.extend(tail.into_children());
    finished_result(Atom::expr(children), bindings, prev)
}

fn atom_into_atom_bindings(pair: Atom) -> (Atom, Bindings) {
    match_atom!{
        pair ~ [atom, bindings] => {
            match bindings.as_gnd::<Bindings>() {
                Some(bindings) => {
                    // TODO: cloning is ineffective, but it is not possible
                    // to convert grounded atom into internal value at the
                    // moment
                    (atom, bindings.clone())
                },
                _ => panic!("Unexpected state: second item cannot be converted to Bindings"),
            }
        },
        _ => {
            panic!("(Atom Bindings) pair is expected, {} was received", pair)
        }
    }
}

fn superpose_bind(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: superpose, ret: _, finished: _, vars: _ } = stack;
    let collapsed = match_atom!{
        superpose ~ [_op, Atom::Expression(collapsed)] => collapsed,
        _ => {
            let error: String = format!("expected: ({} (: <collapsed> Expression)), found: {}", SUPERPOSE_BIND_SYMBOL, superpose);
            return finished_result(error_atom(superpose, error), bindings, prev);
        }
    };
    collapsed.into_children().into_iter()
        .map(atom_into_atom_bindings)
        .map(|(atom, bindings)| {
            InterpretedAtom(Stack::finished(prev.clone(), atom), bindings)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::test_utils::{metta_atom, metta_space};

    #[test]
    fn interpret_atom_evaluate_incorrect_args() {
        assert_eq!(call_interpret(&space(""), &metta_atom("(eval)")),
            vec![expr!("Error" ("eval") "expected: (eval <atom>), found: (eval)")]);
        assert_eq!(call_interpret(&space(""), &metta_atom("(eval a b)")),
            vec![expr!("Error" ("eval" "a" "b") "expected: (eval <atom>), found: (eval a b)")]);
    }

    #[test]
    fn interpret_atom_evaluate_atom() {
        let result = call_interpret(&space("(= a b)"), &metta_atom("(eval a)"));
        assert_eq!(result, vec![metta_atom("b")]);
    }

    #[test]
    fn interpret_atom_evaluate_atom_no_definition() {
        let result = call_interpret(&space(""), &metta_atom("(eval a)"));
        assert_eq!(result, vec![metta_atom("NotReducible")]);
    }

    #[test]
    fn interpret_atom_evaluate_empty_expression() {
        let result = call_interpret(&space(""), &metta_atom("(eval ())"));
        assert_eq!(result, vec![metta_atom("NotReducible")]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_value() {
        let result = call_interpret(&space(""), &expr!("eval" {6}));
        assert_eq!(result, vec![metta_atom("NotReducible")]);
    }


    #[test]
    fn interpret_atom_evaluate_pure_expression() {
        let space = space("(= (foo $a B) $a)");
        let result = call_interpret(&space, &metta_atom("(eval (foo A $b))"));
        assert_eq!(result, vec![metta_atom("A")]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_non_determinism() {
        let space = space("
            (= color red)
            (= color green)
            (= color blue)
        ");
        let result = call_interpret(&space, &metta_atom("(eval color)"));
        assert_eq_no_order!(result, vec![
            metta_atom("red"),
            metta_atom("green"),
            metta_atom("blue"),
        ]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_no_definition() {
        let result = call_interpret(&space(""), &metta_atom("(eval (foo A))"));
        assert_eq!(result, vec![metta_atom("NotReducible")]);
    }

    #[test]
    fn interpret_atom_evaluate_pure_expression_variable_name_conflict() {
        let space = space("(= (foo ($W)) True)");
        let result = call_interpret(&space, &metta_atom("(eval (foo $W))"));
        assert_eq!(result[0], sym!("True"));
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression() {
        let result = call_interpret(&space(""), &expr!("eval" ({MulXUndefinedType(7)} {6})));
        assert_eq!(result, vec![expr!({42})]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_empty() {
        let result = call_interpret(&space(""), &expr!("eval" ({ReturnNothing()} {6})));
        assert_eq!(result, vec![]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_noreduce() {
        let result = call_interpret(&space(""), &expr!("eval" ({NonReducible()} {6})));
        assert_eq!(result, vec![expr!("NotReducible")]);
    }

    #[test]
    fn interpret_atom_evaluate_grounded_expression_error() {
        let result = call_interpret(&space(""), &expr!("eval" ({ThrowError()} {"Test error"})));
        assert_eq!(result, vec![expr!("Error" ({ThrowError()} {"Test error"}) "Test error")]);
    }


    #[test]
    fn interpret_atom_chain_incorrect_args() {
        let _ = env_logger::builder().is_test(true).try_init();
        assert_eq!(call_interpret(&space(""), &metta_atom("(chain n $v t o)")),
            vec![expr!("Error" ("chain" "n" v "t" "o") "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v t o)")]);
        assert_eq!(call_interpret(&space(""), &metta_atom("(chain n v t)")),
            vec![expr!("Error" ("chain" "n" "v" "t") "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n v t)")]);
        assert_eq!(call_interpret(&space(""), &metta_atom("(chain n $v)")),
            vec![expr!("Error" ("chain" "n" v) "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v)")]);
    }

    #[test]
    fn interpret_atom_chain_atom() {
        let result = call_interpret(&space(""), &expr!("chain" ("A" () {6} y) x ("bar" x)));
        assert_eq!(result, vec![expr!("bar" ("A" () {6} y))]);
    }


    #[test]
    fn interpret_atom_chain_evaluation() {
        let space = space("(= (foo $a B) $a)");
        let result = call_interpret(&space, &metta_atom("(chain (eval (foo A $b)) $x (bar $x))"));
        assert_eq!(result, vec![metta_atom("(bar A)")]);
    }

    #[test]
    fn interpret_atom_chain_nested_evaluation() {
        let space = space("(= (foo $a B) $a)");
        let result = call_interpret(&space, &metta_atom("(chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y))"));
        assert_eq!(result, vec![metta_atom("(baz (bar A))")]);
    }

    #[test]
    fn interpret_atom_chain_nested_value() {
        let result = call_interpret(&space(""), &metta_atom("(chain (chain A $x (bar $x)) $y (baz $y))"));
        assert_eq!(result, vec![metta_atom("(baz (bar A))")]);
    }

    #[test]
    fn interpret_atom_chain_expression_non_determinism() {
        let space = space("
            (= (color) red)
            (= (color) green)
            (= (color) blue)
        ");
        let result = call_interpret(&space, &metta_atom("(chain (eval (color)) $x (bar $x))"));
        assert_eq_no_order!(result, vec![
            metta_atom("(bar red)"),
            metta_atom("(bar green)"),
            metta_atom("(bar blue))"),
        ]);
    }

    #[test]
    fn interpret_atom_chain_return() {
        let result = call_interpret(&space(""), &metta_atom("(chain Empty $x (bar $x))"));
        assert_eq!(result, vec![metta_atom("(bar Empty)")]);
    }


    #[test]
    fn interpret_atom_unify_incorrect_args() {
        assert_eq!(call_interpret(&space(""), &metta_atom("(unify a p t e o)")),
            vec![expr!("Error" ("unify" "a" "p" "t" "e" "o") "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t e o)")]);
        assert_eq!(call_interpret(&space(""), &metta_atom("(unify a p t)")),
            vec![expr!("Error" ("unify" "a" "p" "t") "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t)")]);
    }

    #[test]
    fn interpret_atom_unify_then() {
        let result = call_interpret(&space(""), &metta_atom("(unify (A $b) ($a B) ($a $b) Empty)"));
        assert_eq!(result, vec![metta_atom("(A B)")]);
    }

    #[test]
    fn interpret_atom_unify_else() {
        let result = call_interpret(&space(""), &metta_atom("(unify (A $b C) ($a B D) ($a $b) Empty)"));
        assert_eq!(result, vec![]);
    }


    #[test]
    fn interpret_atom_decons_atom_incorrect_args() {
        assert_eq!(call_interpret(&space(""), &metta_atom("(decons-atom a)")),
            vec![expr!("Error" ("decons-atom" "a") "expected: (decons-atom (: <expr> Expression)), found: (decons-atom a)")]);
        assert_eq!(call_interpret(&space(""), &metta_atom("(decons-atom (a) (b))")),
            vec![expr!("Error" ("decons-atom" ("a") ("b")) "expected: (decons-atom (: <expr> Expression)), found: (decons-atom (a) (b))")]);
        assert_eq!(call_interpret(&space(""), &metta_atom("(decons-atom)")),
            vec![expr!("Error" ("decons-atom") "expected: (decons-atom (: <expr> Expression)), found: (decons-atom)")]);
    }

    #[test]
    fn interpret_atom_decons_atom_empty() {
        let result = call_interpret(&space(""), &metta_atom("(decons-atom ())"));
        assert_eq!(result, vec![expr!("Error" ("decons-atom" ()) "expected: (decons-atom (: <expr> Expression)), found: (decons-atom ())")]);
    }

    #[test]
    fn interpret_atom_decons_atom_single() {
        let result = call_interpret(&space(""), &metta_atom("(decons-atom (a))"));
        assert_eq!(result, vec![metta_atom("(a ())")]);
    }

    #[test]
    fn interpret_atom_decons_atom_list() {
        let result = call_interpret(&space(""), &metta_atom("(decons-atom (a b c))"));
        assert_eq!(result, vec![metta_atom("(a (b c))")]);
    }


    #[test]
    fn interpret_atom_cons_atom_incorrect_args() {
        assert_eq!(call_interpret(&space(""), &metta_atom("(cons-atom a (e) o)")),
            vec![expr!("Error" ("cons-atom" "a" ("e") "o") "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a (e) o)")]);
        assert_eq!(call_interpret(&space(""), &metta_atom("(cons-atom a e)")),
            vec![expr!("Error" ("cons-atom" "a" "e") "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a e)")]);
        assert_eq!(call_interpret(&space(""), &metta_atom("(cons-atom a)")),
            vec![expr!("Error" ("cons-atom" "a") "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a)")]);
    }

    #[test]
    fn interpret_atom_cons_atom_empty() {
        let result = call_interpret(&space(""), &metta_atom("(cons-atom a ())"));
        assert_eq!(result, vec![metta_atom("(a)")]);
    }

    #[test]
    fn interpret_atom_cons_atom_single() {
        let result = call_interpret(&space(""), &metta_atom("(cons-atom a (b))"));
        assert_eq!(result, vec![metta_atom("(a b)")]);
    }

    #[test]
    fn interpret_atom_cons_atom_list() {
        let result = call_interpret(&space(""), &metta_atom("(cons-atom a (b c))"));
        assert_eq!(result, vec![metta_atom("(a b c)")]);
    }


    #[test]
    fn metta_turing_machine() {
        let space = space("
            (= (tm $rule $state $tape)
              (function (eval (tm-body $rule $state $tape))) )

            (= (tm-body $rule $state $tape)
              (unify $state HALT
                (return $tape)
                (chain (eval (read $tape)) $char
                  (chain (eval ($rule $state $char)) $res
                    (unify $res ($next-state $next-char $dir)
                      (chain (eval (move $tape $next-char $dir)) $next-tape
                        (eval (tm-body $rule $next-state $next-tape)) )
                      (return (Error (tm-body $rule $state $tape) \"Incorrect state\")) )))))

            (= (read ($head $hole $tail)) $hole)

            (= (move ($head $hole $tail) $char N) ($head $char $tail))
            (= (move ($head $hole $tail) $char L) (function
              (chain (cons-atom $char $head) $next-head
                (chain (decons-atom $tail) $list
                  (unify $list ($next-hole $next-tail)
                    (return ($next-head $next-hole $next-tail))
                    (return ($next-head 0 ())) )))))
            (= (move ($head $hole $tail) $char R) (function
              (chain (cons-atom $char $tail) $next-tail
                (chain (decons-atom $head) $list
                  (unify $list ($next-hole $next-head)
                    (return ($next-head $next-hole $next-tail))
                    (return (() 0 $next-tail)) )))))

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

    #[test]
    fn interpret_minimal_metta_smoketest() {
        let space = space("
            (= (foo $a B) $a)
            (= (fu $x) (function (chain (eval (foo $x B)) $r (return $r))))
            (= (color) red)
            (= (color) green)
            (= (color) blue)
        ");
        let result = interpret(&space, &metta_atom("(chain (chain A $x $x) $y $y)"));
        assert_eq!(result, Ok(vec![metta_atom("A")]));
        let result = interpret(&space, &metta_atom("(chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y))"));
        assert_eq!(result, Ok(vec![metta_atom("(baz (bar A))")]));
        let result = interpret(&space, &metta_atom("(chain (chain (eval (fu A)) $x (bar $x)) $y (baz $y))"));
        assert_eq!(result, Ok(vec![metta_atom("(baz (bar A))")]));
        let result = interpret(&space, &metta_atom("(unify (A $b) ($a B) ($a $b) Empty)"));
        assert_eq!(result, Ok(vec![metta_atom("(A B)")]));
        let result = interpret(&space, &metta_atom("(decons-atom (a b c))"));
        assert_eq!(result, Ok(vec![metta_atom("(a (b c))")]));
        let result = interpret(&space, &metta_atom("(cons-atom a (b c))"));
        assert_eq!(result, Ok(vec![metta_atom("(a b c)")]));
        let result = interpret(&space, &metta_atom("(chain (collapse-bind (eval (color))) $collapsed (superpose-bind $collapsed))")).unwrap();
        assert_eq_no_order!(result, vec![metta_atom("red"), metta_atom("green"), metta_atom("blue")]);
        let result = interpret(&space, &metta_atom("((P $a B) $a)"));
        assert_eq!(result, Ok(vec![metta_atom("((P $a B) $a)")]));
        let result = interpret(&space, &metta_atom("(collapse-bind (eval (color)))")).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq_no_order!(atom_as_slice(&result[0]).unwrap(), [
            atom_bindings_into_atom(expr!("red"), bind!{}),
            atom_bindings_into_atom(expr!("green"), bind!{}),
            atom_bindings_into_atom(expr!("blue"), bind!{})
        ]);
    }

    fn space(text: &str) -> GroundingSpace {
        metta_space(text)
    }

    fn call_interpret<'a, T: SpaceRef<'a>>(space: T, atom: &Atom) -> Vec<Atom> {
        let result = interpret(space, atom);
        assert!(result.is_ok());
        result.unwrap()
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
