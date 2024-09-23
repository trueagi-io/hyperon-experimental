//! MeTTa assembly language implementation. See
//! [minimal MeTTa documentation](https://github.com/trueagi-io/hyperon-experimental/blob/main/docs/minimal-metta.md) for details.

use crate::*;
use crate::atom::matcher::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::types::*;
use crate::metta::runner::stdlib_minimal::IfEqualOp;

use std::fmt::{Debug, Display, Formatter};
use std::convert::TryFrom;
use std::rc::Rc;
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

macro_rules! call_native {
    ($func:ident, $atom:expr) => {
        call_native_atom($func, stringify!($func), $atom)
    }
}

/// Operation return handler, it is triggered when nested operation is finished
/// and returns its results. First argument gets the reference to the stack
/// which on the top has the frame of the wrapping operation. Last two
/// arguments are the result of the nested operation. Handler returns
/// None when it is not ready to provide new stack (it can happen in case of
/// collapse-bind operation) or new stack with variable bindings to continue
/// execution of the program.
type ReturnHandler = fn(Rc<RefCell<Stack>>, Atom, Bindings) -> Option<(Stack, Bindings)>;

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
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

fn no_handler(_stack: Rc<RefCell<Stack>>, _atom: Atom, _bindings: Bindings) -> Option<(Stack, Bindings)> {
    panic!("Unexpected state");
}

impl Stack {
    fn from_prev_with_vars(prev: Option<Rc<RefCell<Self>>>, atom: Atom, vars: Variables, ret: ReturnHandler) -> Self {
        Self{ prev, atom, ret, finished: false, vars }
    }

    fn from_prev_keep_vars(prev: Option<Rc<RefCell<Self>>>, atom: Atom, ret: ReturnHandler) -> Self {
        let vars = Self::vars_copy(&prev);
        Self{ prev, atom, ret, finished: false, vars }
    }

    fn finished(prev: Option<Rc<RefCell<Self>>>, atom: Atom) -> Self {
        Self{ prev, atom, ret: no_handler, finished: true, vars: Variables::new() }
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

    fn add_vars_it<'a, I: 'a + Iterator<Item=&'a VariableAtom>>(prev: &Option<Rc<RefCell<Self>>>, vars: I) -> Variables {
        match prev {
            Some(prev) => prev.borrow().vars.clone().insert_all(vars),
            None => vars.cloned().collect(),
        }
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        fn print_level(buffer: &mut String, level: usize, last: bool, stack: &Stack) -> std::fmt::Result {
            let prefix = if last { "=> " } else { "   " };
            let ret = if stack.finished { "return " } else { "" };
            write!(buffer, "{}{:05} {}{} {}\n", prefix, level, ret, stack.atom, stack.vars)
        }

        let buffer = &mut String::new();
        let last_level = self.len();
        let res = print_level(buffer, last_level, true, self);
        self.prev.as_ref().map_or(res, |prev| {
            prev.borrow().fold((res, last_level - 1), |(res, level), top| {
                (res.and_then(|_| print_level(buffer, level, false, top)), level - 1)
            }).0
        })
        .and_then(|_| write!(f, "{}", buffer))
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
struct InterpretedAtom(Stack, Bindings);

impl Display for InterpretedAtom {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.1.is_empty() {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{}\n{}", self.1, self.0)
        }
    }
}

#[derive(Debug)]
struct InterpreterContext<T: Space> {
    space: T,
}

impl<T: Space> InterpreterContext<T> {
    fn new(space: T) -> Self {
        Self{ space }
    }
}

/// This wrapper is to keep interpreter interface compatible with previous
/// implementation and will be removed in future.
// TODO: MINIMAL: This wrapper is for compatibility with old_interpreter.rs only
pub trait SpaceRef<'a> : Space + 'a {}
impl<'a, T: Space + 'a> SpaceRef<'a> for T {}

/// State of the interpreter which passed between `interpret_step` calls.
#[derive(Debug)]
pub struct InterpreterState<'a, T: SpaceRef<'a>> {
    /// List of the alternatives to evaluate further.
    plan: Vec<InterpretedAtom>,
    /// List of the completely evaluated results to be returned.
    finished: Vec<Atom>,
    /// Evaluation context.
    context: InterpreterContext<T>,
    phantom: std::marker::PhantomData<dyn SpaceRef<'a>>,
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
    #[allow(dead_code)] //TODO: MINIMAL only silence the warning until interpreter_minimal replaces interpreter
    pub(crate) fn new_finished(space: T, results: Vec<Atom>) -> Self {
        Self {
            plan: vec![],
            finished: results,
            context: InterpreterContext::new(space),
            phantom: std::marker::PhantomData,
        }
    }

    /// Returns true if there are alternatives which can be evaluated further.
    pub fn has_next(&self) -> bool {
        !self.plan.is_empty()
    }

    /// Returns vector of fully evaluated results or error if there are still
    /// alternatives to be evaluated.
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
                let atom = apply_bindings_to_atom_move(stack.atom, &bindings);
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

/// Initialize interpreter and returns the starting interpreter state.
/// See [crate::metta::interpreter_minimal] for algorithm explanation.
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
        phantom: std::marker::PhantomData,
    }
}

/// Perform next step of the interpretation return the resulting interpreter
/// state. See [crate::metta::interpreter_minimal] for algorithm explanation.
///
/// # Arguments
/// * `state` - interpreter state from the previous step.
pub fn interpret_step<'a, T: Space + 'a>(mut state: InterpreterState<'a, T>) -> InterpreterState<'a, T> {
    let interpreted_atom = state.pop().unwrap();
    log::debug!("interpret_step:\n{}", interpreted_atom);
    let InterpretedAtom(stack, bindings) = interpreted_atom;
    for result in interpret_stack(&state.context, stack, bindings) {
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
            || *op == SUPERPOSE_BIND_SYMBOL
            || *op == METTA_SYMBOL
            || *op == CALL_NATIVE_SYMBOL,
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
#[cfg_attr(test, derive(PartialEq))]
struct Variables(im::HashSet<VariableAtom>);

impl Variables {
    fn new() -> Self {
        Self(im::HashSet::new())
    }
    fn insert(&mut self, var: VariableAtom) -> Option<VariableAtom> {
        self.0.insert(var)
    }
    fn insert_all<'a, I: 'a + Iterator<Item=&'a VariableAtom>>(mut self, it: I) -> Self {
        it.for_each(|var| { self.insert(var.clone()); });
        self
    }
}
fn vars_from_atom(atom: &Atom) -> impl Iterator<Item=&VariableAtom> {
    atom.iter().filter_type::<&VariableAtom>()
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

impl Display for Variables {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")
            .and_then(|_| self.iter().take(1).fold(Ok(()),
                |res, atom| res.and_then(|_| write!(f, "{}", atom))))
            .and_then(|_| self.iter().skip(1).fold(Ok(()),
                |res, atom| res.and_then(|_| write!(f, " {}", atom))))
            .and_then(|_| write!(f, "]"))
    }
}

fn interpret_stack<'a, T: Space>(context: &InterpreterContext<T>, stack: Stack, mut bindings: Bindings) -> Vec<InterpretedAtom> {
    if stack.finished {
        // first executed minimal operation returned error
        if stack.prev.is_none() {
            return vec![InterpretedAtom(stack, bindings)];
        }
        let Stack{ prev, mut atom, ret: _, finished: _, vars: _ } = stack;
        let prev = match prev {
            Some(prev) => prev,
            None => panic!("Unexpected state"),
        };
        {
            let outer_vars = &prev.borrow().vars;
            bindings.apply_and_retain(&mut atom, |v| outer_vars.contains(v));
        }
        let ret = prev.borrow().ret;
        ret(prev, atom, bindings)
            .map_or(vec![], |(stack, bindings)| vec![InterpretedAtom(stack, bindings)])
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
            Some([op, ..]) if *op == METTA_SYMBOL => {
                metta_sym(stack, bindings)
            },
            Some([op, ..]) if *op == CALL_NATIVE_SYMBOL => {
                call_native_symbol(stack, bindings)
            },
            _ => {
                let stack = Stack::finished(stack.prev, stack.atom);
                vec![InterpretedAtom(stack, bindings)]
            },
        };
        result
    }
}

fn return_not_reducible() -> Atom {
    NOT_REDUCIBLE_SYMBOL
}

fn error_msg(atom: Atom, err: String) -> Atom {
    error_atom(atom, Atom::sym(err))
}

fn error_atom(atom: Atom, err: Atom) -> Atom {
    Atom::expr([ERROR_SYMBOL, atom, err])
}

fn finished_result(atom: Atom, bindings: Bindings, prev: Option<Rc<RefCell<Stack>>>) -> Vec<InterpretedAtom> {
    vec![InterpretedAtom(Stack::finished(prev, atom), bindings)]
}

fn eval<'a, T: Space>(context: &InterpreterContext<T>, stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: eval, ret: _, finished: _, vars } = stack;
    let to_eval = match_atom!{
        eval ~ [_op, to_eval] => to_eval,
        _ => {
            let error = format!("expected: ({} <atom>), found: {}", EVAL_SYMBOL, eval);
            return finished_result(error_msg(eval, error), bindings, prev);
        }
    };
    let to_eval = apply_bindings_to_atom_move(to_eval, &bindings);
    log::debug!("eval: to_eval: {}", to_eval);
    match atom_as_slice(&to_eval) {
        Some([Atom::Grounded(op), args @ ..]) => {
            match op.as_grounded().as_execute() {
                None => finished_result(return_not_reducible(), bindings, prev),
                Some(executable) => {
                    let exec_res = executable.execute(args);
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
                                let call_stack = call_to_stack(to_eval, vars, prev.clone());
                                results.into_iter()
                                    .map(|res| eval_result(prev.clone(), res, &call_stack, bindings.clone()))
                                    .collect()
                            }
                        },
                        Err(ExecError::Runtime(err)) =>
                            finished_result(error_msg(to_eval, err), bindings, prev),
                        Err(ExecError::NoReduce) =>
                            // TODO: we could remove ExecError::NoReduce and explicitly
                            // return NOT_REDUCIBLE_SYMBOL from the grounded function instead.
                            finished_result(return_not_reducible(), bindings, prev),
                    }
                },
            }
        },
        _ if is_embedded_op(&to_eval) =>
            vec![InterpretedAtom(atom_to_stack(to_eval, prev), bindings)],
        _ => query(&context.space, prev, to_eval, bindings, vars),
    }
}

fn eval_result(prev: Option<Rc<RefCell<Stack>>>, res: Atom, call_stack: &Rc<RefCell<Stack>>, mut bindings: Bindings) -> InterpretedAtom {
    let stack = if is_function_op(&res) {
        let mut stack = function_to_stack(res, Some(call_stack.clone()));
        let call_stack = call_stack.borrow();
        // Apply arguments bindings is required to replace formal argument
        // variables by matched actual argument variables. Otherwise the
        // equality of formal and actual argument variables will be cleaned up
        // on any return from the nested function call.
        // TODO: we could instead add formal argument variables of the called
        // functions into stack.vars collection. One way of doing it is getting
        // list of formal var parameters from the function definition atom
        // but we don't have it returned from the query. Another way is to
        // find all variables equalities in bindings with variables
        // from call_stack.vars.
        bindings.apply_and_retain(&mut stack.atom, |v| call_stack.vars.contains(v));
        stack
    } else {
        Stack::finished(prev, res)
    };
    InterpretedAtom(stack, bindings)
}

fn call_to_stack(call: Atom, vars: Variables, prev: Option<Rc<RefCell<Stack>>>) -> Rc<RefCell<Stack>> {
    let vars = vars.insert_all(vars_from_atom(&call));
    let stack = Stack::from_prev_with_vars(prev, call, vars, call_ret);
    Rc::new(RefCell::new(stack))
}

#[cfg(not(feature = "variable_operation"))]
fn is_variable_op(atom: &Atom) -> bool {
    match atom {
        Atom::Expression(expr) => is_variable_op_expr(expr),
        _ => false,
    }
}

#[cfg(not(feature = "variable_operation"))]
fn is_variable_op_expr(expr: &ExpressionAtom) -> bool {
    match expr.children().get(0) {
        Some(Atom::Variable(_)) => true,
        Some(Atom::Expression(expr)) => is_variable_op_expr(expr),
        _ => false,
    }
}

fn query<'a, T: Space>(space: T, prev: Option<Rc<RefCell<Stack>>>, to_eval: Atom, bindings: Bindings, vars: Variables) -> Vec<InterpretedAtom> {
    #[cfg(not(feature = "variable_operation"))]
    if is_variable_op(&to_eval) {
        // TODO: This is a hotfix. Better way of doing this is adding
        // a function which modifies minimal MeTTa interpreter code
        // in order to skip such evaluations in metta-call function.
        return finished_result(return_not_reducible(), bindings, prev)
    }
    let var_x = &VariableAtom::new("X").make_unique();
    let query = Atom::expr([EQUAL_SYMBOL, to_eval.clone(), Atom::Variable(var_x.clone())]);
    let results = space.query(&query);
    log::debug!("interpreter_minimal::query: query: {}", query);
    log::debug!("interpreter_minimal::query: results.len(): {}, bindings.len(): {}, results: {} bindings: {}",
        results.len(), bindings.len(), results, bindings);
    let call_stack = call_to_stack(to_eval, vars, prev.clone());
    let result = |res, bindings| eval_result(prev.clone(), res, &call_stack, bindings);
    let results: Vec<InterpretedAtom> = results.into_iter().flat_map(|b| {
        log::debug!("interpreter_minimal::query: b: {}", b);
        b.merge_v2(&bindings).into_iter()
    }).filter_map(move |b| {
        b.resolve(&var_x).map_or(None, |res| {
            if b.has_loops() {
                None
            } else {
                Some(result(res, b))
            }
        })
    })
    .collect();
    if results.is_empty() {
        finished_result(return_not_reducible(), bindings, prev)
    } else {
        results
    }
}

fn atom_to_stack(atom: Atom, prev: Option<Rc<RefCell<Stack>>>) -> Stack {
    let expr = atom_as_slice(&atom);
    let result = match expr {
        Some([op, ..]) if *op == CHAIN_SYMBOL =>
            chain_to_stack(atom, prev),
        Some([op, ..]) if *op == FUNCTION_SYMBOL =>
            function_to_stack(atom, prev),
        Some([op, ..]) if *op == EVAL_SYMBOL =>
            Stack::from_prev_keep_vars(prev, atom, no_handler),
        Some([op, ..]) if *op == UNIFY_SYMBOL =>
            unify_to_stack(atom, prev),
        _ =>
            Stack::from_prev_keep_vars(prev, atom, no_handler),
    };
    result
}

fn chain_to_stack(mut atom: Atom, prev: Option<Rc<RefCell<Stack>>>) -> Stack {
    let mut nested = Atom::sym("%Nested%");
    let (nested_arg, templ_arg) = match atom_as_slice_mut(&mut atom) {
        Some([_op, nested, Atom::Variable(_var), templ]) => (nested, templ),
        _ => {
            let error: String = format!("expected: ({} <nested> (: <var> Variable) <templ>), found: {}", CHAIN_SYMBOL, atom);
            return Stack::finished(prev, error_msg(atom, error));
        },
    };
    std::mem::swap(nested_arg, &mut nested);
    let nested_vars: im::HashSet<&VariableAtom> = vars_from_atom(&nested).collect();
    let templ_vars: im::HashSet<&VariableAtom> = vars_from_atom(templ_arg).collect();
    let both_vars = nested_vars.intersection(templ_vars).into_iter();
    let vars = Stack::add_vars_it(&prev, both_vars);
    let cur = Stack::from_prev_with_vars(prev, atom, vars, chain_ret);
    atom_to_stack(nested, Some(Rc::new(RefCell::new(cur))))
}

fn chain_ret(stack: Rc<RefCell<Stack>>, atom: Atom, bindings: Bindings) -> Option<(Stack, Bindings)> {
    let mut stack = (*stack.borrow()).clone();
    let nested = atom;
    let Stack{ prev: _, atom: chain, ret: _, finished: _, vars: _} = &mut stack;
    let arg = match atom_as_slice_mut(chain) {
        Some([_op, nested, Atom::Variable(_var), _templ]) => nested,
        _ => panic!("Unexpected state"),
    };
    *arg = nested;
    Some((stack, bindings))
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
    let templ = apply_bindings_to_atom_move(templ, &b);
    vec![InterpretedAtom(atom_to_stack(templ, prev), bindings)]
}

fn function_to_stack(mut atom: Atom, prev: Option<Rc<RefCell<Stack>>>) -> Stack {
    let mut nested = Atom::sym("%Nested%");
    let nested_arg = match atom_as_slice_mut(&mut atom) {
        Some([_op, nested @ Atom::Expression(_)]) => nested,
        _ => {
            let error: String = format!("expected: ({} (: <body> Expression)), found: {}", FUNCTION_SYMBOL, atom);
            return Stack::finished(prev, error_msg(atom, error));
        },
    };
    std::mem::swap(nested_arg, &mut nested);
    let cur = Stack::from_prev_keep_vars(prev, atom, function_ret);
    atom_to_stack(nested, Some(Rc::new(RefCell::new(cur))))
}

fn call_ret(stack: Rc<RefCell<Stack>>, atom: Atom, bindings: Bindings) -> Option<(Stack, Bindings)> {
    let stack = Stack::finished(stack.borrow().prev.clone(), atom);
    Some((stack, bindings))
}

fn function_ret(stack: Rc<RefCell<Stack>>, atom: Atom, bindings: Bindings) -> Option<(Stack, Bindings)> {
    match_atom!{
        atom ~ [op, result] if *op == RETURN_SYMBOL => {
            let stack = Stack::finished(stack.borrow().prev.clone(), result);
            Some((stack, bindings))
        },
        _ => {
            Some((atom_to_stack(atom, Some(stack)), bindings))
        }
    }
}

fn collapse_bind(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: mut collapse, ret: _, finished: _, vars } = stack;

    let mut nested = Atom::expr([]);
    match &mut collapse {
        Atom::Expression(expr) => {
            std::mem::swap(&mut nested, &mut expr.children_mut()[1]);
            expr.children_mut().push(Atom::value(bindings.clone()))
        },
        _ => panic!("Unexpected state"),
    }

    let prev = Stack::from_prev_with_vars(prev, collapse, vars, collapse_bind_ret);
    let prev = Rc::new(RefCell::new(prev));
    let cur = atom_to_stack(nested, Some(prev.clone()));
    let dummy = Stack::finished(Some(prev), EMPTY_SYMBOL);
    vec![InterpretedAtom(dummy, bindings.clone()), InterpretedAtom(cur, bindings)]
}

fn collapse_bind_ret(stack: Rc<RefCell<Stack>>, atom: Atom, bindings: Bindings) -> Option<(Stack, Bindings)> {
    let nested = atom;
    {
        let stack_ref = &mut *stack.borrow_mut();
        let Stack{ prev: _, atom: collapse, ret: _, finished: _, vars: _ } = stack_ref;
        let finished = match atom_as_slice_mut(collapse) {
            Some([_op, Atom::Expression(finished), _bindings]) => finished,
            _ => panic!("Unexpected state"),
        };
        if nested != EMPTY_SYMBOL {
            finished.children_mut().push(atom_bindings_into_atom(nested, bindings));
        }
    }

    // all alternatives are evaluated
    match Rc::into_inner(stack).map(RefCell::into_inner) {
        Some(stack) => {
            let Stack{ prev, atom: collapse, ret: _, finished: _, vars: _ } = stack;
            let (result, bindings) = match atom_into_array(collapse) {
                Some([_op, result, bindings]) => (result, atom_into_bindings(bindings)),
                None => panic!("Unexpected state"),
            };
            Some((Stack::finished(prev, result), bindings))
        },
        None => None,
    }
}

fn atom_bindings_into_atom(atom: Atom, bindings: Bindings) -> Atom {
    Atom::expr([atom, Atom::value(bindings)])
}

fn unify_to_stack(mut atom: Atom, prev: Option<Rc<RefCell<Stack>>>) -> Stack {
    let () = match atom_as_slice_mut(&mut atom) {
        Some([_op, _a, _b, _then, _else]) => (),
        _ => {
            let error: String = format!("expected: ({} <atom> <pattern> <then> <else>), found: {}", UNIFY_SYMBOL, atom);
            return Stack::finished(prev, error_msg(atom, error));
        },
    };
    Stack::from_prev_with_vars(prev, atom, Variables::new(), no_handler)
}

fn unify(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: unify, ret: _, finished: _, vars: _ } = stack;
    let (atom, pattern, then, else_) = match_atom!{
        unify ~ [_op, atom, pattern, then, else_] => (atom, pattern, then, else_),
        _ => {
            let error: String = format!("expected: ({} <atom> <pattern> <then> <else>), found: {}", UNIFY_SYMBOL, unify);
            return finished_result(error_msg(unify, error), bindings, prev);
        }
    };

    let matches: Vec<Bindings> = match_atoms(&atom, &pattern).collect();
    let result = |bindings| {
        let stack = Stack::finished(prev.clone(), then.clone());
        InterpretedAtom(stack, bindings)
    };
    let bindings_ref = &bindings;
    let matches: Vec<InterpretedAtom> = matches.into_iter().flat_map(move |b| {
        b.merge_v2(bindings_ref).into_iter().filter_map(move |b| {
            if b.has_loops() {
                None
            } else {
                Some(result(b))
            }
        })
    })
    .collect();
    if matches.is_empty() {
        finished_result(else_, bindings, prev)
    } else {
        matches
    }
}

fn decons_atom(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: decons, ret: _, finished: _, vars: _ } = stack;
    let expr = match_atom!{
        decons ~ [_op, Atom::Expression(expr)] if expr.children().len() > 0 => expr,
        _ => {
            let error: String = format!("expected: ({} (: <expr> Expression)), found: {}", DECONS_ATOM_SYMBOL, decons);
            return finished_result(error_msg(decons, error), bindings, prev);
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
            return finished_result(error_msg(cons, error), bindings, prev);
        }
    };
    let mut children = vec![head];
    children.extend(tail.into_children());
    finished_result(Atom::expr(children), bindings, prev)
}

fn atom_into_atom_bindings(pair: Atom) -> (Atom, Bindings) {
    match_atom!{
        pair ~ [atom, bindings] => (atom, atom_into_bindings(bindings)),
        _ => {
            panic!("(Atom Bindings) pair is expected, {} was received", pair)
        }
    }
}

fn atom_into_bindings(bindings: Atom) -> Bindings {
    match bindings.as_gnd::<Bindings>() {
        Some(bindings) => {
            // TODO: cloning is ineffective, but it is not possible
            // to convert grounded atom into internal value at the
            // moment
            bindings.clone()
        },
        _ => panic!("Unexpected state: second item cannot be converted to Bindings"),
    }
}

fn superpose_bind(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: superpose, ret: _, finished: _, vars: _ } = stack;
    let collapsed = match_atom!{
        superpose ~ [_op, Atom::Expression(collapsed)] => collapsed,
        _ => {
            let error: String = format!("expected: ({} (: <collapsed> Expression)), found: {}", SUPERPOSE_BIND_SYMBOL, superpose);
            return finished_result(error_msg(superpose, error), bindings, prev);
        }
    };
    collapsed.into_children().into_iter()
        .map(atom_into_atom_bindings)
        .flat_map(|(atom, b)| {
            let result = |atom, bindings| {
                let stack = Stack::finished(prev.clone(), atom);
                InterpretedAtom(stack, bindings)
            };
            b.merge_v2(&bindings).into_iter().filter_map(move |b| {
                if b.has_loops() {
                    None
                } else {
                    Some(result(atom.clone(), b))
                }
            })
        })
        .collect()
}

type NativeFunc = fn(Atom, Bindings) -> MettaResult;

fn call_native_symbol(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: call, ret: _, finished: _, vars } = stack;
    let (name, func, args) = match_atom!{
        call ~ [_op, name, func, args]
            if func.as_gnd::<NativeFunc>().is_some() => (name, func, args),
        _ => {
            let error = format!("expected: ({} func args), found: {}", CALL_NATIVE_SYMBOL, call);
            return finished_result(error_msg(call, error), bindings, prev);
        }
    };

    let call_stack = Some(call_to_stack(Atom::expr([name, args.clone()]), vars, prev));
    let func = func.as_gnd::<NativeFunc>().expect("Unexpected state");
    func(args, bindings)
        .map(|(atom, bindings)| InterpretedAtom(atom_to_stack(atom, call_stack.clone()), bindings))
        .collect()
}

fn metta_sym(stack: Stack, bindings: Bindings) -> Vec<InterpretedAtom> {
    let Stack{ prev, atom: metta, ret: _, finished: _, vars: _ } = stack;
    let (atom, typ, space) = match_atom!{
        metta ~ [_op, atom, typ, space]
            if space.as_gnd::<DynSpace>().is_some() => (atom, typ, space),
        _ => {
            let error = format!("expected: ({} atom type space), found: {}", METTA_SYMBOL, metta);
            return finished_result(error_msg(metta, error), bindings, prev);
        }
    };

    vec![InterpretedAtom(atom_to_stack(call_native!(metta_impl, Atom::expr([atom, typ, space])), prev), bindings)]
}

type MettaResult = Box<dyn Iterator<Item=(Atom, Bindings)>>;

#[inline]
fn once<'a, T: 'a>(data: T) -> Box<dyn Iterator<Item=T> + 'a> {
    Box::new(std::iter::once(data))
}

#[inline]
fn empty<'a, T: 'a>() -> Box<dyn Iterator<Item=T> + 'a> {
    Box::new(std::iter::empty())
}

#[inline]
fn call_native_atom(func: NativeFunc, name: &str, args: Atom) -> Atom {
    function_atom(Atom::expr([CALL_NATIVE_SYMBOL, Atom::sym(name), Atom::value(func), args]))
}

#[inline]
fn return_atom(atom: Atom) -> Atom {
    Atom::expr([RETURN_SYMBOL, atom])
}

#[inline]
fn function_atom(atom: Atom) -> Atom {
    Atom::expr([FUNCTION_SYMBOL, atom])
}

fn metta_impl(args: Atom, bindings: Bindings) -> MettaResult {
    let (atom, typ, space) = match_atom!{
        args ~ [atom, typ, space]
            if space.as_gnd::<DynSpace>().is_some() => (atom, typ, space),
        _ => {
            let error = format!("expected args: (atom type space), found: {}", args);
            return once((return_atom(error_msg(call_native!(metta_impl, args), error)), bindings));
        }
    };

    let meta = get_meta_type(&atom);
    if typ == ATOM_TYPE_ATOM {
        once((return_atom(atom), bindings))
    } else if typ == meta {
        once((return_atom(atom), bindings))
    } else {
        if meta == ATOM_TYPE_VARIABLE {
            once((return_atom(atom), bindings))
        } else if meta == ATOM_TYPE_SYMBOL {
            type_cast(space, atom, typ, bindings)
        } else if meta == ATOM_TYPE_GROUNDED {
            type_cast(space, atom, typ, bindings)
        } else {
            let var = Atom::Variable(VariableAtom::new("x").make_unique());
            let res = Atom::Variable(VariableAtom::new("res").make_unique());
            once((Atom::expr([CHAIN_SYMBOL, Atom::expr([COLLAPSE_BIND_SYMBOL, call_native!(interpret_expression, Atom::expr([atom, typ, space]))]), var.clone(),
                Atom::expr([CHAIN_SYMBOL, call_native!(check_alternatives, Atom::expr([var])), res.clone(),
                    return_atom(res)
                ])
            ]), bindings))
        }
    }
}

fn get_meta_type(atom: &Atom) -> Atom {
    match atom {
        Atom::Variable(_) => ATOM_TYPE_VARIABLE,
        Atom::Symbol(_) => ATOM_TYPE_SYMBOL,
        Atom::Expression(_) => ATOM_TYPE_EXPRESSION,
        Atom::Grounded(_) => ATOM_TYPE_GROUNDED,
    }
}

fn type_cast(space: Atom, atom: Atom, expected_type: Atom, bindings: Bindings) -> MettaResult {
    let meta = get_meta_type(&atom);
    if expected_type == meta {
        once((return_atom(atom), bindings))
    } else {
        let space = space.as_gnd::<DynSpace>().unwrap();
        let first_match = get_atom_types(space, &atom).into_iter()
            .map(|actual_type| match_types(&expected_type, &actual_type, bindings.clone()))
            .filter(|res| res.is_ok())
            .flat_map(|res| {
                match res {
                    Ok(it) => it,
                    Err(_) => panic!("Unexpected state"),
                }
            })
            .next();
        match first_match {
            Some(bindings) => once((return_atom(atom), bindings)),
            None => once((return_atom(error_atom(atom, BAD_TYPE_SYMBOL)), bindings)),
        }
    }
}

fn match_types(type1: &Atom, type2: &Atom, bindings: Bindings) -> Result<MatchResultIter, MatchResultIter>  {
    if *type1 == ATOM_TYPE_UNDEFINED
        || *type2 == ATOM_TYPE_UNDEFINED
        || *type1 == ATOM_TYPE_ATOM
        || *type2 == ATOM_TYPE_ATOM {
        Ok(once(bindings))
    } else {
        let bindings_copy = bindings.clone();
        let mut result = match_atoms(type1, type2)
            .flat_map(move |b| b.merge_v2(&bindings).into_iter())
            .peekable();
        if result.peek().is_none() {
            log::trace!("match_types: no match: {} !~ {}", type1, type2);
            Err(once(bindings_copy))
        } else {
            if log::log_enabled!(log::Level::Trace) {
                let result: Vec<Bindings> = result.collect();
                log::trace!("match_types: match: {} ~ {}, bindings {:?}", type1, type2, result);
                Ok(Box::new(result.into_iter()))
            } else {
                Ok(Box::new(result))
            }
        }
    }
}

fn check_alternatives(args: Atom, bindings: Bindings) -> MettaResult {
    let expr = match_atom!{
        args ~ [Atom::Expression(expr)] => expr,
        _ => {
            let error = format!("expected args: ((: expr Expression)), found: {}", args);
            return once((return_atom(error_msg(call_native!(check_alternatives, args), error)), bindings));
        }
    };
    let results = expr.into_children().into_iter()
        .map(atom_into_atom_bindings);
    let mut succ = results.clone()
        .filter(|(atom, _bindings)| !atom_is_error(&atom))
        .map(|(atom, bindings)| (return_atom(atom), bindings))
        .peekable();
    let err = results
        .filter(|(atom, _bindings)| atom_is_error(&atom))
        .map(|(atom, bindings)| (return_atom(atom), bindings));
    match succ.peek() {
        Some(_) => Box::new(succ),
        None => Box::new(err),
    }
}

fn interpret_expression(args: Atom, bindings: Bindings) -> MettaResult {
    let (expr, expr_typ, space) = match_atom!{
        args ~ [expr, expr_typ, space]
            if space.as_gnd::<DynSpace>().is_some() => (expr, expr_typ, space),
        _ => {
            let error = format!("expected args: (atom type space), found: {}", args);
            return once((return_atom(error_msg(call_native!(interpret_expression, args), error)), bindings));
        }
    };
    match atom_as_slice(&expr) {
        Some([op, _args @ ..]) => {
            let space_ref = space.as_gnd::<DynSpace>().unwrap();
            let actual_types = get_atom_types(space_ref, op);

            let has_tuple_type = actual_types.iter().filter(|typ| !is_func(typ)).next().is_some();
            let tuple = if has_tuple_type {
                let reduced = Atom::Variable(VariableAtom::new("reduced").make_unique());
                let result = Atom::Variable(VariableAtom::new("result").make_unique());
                once((
                    Atom::expr([CHAIN_SYMBOL, call_native!(interpret_tuple, Atom::expr([expr.clone(), space.clone()])), reduced.clone(),
                        Atom::expr([CHAIN_SYMBOL, call_native!(metta_call, Atom::expr([reduced, expr_typ.clone(), space.clone()])), result.clone(),
                            return_atom(result)
                        ])
                    ]), bindings.clone()))
            } else {
                empty()
            };

            let mut func_types = actual_types.into_iter().filter(|typ| is_func(typ)).peekable();
            let func = if func_types.peek().is_some() {
                let ret_typ = expr_typ.clone();
                let type_check_results = func_types.flat_map(|typ| check_if_function_type_is_applicable(&expr, typ, &ret_typ, space_ref, bindings.clone()));
                let mut errors = Vec::new();
                for res in type_check_results {
                    log::debug!("interpret_expression: function type check: expr: {} type: {:?}", expr, res);
                    match res {
                        (Ok(op_type), bindings) => {
                            let reduced = Atom::Variable(VariableAtom::new("reduced").make_unique());
                            let result = Atom::Variable(VariableAtom::new("result").make_unique());
                            return once((Atom::expr([CHAIN_SYMBOL, call_native!(interpret_function, Atom::expr([expr.clone(), op_type, expr_typ.clone(), space.clone()])), reduced.clone(),
                                Atom::expr([CHAIN_SYMBOL, call_native!(metta_call, Atom::expr([reduced, expr_typ, space.clone()])), result.clone(),
                                    return_atom(result)
                                ])
                            ]), bindings));
                        },
                        (Err(err), bindings) => errors.push((err, bindings)),
                    }
                }
                Box::new(errors.into_iter()
                    .map(move |(err, bindings)| (return_atom(err), bindings)))
            } else {
                empty()
            };

            Box::new(std::iter::empty().chain(tuple).chain(func))
        },
        _ => type_cast(space, expr, expr_typ, bindings),
    }
}

fn interpret_tuple(args: Atom, bindings: Bindings) -> MettaResult {
    let (expr, space) = match_atom!{
        args ~ [Atom::Expression(expr), space]
            if space.as_gnd::<DynSpace>().is_some() => (expr, space),
        _ => {
            let error = format!("expected args: ((: expr Expression) space), found: {}", args);
            return once((return_atom(error_msg(call_native!(interpret_tuple, args), error)), bindings));
        }
    };
    if expr.children().is_empty() {
        once((return_atom(Atom::Expression(expr)), bindings))
    } else {
        let mut tuple = expr.into_children();
        let head = tuple.remove(0);
        let tail = tuple;
        let rhead = Atom::Variable(VariableAtom::new("rhead").make_unique());
        let rtail = Atom::Variable(VariableAtom::new("rtail").make_unique());
        let result = Atom::Variable(VariableAtom::new("result").make_unique());
        once((
            Atom::expr([CHAIN_SYMBOL, Atom::expr([METTA_SYMBOL, head, ATOM_TYPE_UNDEFINED, space.clone()]), rhead.clone(),
                Atom::expr([EVAL_SYMBOL, Atom::expr([Atom::gnd(IfEqualOp{}), rhead.clone(), EMPTY_SYMBOL, return_atom(EMPTY_SYMBOL),
                    Atom::expr([CHAIN_SYMBOL, call_native!(interpret_tuple, Atom::expr([Atom::expr(tail), space.clone()])), rtail.clone(),
                        Atom::expr([EVAL_SYMBOL, Atom::expr([Atom::gnd(IfEqualOp{}), rtail.clone(), EMPTY_SYMBOL, return_atom(EMPTY_SYMBOL),
                            Atom::expr([CHAIN_SYMBOL, Atom::expr([CONS_ATOM_SYMBOL, rhead, rtail]), result.clone(),
                                return_atom(result)
                            ])
                        ])])
                    ])
                ])])
            ]), bindings))
    }
}

fn interpret_function(args: Atom, bindings: Bindings) -> MettaResult {
    let (atom, op_type, ret_type, space) = match_atom!{
        args ~ [Atom::Expression(atom), Atom::Expression(op_type), ret_type, space]
            if space.as_gnd::<DynSpace>().is_some() &&
                op_type.children().get(0) == Some(&ARROW_SYMBOL) => (atom, op_type, ret_type, space),
        _ => {
            let error = format!("expected args: ((: atom Expression) (: op_type Expression) ret_type space), found: {}", args);
            return once((return_atom(error_msg(call_native!(interpret_function, args), error)), bindings));
        }
    };
    let mut call = atom.clone().into_children();
    let head = call.remove(0);
    let args = call;
    let mut arg_types = op_type.clone();
    arg_types.children_mut().remove(0);
    let arg_types = Atom::Expression(arg_types);
    let rop = Atom::Variable(VariableAtom::new("rop").make_unique());
    let rargs = Atom::Variable(VariableAtom::new("rargs").make_unique());
    let result = Atom::Variable(VariableAtom::new("result").make_unique());
    let unpacked_args = Atom::Variable(VariableAtom::new("unpacked_args").make_unique());
    let call_interpret_args = call_native!(interpret_args, Atom::expr([Atom::Expression(atom), Atom::expr(args), arg_types, ret_type, space.clone()]));
    once((
        Atom::expr([CHAIN_SYMBOL, Atom::expr([METTA_SYMBOL, head, Atom::Expression(op_type), space.clone()]), rop.clone(),
            call_native!(return_on_error, Atom::expr([rop.clone(), 
                Atom::expr([CHAIN_SYMBOL, call_interpret_args.clone(), rargs.clone(),
                    Atom::expr([UNIFY_SYMBOL, Atom::expr([Atom::sym("Ok"), unpacked_args.clone()]), rargs.clone(),
                        Atom::expr([CHAIN_SYMBOL, Atom::expr([CONS_ATOM_SYMBOL, rop, unpacked_args]), result.clone(),
                            return_atom(result)
                        ]),
                        return_atom(rargs)
                    ])
                ])
            ]))
        ]), bindings))
}

fn check_if_function_type_is_applicable<'a>(expr: &'a Atom, op_type: Atom, expected_type: &'a Atom, space: &'a DynSpace, bindings: Bindings) -> Box<dyn Iterator<Item=(Result<Atom, Atom>, Bindings)> + 'a> {
    log::trace!("check_if_function_type_is_applicable: function type check: expr: {}, op_type: {}, expected_type: {}", expr, op_type, expected_type);
    let actual_args = match atom_as_slice(expr) {
        Some([_op, actual_args @ ..]) => actual_args,
        _ => panic!("Unexpected state"),
    };
    let arg_types: ExpressionAtom = op_type.clone().try_into().unwrap();
    let mut arg_types = arg_types.into_children(); 
    let arrow = arg_types.remove(0);
    assert_eq!(arrow, ARROW_SYMBOL);
    check_if_function_type_is_applicable_(expr, op_type, arg_types, actual_args, expected_type, space, bindings)
}

fn is_meta_type(atom: &Atom) -> bool {
    if *atom == ATOM_TYPE_ATOM
        || *atom == ATOM_TYPE_SYMBOL
        || *atom == ATOM_TYPE_VARIABLE
        || *atom == ATOM_TYPE_EXPRESSION
        || *atom == ATOM_TYPE_GROUNDED {
        true
    } else {
        false
    }
}

fn match_meta_types(actual: &Atom, expected: &Atom) -> bool {
    if *expected == ATOM_TYPE_ATOM {
        true
    } else {
        actual == expected
    }
}

fn check_if_function_type_is_applicable_<'a>(expr: &'a Atom, op_type: Atom, mut arg_types: Vec<Atom>, actual_args: &'a[Atom], expected_type: &'a Atom, space: &'a DynSpace, bindings: Bindings) -> Box<dyn Iterator<Item=(Result<Atom, Atom>, Bindings)> + 'a> {
    match arg_types.len() {
        0 => once((Err(error_atom(expr.clone(), INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL)), bindings)),
        1 => {
            let ret_type = arg_types.pop().unwrap();
            log::trace!("check_if_function_type_is_applicable_: function type check: expr: {}, ret_type: {}, expected_type: {}", expr, ret_type, expected_type);
            match actual_args {
                [] => {
                    // Here expected_type is always some specific type not meta-type. It is because
                    // there are two places to assign expected_type. First place is passing result of
                    // the function call to another function. In this case expected_type is an expected
                    // type of the outer function argument. Second place is explicit call to
                    // `metta`. In this case expected_type is explicitly set as an argument and handled
                    // in metta_impl() Rust function which compares it with passed expression
                    // meta-type. Thus if expected_type is meta-type it is always first compared to the
                    // expression's meta-type and type check finishes.
                    match match_types(&ret_type, expected_type, bindings) {
                        Ok(matches) => Box::new(matches.map(move |bindings| (Ok(op_type.clone()), bindings))),
                        Err(nomatch) => Box::new(nomatch.map(move |bindings| (Err(error_atom(expr.clone(), BAD_TYPE_SYMBOL)), bindings))),
                    }
                },
                _ => once((Err(error_atom(expr.clone(), INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL)), bindings)),
            }
        },
        _ => {
            let formal_arg_type = arg_types.remove(0);
            let arg_types_tail = arg_types;
            match actual_args {
                [] => once((Err(error_atom(expr.clone(), INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL)), bindings)),
                [actual_arg, args_tail @ ..] => {
                    if is_meta_type(&formal_arg_type) && match_meta_types(&get_meta_type(actual_arg), &formal_arg_type) {
                        check_if_function_type_is_applicable_(expr, op_type, arg_types_tail, args_tail, expected_type, space, bindings)
                    } else {
                        let mut actual_arg_types = get_atom_types(space, actual_arg).into_iter().peekable();
                        if  actual_arg_types.peek().is_none() {
                            return once((Err(error_atom(actual_arg.clone(), BAD_TYPE_SYMBOL)), bindings))
                        }
                        let actual_arg_types = actual_arg_types.inspect(move |typ| log::trace!("check_if_function_type_is_applicable_: function type check: expr: {}, actual_arg: {}, actual_type: {}", expr, actual_arg, typ));
                        let iter = actual_arg_types.flat_map(move |actual_arg_type| -> Box<dyn Iterator<Item=(Result<Atom, Atom>, Bindings)> + '_> {
                            let arg_types_tail = arg_types_tail.clone();
                            let op_type = op_type.clone();
                            match match_types(&formal_arg_type, &actual_arg_type, bindings.clone()) {
                                Ok(matches) => Box::new(matches.flat_map(move |bindings| check_if_function_type_is_applicable_(expr, op_type.clone(), arg_types_tail.clone(), args_tail, expected_type, space, bindings))),
                                Err(nomatch) => Box::new(nomatch.map(|bindings| (Err(error_atom(actual_arg.clone(), BAD_TYPE_SYMBOL)), bindings))),
                            }
                        });
                        Box::new(iter)
                    }
                },
            }
        },
    }
}

fn interpret_args(args_: Atom, bindings: Bindings) -> MettaResult {
    let (atom, args, arg_types, ret_type, space) = match_atom!{
        args_ ~ [atom, Atom::Expression(args), Atom::Expression(arg_types), ret_type, space]
            if space.as_gnd::<DynSpace>().is_some() => (atom, args, arg_types, ret_type, space),
        _ => {
            let error = format!("expected args: (atom (: args Expression) (: arg_types Expression) ret_type space), found: {}", args_);
            return once((return_atom(error_msg(call_native!(interpret_args, args_), error)), bindings));
        }
    };
    let mut types = arg_types.into_children();
    if types.is_empty() {
        return once((return_atom(error_atom(atom, INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL)), bindings));
    }
    let types_head = types.remove(0);
    let types_tail = types;
    if args.children().is_empty() {
        if types_tail.is_empty() {
            match match_types(&types_head, &ret_type, bindings) {
                Ok(matches) => Box::new(matches.map(move |bindings| (return_atom(Atom::expr([Atom::sym("Ok"), Atom::Expression(args.clone())])), bindings))),
                Err(nomatch) => Box::new(nomatch.map(move |bindings| (return_atom(error_atom(atom.clone(), BAD_TYPE_SYMBOL)), bindings))),
            }
        } else {
            once((return_atom(error_atom(atom, INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL)), bindings))
        }
    } else {
        let mut args = args.into_children();
        let args_head = args.remove(0);
        let args_tail = args;
        let rhead = Atom::Variable(VariableAtom::new("rhead").make_unique());
        let rtail = Atom::Variable(VariableAtom::new("rtail").make_unique());
        let result = Atom::Variable(VariableAtom::new("result").make_unique());
        let tail = Atom::Variable(VariableAtom::new("tail").make_unique());
        let call_self = call_native!(interpret_args, Atom::expr([atom, Atom::expr(args_tail), Atom::expr(types_tail), ret_type, space.clone()]));
        let recursion = Atom::expr([CHAIN_SYMBOL, call_self.clone(), rtail.clone(),
            Atom::expr([UNIFY_SYMBOL, Atom::expr([Atom::sym("Ok"), tail.clone()]), rtail.clone(),
                Atom::expr([CHAIN_SYMBOL, Atom::expr([CONS_ATOM_SYMBOL, rhead.clone(), tail]), result.clone(),
                    return_atom(Atom::expr([Atom::sym("Ok"), result]))
                ]),
                return_atom(rtail)
            ])
        ]);
        once((
            Atom::expr([CHAIN_SYMBOL, Atom::expr([METTA_SYMBOL, args_head.clone(), types_head, space.clone()]), rhead.clone(),
                Atom::expr([EVAL_SYMBOL, Atom::expr([Atom::gnd(IfEqualOp{}), rhead.clone(), args_head,
                    recursion.clone(),
                    call_native!(return_on_error, Atom::expr([rhead, 
                        recursion
                    ]))
                ])])
            ]), bindings))
    }
}

fn return_on_error(args: Atom, bindings: Bindings) -> MettaResult {
    let (atom, then) = match_atom!{
        args ~ [atom, then] => (atom, then),
        _ => {
            let error = format!("expected args: (atom then), found: {}", args);
            return once((return_atom(error_msg(call_native!(return_on_error, args), error)), bindings));
        }
    };
    if EMPTY_SYMBOL == atom {
        once((return_atom(return_atom(EMPTY_SYMBOL)), bindings))
    } else if atom_is_error(&atom) {
        once((return_atom(return_atom(atom)), bindings))
    } else {
        once((return_atom(then), bindings))
    }
}

fn metta_call(args: Atom, bindings: Bindings) -> MettaResult {
    let (atom, typ, space) = match_atom!{
        args ~ [atom, typ, space]
            if space.as_gnd::<DynSpace>().is_some() => (atom, typ, space),
        _ => {
            let error = format!("expected args: (atom type space), found: {}", args);
            return once((return_atom(error_msg(call_native!(metta_call, args), error)), bindings));
        }
    };
    if atom_is_error(&atom) {
        once((return_atom(atom), bindings))
    } else {
        let result = Atom::Variable(VariableAtom::new("result").make_unique());
        let ret = Atom::Variable(VariableAtom::new("ret").make_unique());
        once((
            // TODO: At the moment metta_call() is called we already know
            // should we call atom as a tuple or as a function.
            // But (eval (<op> <args>)) inside independently decides whether it
            // should call grounded operation <op>, or match (= (<op> <args>) <res>).
            // This can lead to the conflict if user defines a function using
            // grounded atom (for instance (+3 1 2 3)) and after type analysis
            // interpreter decides we need to match it then calling eval will
            // analyze the expression again and may call grounded op instead of
            // matching.
            Atom::expr([CHAIN_SYMBOL, Atom::expr([EVAL_SYMBOL, atom.clone()]), result.clone(),
                Atom::expr([CHAIN_SYMBOL, call_native!(metta_call_return, Atom::expr([atom, result, typ, space])), ret.clone(),
                    return_atom(ret)
                ])
            ]), bindings))
    }
}

fn metta_call_return(args: Atom, bindings: Bindings) -> MettaResult {
    let (atom, result, typ, space) = match_atom!{
        args ~ [atom, result, typ, space]
            if space.as_gnd::<DynSpace>().is_some() => (atom, result, typ, space),
        _ => {
            let error = format!("expected args: (atom result type space), found: {}", args);
            return once((return_atom(error_msg(call_native!(metta_call_return, args), error)), bindings));
        }
    };
    if NOT_REDUCIBLE_SYMBOL == result {
        once((return_atom(atom), bindings))
    } else if EMPTY_SYMBOL == result {
        once((return_atom(EMPTY_SYMBOL), bindings))
    } else if atom_is_error(&result) {
        once((return_atom(result), bindings))
    } else {
        let ret = Atom::Variable(VariableAtom::new("ret").make_unique());
        once((
            Atom::expr([CHAIN_SYMBOL, Atom::expr([METTA_SYMBOL, result, typ, space]), ret.clone(),
                return_atom(ret)
            ]), bindings))
    }
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
    fn interpret_atom_evaluate_pure_expression_variable_in_space() {
        let space = space("$t (= (foo $a B) $a)");
        let result = call_interpret(&space, &metta_atom("(eval (foo A $b))"));
        assert_eq!(result, vec![metta_atom("A")]);
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
    fn interpret_atom_evaluate_variable_operation() {
        let space = space("(= (foo $a B) $a)");
        let result = call_interpret(&space, &metta_atom("(eval ($a A $b))"));
        #[cfg(feature = "variable_operation")]
        assert_eq!(result, vec![metta_atom("A")]);
        #[cfg(not(feature = "variable_operation"))]
        assert_eq!(result, vec![NOT_REDUCIBLE_SYMBOL]);
    }

    #[test]
    fn interpret_atom_evaluate_variable_via_call_direct_equality() {
        let space = space("
            (= (bar) (function (return ())))
            (= (foo $b) (function
              (chain (eval (bar)) $_
              (unify $b value
                (return ())
                (return (Error () \"Unexpected error\")) ))))");
        let result = call_interpret(&space,
            &metta_atom("(chain (eval (foo $a)) $_ $a)"));
        assert_eq!(result[0], sym!("value"));
    }

    #[test]
    fn interpret_atom_evaluate_variable_via_call_struct_equality() {
        let formal_arg_struct = space("
            (= (bar) (function (return ())))
            (= (foo ($b)) (function
              (chain (eval (bar)) $_
              (unify $b value
                (return ())
                (return (Error () \"Unexpected error\")) ))))");
        let result = call_interpret(&formal_arg_struct,
            &metta_atom("(chain (eval (foo $a)) $_ $a)"));
        assert_eq!(result[0], expr!(("value")));

        let actual_arg_struct = space("
            (= (bar) (function (return ())))
            (= (foo $b) (function
              (chain (eval (bar)) $_
              (unify $b (value)
                (return ())
                (return (Error () \"Unexpected error\")) ))))");
        let result = call_interpret(&actual_arg_struct,
            &metta_atom("(chain (eval (foo ($a))) $_ $a)"));
        assert_eq!(result[0], sym!("value"));
    }

    #[test]
    fn interpret_atom_evaluate_variable_operation_nested() {
        let space = space("(= ((baz $a) $b) ($a $b))");
        let result = call_interpret(&space, &metta_atom("(eval (($a A) B))"));
        #[cfg(feature = "variable_operation")]
        assert_eq!(result, vec![metta_atom("(A B)")]);
        #[cfg(not(feature = "variable_operation"))]
        assert_eq!(result, vec![NOT_REDUCIBLE_SYMBOL]);
    }


    #[test]
    fn interpret_atom_chain_incorrect_args() {
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
    fn interpret_atom_chain_keep_var_from_evaluated_part() {
        let result = call_interpret(&space("(= (even 4) True)"), &metta_atom("(chain (eval (even $x)) $res (= (is-even $x) $res))"));
        assert_eq!(result, vec![metta_atom("(= (is-even 4) True)")]);
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
    fn test_superpose_bind() {
        let vars: Variables = [ "a", "b", "c" ].into_iter().map(VariableAtom::new).collect();
        let atom = Atom::expr([Atom::sym("superpose-bind"),
            Atom::expr([atom_bindings_into_atom(expr!("foo" a b), bind!{ a: expr!("A"), c: expr!("C") })])]);
        let stack = Stack{ prev: None, atom, ret: no_handler, finished: false, vars: vars.clone() };

        let result = superpose_bind(stack, bind!{ b: expr!("B"), d: expr!("D") });

        assert_eq!(result, vec![InterpretedAtom(
                Stack{ prev: None, atom: expr!("foo" a b), ret: no_handler, finished: true, vars: Variables::new() },
                bind!{ a: expr!("A"), b: expr!("B"), c: expr!("C"), d: expr!("D") }
        )]);
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

    fn call_interpret<'a, T: Space>(space: T, atom: &Atom) -> Vec<Atom> {
        let _ = env_logger::builder().is_test(true).try_init();
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
        fn as_execute(&self) -> Option<&dyn CustomExecute> {
            Some(self)
        }
    }

    impl CustomExecute for ThrowError {
        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Err((*args[0].as_gnd::<&str>().unwrap()).into())
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
        fn as_execute(&self) -> Option<&dyn CustomExecute> {
            Some(self)
        }
    }

    impl CustomExecute for NonReducible {
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Err(ExecError::NoReduce)
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
        fn as_execute(&self) -> Option<&dyn CustomExecute> {
            Some(self)
        }
    }

    impl CustomExecute for MulXUndefinedType {
        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![Atom::value(self.0 * args.get(0).unwrap().as_gnd::<i32>().unwrap())])
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
        fn as_execute(&self) -> Option<&dyn CustomExecute> {
            Some(self)
        }
    }

    impl CustomExecute for ReturnNothing {
        fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![])
        }
    }

    impl Display for ReturnNothing {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "return-nothing")
        }
    }

    #[test]
    fn interpret_duplicated_types() {
        let space = DynSpace::new(space("
            (: foo (-> A A))
            (: foo (-> A A))
            (: foo (-> Atom A))
            (: a A)
            (= (foo $x) a)
        "));
        let result = interpret(&space, &Atom::expr([METTA_SYMBOL, expr!("foo" "a"), ATOM_TYPE_UNDEFINED, Atom::gnd(space.clone())]));
        assert_eq!(result, Ok(vec![metta_atom("a")]));
    }
}
