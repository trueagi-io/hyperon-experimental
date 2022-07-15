//! # Algorithm
//!
//! For an atom and type on input (when type is not set `Undefined` is used):
//! * [Atom::Variable] is returned as is.
//! * [Atom::Symbol] and [Atom::Grounded] are type checked; each applicable
//!   type of the atom is checked vs expected type:
//!   * If type is corrent then atom is returned as is.
//!   * If type is incorrect then error result is returned.
//!   * Note: cast may return as many results as many types were casted
//!     successfully, each result may have its own variable bindings if
//!     types are parameterized.
//! * First atom (operation) of [Atom::Expression] is extracted and plan to
//!   calculate its type is returned. When type is calculated the expression
//!   is interpreted according its operation type. Note: few alternative
//!   interpretations may be found here one for each type of the operation.
//!
//! For and expression atom and its operation type:
//! * If expected type is `Atom` or `Expression` then expression is returned as is.
//! * If operation type is a function:
//!   * Check arity and return type of the function, if check fails then return
//!     error result.
//!   * Return a sequence plan which interprets each argument one by one using
//!     corresponding types and calls resulting expression. If any argument
//!     cannot be casted to type then error result is returned. If argument's
//!     bindings are not compatible with bindings of the expression such
//!     result is skipped if no options to interpret argument left then error
//!     is returned. If argument returns empty value after interpretation
//!     then the whole expression is not interpreted further.
//!   * Note: this step may return more than one result because each
//!     argument can be interpreted by more than one way.
//! * If operation type is not function:
//!   * Return a sequence plan which interprets each member using
//!     `Undefined` type and calls resulting expression. If member's
//!     bindings are not compatible with bindings of the expression such
//!     result is skipped if no options to interpret member left then error
//!     is returned.
//!
//! Call the expression:
//! * If there is a cached result for this expression then return it
//! * If operation is instance of [Atom::Grounded] then operation is executed:
//!   * If result is error then error is returned
//!   * If result is empty then it is returned as is
//!   * If result is not empty plan to interpret each alternative further is
//!     returned. Note: if each alternative returns error then the result
//!     of execution is also error.
//! * If operation is not [Atom::Grounded] then expression is matched.
//!   Atomspace is queried for `(= <expr> $X)` and expression is replaced by $X.
//!   * If no results returned then error is returned
//!   * If result is not empty plan to interpret each alternative further is
//!     returned. Note: if each alternative returns error then the result
//!     of execution is also error.
//! * If one of previous steps returned error then original expresion is
//!   returned. Otherwise the result of the interpretation is returned.
//!   It may be empty if one of expression is grounded expression which
//!   returns empty result.
//!
//! Summary on possible results:
//! * empty result for expression is returned when grounded operation
//!   returns empty result, or one of the arguments returned empty result
//! * error is returned when atom cannot be casted to the type expected
//!   or all alternative interpretations are errors; the overall result includes
//!   successfuly interpreted alternatives only
//! * call of the expression returns either succesful result or original expression

use crate::*;
use crate::common::plan::*;
use crate::atom::subexpr::*;
use crate::atom::matcher::*;
use crate::space::grounding::*;
use crate::common::collections::ListMap;
use crate::metta::*;
use crate::metta::types::{is_func, get_arg_types, check_type_bindings,
    get_atom_types, match_reducted_types};

use std::ops::Deref;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};

/// Result of atom interpretation plus variable bindings found
#[derive(Clone, PartialEq)]
pub struct InterpretedAtom(Atom, Bindings);

impl InterpretedAtom {
    fn atom(&self) -> &Atom {
        &self.0
    }

    fn bindings(&self) -> &Bindings {
        &self.1
    }
    
    /// Convert the instance into tuple of [Atom] and [Bindings]
    pub fn into_tuple(self) -> (Atom, Bindings) {
        (self.0, self.1)
    }
}

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

type Results = Vec<InterpretedAtom>;
type NoInputPlan = Box<dyn Plan<(), Results>>;

/// Initialize interpreter and returns the result of the zero step.
/// It can be error, immediate result or interpretation plan to be executed.
/// See [crate::metta::interpreter] for algorithm explanation.
///
/// # Arguments
/// * `space` - atomspace to query for interpretation
/// * `expr` - atom to interpret
pub fn interpret_init(space: GroundingSpace, expr: &Atom) -> StepResult<Vec<InterpretedAtom>> {
    let context = InterpreterContextRef::new(space);
    interpret_as_type_plan(context,
        InterpretedAtom(expr.clone(), Bindings::new()),
        ATOM_TYPE_UNDEFINED)
}

/// Perform next step of the interpretation plan and return the result. Panics
/// when [StepResult::Return] or [StepResult::Error] are passed as input.
/// See [crate::metta::interpreter] for algorithm explanation.
///
/// # Arguments
/// * `step` - [StepResult::Execute] result from the previous step.
pub fn interpret_step(step: StepResult<Vec<InterpretedAtom>>) -> StepResult<Vec<InterpretedAtom>> {
    log::debug!("current plan:\n{:?}", step);
    match step {
        StepResult::Execute(plan) => plan.step(()),
        StepResult::Return(_) => panic!("Plan execution is finished already"),
        StepResult::Error(_) => panic!("Plan execution is finished with error"),
    }
}

/// Interpret passed atom and return a new plan, result or error. This function
/// blocks until result is calculated. For step by step interpretation one
/// should use [interpret_init] and [interpret_step] functions.
/// # Arguments
/// * `space` - atomspace to query for interpretation
/// * `expr` - atom to interpret
pub fn interpret(space: GroundingSpace, expr: &Atom) -> Result<Vec<Atom>, String> {
    let mut step = interpret_init(space, expr);
    while step.has_next() {
        step = interpret_step(step);
    }
    match step {
        StepResult::Return(mut result) => Ok(result.drain(0..)
            .map(|InterpretedAtom(atom, _)| atom).collect()),
        StepResult::Error(message) => Err(message),
        _ => panic!("Not expected step result: {:?}", step),
    }
}

// TODO: ListMap is not effective but we cannot use HashMap here without
// requiring hash functions for the grounded atoms.
struct InterpreterCache(ListMap<Atom, Results>);

impl InterpreterCache {
    fn new() -> Self {
        Self(ListMap::new())
    }

    fn get(&self, key: &Atom, current_bindings: &Bindings) -> Option<Results> {
        self.0.get(key).map(|results| -> Option<Results> {
                let mut inconsistent = Vec::new();
                let mut result = Vec::new();
                for res in results {
                    let merged = Bindings::merge(res.bindings(), &current_bindings);
                    if let Some(merged) = merged {
                        result.push(InterpretedAtom(res.atom().clone(), merged));
                    } else {
                        inconsistent.push(res);
                    }
                }
                if inconsistent.is_empty() {
                    Some(result)
                } else {
                    log::debug!("get_cached: return None as some results has inconsistent bindings");
                    log::debug!("get_cached: current bindings: {}, inconsistent results: {:?}", current_bindings, inconsistent);
                    None
                }
            }).flatten()
    }

    fn insert(&mut self, key: Atom, value: Results) {
        self.0.insert(key, value)
    }

    fn reset(&mut self) {
        self.0.clear();
    }
}

impl SpaceObserver for InterpreterCache {
    fn notify(&mut self, _event: &SpaceEvent) {
        // TODO: implement more specific cache cleanup for each event
        self.reset();
    }
}

struct InterpreterContext {
    space: GroundingSpace,
    cache: Rc<RefCell<InterpreterCache>>,
}

#[derive(Clone)]
struct InterpreterContextRef(Rc<InterpreterContext>);

impl InterpreterContextRef {
    fn new(mut space: GroundingSpace) -> Self {
        let cache = Rc::new(RefCell::new(InterpreterCache::new()));
        space.register_observer(Rc::clone(&cache));
        Self(Rc::new(InterpreterContext{ space, cache }))
    }
}

impl Deref for InterpreterContextRef {
    type Target = InterpreterContext;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

fn is_grounded_op(expr: &ExpressionAtom) -> bool {
    match expr.children().get(0) { 
        Some(Atom::Grounded(op)) if is_func(&op.type_())
            || op.type_() == ATOM_TYPE_UNDEFINED => true,
        _ => false,
    }
}

fn has_grounded_sub_expr(expr: &Atom) -> bool {
    return SubexprStream::from_expr(expr.clone(), TOP_DOWN_DEPTH_WALK)
        .any(|sub| if let Atom::Expression(sub) = sub {
            is_grounded_op(&sub)
        } else {
            panic!("Expression is expected");
        });
}

fn interpret_as_type_plan(context: InterpreterContextRef,
        input: InterpretedAtom, typ: Atom) -> StepResult<Results> {
    log::debug!("interpret_as_type_plan: input: {}, type: {}", input, typ);
    match input.atom() {
        Atom::Symbol(_) | Atom::Grounded(_) =>
            cast_atom_to_type_plan(context, input, typ),

        Atom::Expression(ref expr) if expr.children().is_empty() =>
            cast_atom_to_type_plan(context, input, typ),

        Atom::Expression(ref expr) => {
            let op = &expr.children()[0];
            StepResult::execute(SequencePlan::new(
                    get_type_of_atom_plan(context.clone(), op.clone()),
                    interpret_expression_as_type_plan(context, input, typ)
            ))
        },

        Atom::Variable(_) => {
            StepResult::ret(vec![input])
        },
    }
}

fn cast_atom_to_type_plan(context: InterpreterContextRef,
        input: InterpretedAtom, typ: Atom) -> StepResult<Results> {
    // TODO: implement this via interpreting of the (:cast atom typ) expression
    let typ = apply_bindings_to_atom(&typ, input.bindings());
    let mut results = check_type_bindings(&context.space, input.atom(), &typ);
    log::debug!("cast_atom_to_type_plan: type check results: {:?}", results);
    if !results.is_empty() {
        log::debug!("cast_atom_to_type_plan: input: {} is casted to type: {}", input, typ);
        StepResult::ret(results.drain(0..).map(|(_match_typ, typ_bindings)| {
            let InterpretedAtom(atom, bindings) = input.clone();
            // TODO: need to understand if it is needed to apply bindings
            // should we apply bindings to bindings?
            let bindings = Bindings::merge(&bindings, &typ_bindings);
            if let Some(bindings) = bindings {
                let atom = apply_bindings_to_atom(&atom, &bindings);
                Some(InterpretedAtom(atom, bindings))
            } else {
                None
            }
        }).filter(Option::is_some).map(Option::unwrap).collect())
    } else {
        log::debug!("cast_atom_to_type_plan: input: {} cannot be casted to type: {}", input, typ);
        StepResult::err(format!("Incorrect type, input: {}, type: {}", input, typ))
    }
}

fn get_type_of_atom_plan(context: InterpreterContextRef, atom: Atom) -> StepResult<Vec<Atom>> {
    // TODO: implement this via interpreting of the (:? atom)
    StepResult::ret(get_atom_types(&context.space, &atom))
}

fn interpret_expression_as_type_plan(context: InterpreterContextRef,
        input: InterpretedAtom, typ: Atom) -> OperatorPlan<Vec<Atom>, Results> {
    let descr = format!("form alternative plans for expression {} using types", input);
    OperatorPlan::new(move |op_types: Vec<Atom>| {
        make_alternives_plan(input.clone(), op_types, move |op_typ| {
            interpret_expression_as_type_op(context.clone(),
                input.clone(), op_typ, typ.clone())
        })
    }, descr)
}

fn get_expr(atom: &Atom) -> &ExpressionAtom {
    match atom {
        Atom::Expression(expr) => expr,
        _ => panic!("Atom::Expression is expected, recieved: {}", atom),
    }
}

fn get_expr_mut(atom: &mut Atom) -> &mut ExpressionAtom {
    match atom {
        Atom::Expression(expr) => expr,
        _ => panic!("Atom::Expression is expected, recieved: {}", atom),
    }
}

fn interpret_expression_as_type_op(context: InterpreterContextRef,
        input: InterpretedAtom, op_typ: Atom, ret_typ: Atom) -> NoInputPlan {
    log::debug!("interpret_expression_as_type_op: input: {}, operation type: {}, expected return type: {}", input, op_typ, ret_typ);
    if ret_typ == ATOM_TYPE_ATOM || ret_typ == ATOM_TYPE_EXPRESSION {
        Box::new(StepResult::ret(vec![input]))
    } else if is_func(&op_typ) {
        let InterpretedAtom(input_atom, mut input_bindings) = input;
        let expr = get_expr(&input_atom);
        let (op_arg_types, op_ret_typ) = get_arg_types(&op_typ);
        // TODO: supertypes should be checked as well
        if !match_reducted_types(op_ret_typ, &ret_typ, &mut input_bindings) {
            Box::new(StepResult::err(format!("Operation returns wrong type: {}, expected: {}", op_ret_typ, ret_typ)))
        } else if op_arg_types.len() != (expr.children().len() - 1) {
            Box::new(StepResult::err(format!("Operation arity is not equal to call arity: operation type, {}, call: {}", op_typ, expr)))
        } else {
            let input = InterpretedAtom(input_atom, input_bindings);
            let expr = get_expr(input.atom());
            assert!(!expr.children().is_empty(), "Empty expression is not expected");
            let mut plan: NoInputPlan = Box::new(StepResult::ret(vec![input.clone()]));
            for expr_idx in 1..(expr.children().len()) {
                let arg = expr.children()[expr_idx].clone();
                let arg_typ = op_arg_types[expr_idx - 1].clone();
                let context = context.clone();
                plan = Box::new(SequencePlan::new(
                    plan,
                    OperatorPlan::new(move |mut results: Results| {
                        let alternatives = results.drain(0..).map(|result| -> NoInputPlan {
                            let arg_typ = apply_bindings_to_atom(&arg_typ, result.bindings());
                            Box::new(SequencePlan::new(
                                interpret_as_type_plan(context.clone(),
                                    InterpretedAtom(arg.clone(), result.bindings().clone()),
                                    arg_typ),
                                insert_reducted_arg_plan(result, expr_idx)))
                        }).collect();
                        StepResult::execute(AlternativeInterpretationsPlan::new(arg, alternatives))
                    }, format!("Interpret {} argument", expr_idx))
                ))
            }
            call_alternatives_plan(plan, context, input)
        }
    } else {
        let expr = get_expr(input.atom());
        let mut plan: NoInputPlan = Box::new(StepResult::ret(vec![input.clone()]));
        for expr_idx in 0..(expr.children().len()) {
            let arg = expr.children()[expr_idx].clone();
            let context = context.clone();
            plan = Box::new(SequencePlan::new(
                plan,
                OperatorPlan::new(move |mut results: Results| {
                    let alternatives = results.drain(0..).map(|result| -> NoInputPlan {
                        Box::new(SequencePlan::new(
                            interpret_as_type_plan(context.clone(),
                                InterpretedAtom(arg.clone(), result.bindings().clone()),
                                ATOM_TYPE_UNDEFINED),
                            insert_reducted_arg_plan(result, expr_idx)))
                    }).collect();
                    StepResult::execute(AlternativeInterpretationsPlan::new(arg, alternatives))
                }, format!("Interpret {} argument", expr_idx))
            ))
        }
        call_alternatives_plan(plan, context, input)
    }
}

fn call_alternatives_plan(plan: NoInputPlan, context: InterpreterContextRef,
    input: InterpretedAtom) -> NoInputPlan {
    Box::new(SequencePlan::new(plan, OperatorPlan::new(move |results: Results| {
        make_alternives_plan(input, results, move |result| {
            call_plan(context.clone(), result)
        })
    }, "interpret each alternative")))
}

fn insert_reducted_arg_plan(expr: InterpretedAtom, atom_idx: usize) -> OperatorPlan<Results, Results> {
    let descr = format!("insert right element as child {} of left element", atom_idx);
    OperatorPlan::new(move |arg_variants| insert_reducted_arg_op(expr, atom_idx, arg_variants), descr)
}

fn insert_reducted_arg_op(expr: InterpretedAtom, atom_idx: usize, mut arg_variants: Results) -> StepResult<Results> {
    let result = arg_variants.drain(0..).map(|arg| {
        let InterpretedAtom(arg, bindings) = arg;
        let mut expr_with_arg = expr.atom().clone();
        get_expr_mut(&mut expr_with_arg).children_mut()[atom_idx] = arg;
        InterpretedAtom(apply_bindings_to_atom(&expr_with_arg, &bindings), bindings)
    }).collect();
    log::debug!("insert_reducted_arg_op: result: {:?}", result);
    StepResult::ret(result)
}

fn call_plan(context: InterpreterContextRef, input: InterpretedAtom) -> NoInputPlan {
    let descr = format!("call {}", input);
    Box::new(OperatorPlan::new(|_| call_op(context, input), descr))
}

fn call_op(context: InterpreterContextRef, input: InterpretedAtom) -> StepResult<Results> {
    log::debug!("call_op: {}", input);

    let cached = context.cache.borrow().get(input.atom(), input.bindings());
    if let Some(result) = cached {
        return_cached_result_plan(result)
    } else {
        if let Atom::Expression(_) = input.atom() {
            if !has_grounded_sub_expr(input.atom()) {
                let key = input.atom().clone();
                StepResult::execute(SequencePlan::new(
                    OrPlan::new(
                        interpret_reducted_plan(context.clone(), input.clone()),
                        StepResult::ret(vec![input])),
                    save_result_in_cache_plan(context, key)
                ))
            } else {
                StepResult::execute(OrPlan::new(
                        interpret_reducted_plan(context.clone(), input.clone()),
                        StepResult::ret(vec![input])))
            }
        } else {
            panic!("Only expressions are expected to be called");
        }
    }
}

fn return_cached_result_plan(results: Results) -> StepResult<Results> {
    let descr = format!("return cached results {:?}", results);
    StepResult::execute(OperatorPlan::new(|_| StepResult::ret(results), descr))
}

fn save_result_in_cache_plan(context: InterpreterContextRef, key: Atom) -> OperatorPlan<Results, Results> {
    let descr = format!("save results in cache for key {}", key);
    OperatorPlan::new(move |results: Results| {
        context.cache.borrow_mut().insert(key, results.clone());
        StepResult::ret(results)
    }, descr)
}

fn interpret_reducted_plan(context: InterpreterContextRef,
        input: InterpretedAtom) -> NoInputPlan {
    if let Atom::Expression(ref expr) = input.atom() {
        if is_grounded_op(expr) {
            Box::new(execute_plan(context, input))
        } else {
            Box::new(match_plan(context, input))
        }
    } else {
        panic!("Only expression is expected, received: {}", input);
    }
}


fn execute_plan(context: InterpreterContextRef, input: InterpretedAtom) -> OperatorPlan<(), Results> {
    let descr = format!("execute {}", input);
    OperatorPlan::new(|_| execute_op(context, input), descr)
}

fn execute_op(context: InterpreterContextRef, input: InterpretedAtom) -> StepResult<Results> {
    log::debug!("execute_op: {}", input);
    match input {
        InterpretedAtom(Atom::Expression(ref expr), ref bindings) => {
            let mut expr = expr.clone();
            let op = expr.children().get(0).cloned();
            if let Some(Atom::Grounded(op)) = op {
                let mut args = expr.children_mut().drain(1..).collect();
                match op.execute(&mut args) {
                    Ok(mut vec) => {
                        let results: Vec<InterpretedAtom> = vec.drain(0..)
                            .map(|atom| InterpretedAtom(atom, bindings.clone()))
                            .collect();
                        if results.is_empty() {
                            StepResult::ret(results)
                        } else {
                            make_alternives_plan(input, results, move |result| {
                                interpret_as_type_plan(context.clone(),
                                    result, ATOM_TYPE_UNDEFINED)
                            })
                        }
                    },
                    Err(ExecError::Runtime(msg)) => StepResult::ret(vec![InterpretedAtom(
                           Atom::expr([ERROR_SYMBOL, input.0, Atom::sym(msg)]), input.1)]),
                    Err(ExecError::NoReduce) => StepResult::err("Grounded operation is not reducible"),
                }
            } else {
                panic!("Trying to execute non grounded atom: {}", expr)
            }
        },
        _ => panic!("Unexpected non expression argument: {}", input),
    }
}

fn match_plan(context: InterpreterContextRef, input: InterpretedAtom) -> OperatorPlan<(), Results> {
    let descr = format!("match {}", input);
    OperatorPlan::new(|_| match_op(context, input), descr)
}

fn match_op(context: InterpreterContextRef, input: InterpretedAtom) -> StepResult<Results> {
    log::debug!("match_op: {}", input);
    let var_x = VariableAtom::new("%X%");
    // TODO: unique variable?
    let atom_x = Atom::Variable(var_x.clone());
    let query = Atom::expr(vec![EQUAL_SYMBOL, input.atom().clone(), atom_x]);
    let mut local_bindings = context.space.query(&query);
    let results: Vec<InterpretedAtom> = local_bindings
        .drain(0..)
        .map(|mut binding| {
            let result = binding.remove(&var_x).unwrap(); 
            let result = apply_bindings_to_atom(&result, &binding);
            // TODO: sometimes we apply bindings twice: first time here,
            // second time when inserting matched argument into nesting
            // expression.  It should be enough doing it only once.
            let bindings = apply_bindings_to_bindings(&binding, input.bindings());
            let bindings = bindings.map(|mut bindings| {
                binding.drain().for_each(|(k, v)| { bindings.insert(k, v); });
                bindings
            });
            log::debug!("match_op: query: {}, binding: {:?}, result: {}", input, bindings, result);
            (result, bindings)
        })
        .filter(|(_, bindings)| bindings.is_ok())
        .map(|(result, bindings)| InterpretedAtom(result, bindings.unwrap()))
        .collect();
    make_alternives_plan(input, results, move |result| {
        interpret_as_type_plan(context.clone(), result, ATOM_TYPE_UNDEFINED)
    })
}

fn make_alternives_plan<T, F, P>(input: InterpretedAtom, mut results: Vec<T>,
    plan: F) -> StepResult<Results>
where
    F: Fn(T) -> P,
    P: 'static + Plan<(), Results>
{
    match results.len() {
        0 => StepResult::err("No alternatives to interpret further"),
        1 => StepResult::execute(plan(results.pop().unwrap())),
        _ => {
            StepResult::execute(AlternativeInterpretationsPlan::new(
                input.0,
                results.drain(0..)
                    .map(|result| -> NoInputPlan { Box::new(plan(result)) })
                    .collect()))
        },
    }
}

use std::collections::VecDeque;

/// Plan which interprets in parallel alternatives of the expression.
/// Each successful result is appended to the overall result of the plan.
/// If no alternatives returned successful result the plan returns error. 
pub struct AlternativeInterpretationsPlan<T> {
    atom: Atom,
    plans: VecDeque<Box<dyn Plan<(), Vec<T>>>>,
    results: Vec<T>,
    success: bool,
}

impl<T> AlternativeInterpretationsPlan<T> {
    /// Create new instance of [AlternativeInterpretationsPlan]. 
    ///
    /// # Arguments
    /// `atom` - atom to be printed as root of the alternative interpretations
    /// `plan` - altenative plans for the atom
    pub fn new(atom: Atom, plans: Vec<Box<dyn Plan<(), Vec<T>>>>) -> Self {
        Self{ atom, plans: plans.into(), results: Vec::new(), success: false }
    }
}

impl<T: 'static + Debug> Plan<(), Vec<T>> for AlternativeInterpretationsPlan<T> {
    fn step(mut self: Box<Self>, _: ()) -> StepResult<Vec<T>> {
        if self.plans.len() == 0 {
            if self.success {
                StepResult::ret(self.results)
            } else {
                StepResult::err("No successful alternatives")
            }
        } else {
            let plan = self.plans.pop_front().unwrap();
            match plan.step(()) {
                StepResult::Execute(next) => {
                    self.plans.push_front(next);
                    StepResult::Execute(self)
                },
                StepResult::Return(mut result) => {
                    self.results.append(&mut result);
                    self.success = true;
                    StepResult::Execute(self)
                },
                StepResult::Error(message) => {
                    log::debug!("skip alternative because of error returned: {}", message);
                    StepResult::Execute(self)
                },
            }
        }
    }
}

impl<T: Debug> Debug for AlternativeInterpretationsPlan<T> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut res = write!(f, "interpret alternatives for {} (current results: {:?}):\n", self.atom, self.results);
        for (i, plan) in self.plans.iter().enumerate() {
            let plan_str = format!("{:?}", plan);
            let mut lines = plan_str.lines();
            res = res.and_then(|_| write!(f, "  {} {}\n",
                    if i == 0 { ">" } else { "-" }, lines.next().unwrap()));
            for line in lines {
                res = res.and_then(|_| write!(f, "    {}\n", line));
            }
        }
        res
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_match_all() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("color") "blue"));
        space.add(expr!("=" ("color") "red"));
        space.add(expr!("=" ("color") "green"));
        let expr = expr!(("color"));

        assert_eq!(interpret(space, &expr),
            Ok(vec![expr!("blue"), expr!("red"), expr!("green")]));
    }

    #[test]
    fn test_frog_reasoning() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("and" "True" "True") "True"));
        space.add(expr!("=" ("if" "True" then else) then));
        space.add(expr!("=" ("if" "False" then else) else));
        space.add(expr!("=" ("Fritz" "croaks") "True"));
        space.add(expr!("=" ("Fritz" "eats-flies") "True"));
        space.add(expr!("=" ("Tweety" "chirps") "True"));
        space.add(expr!("=" ("Tweety" "yellow") "True"));
        space.add(expr!("=" ("Tweety" "eats-flies") "True"));
        let expr = expr!("if" ("and" (x "croaks") (x "eats-flies"))
            ("=" (x "frog") "True") "nop");

        assert_eq!(interpret(space, &expr),
            Ok(vec![expr!("=" ("Fritz" "frog") "True")]));
    }

    fn results_are_equivalent(actual: &Result<Vec<Atom>, String>,
            expected: &Result<Vec<Atom>, String>) -> bool {
        match (actual, expected) {
            (Ok(actual), Ok(expected)) =>
                actual.len() == expected.len() &&
                actual.iter().zip(expected.iter()).all(|(actual, expected)| {
                    atoms_are_equivalent(actual, expected) }),
            (Err(actual), Err(expected)) => actual == expected,
            _ => false,
        }
    }

    #[test]
    fn test_variable_keeps_value_in_different_sub_expressions() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("eq" x x) "True"));
        space.add(expr!("=" ("plus" "Z" y) y));
        space.add(expr!("=" ("plus" ("S" k) y) ("S" ("plus" k y))));

        assert_eq!(interpret(space.clone(), &expr!("eq" ("plus" "Z" n) n)),
            Ok(vec![expr!("True")]));
        let actual = interpret(space.clone(), &expr!("eq" ("plus" ("S" "Z") n) n));
        let expected = Ok(vec![expr!("eq" ("S" y) y)]);
        assert!(results_are_equivalent(&actual, &expected),
            "actual: {:?} and expected: {:?} are not equivalent", actual, expected);
    }

    fn test_interpret<T, R, P: Plan<T, R>>(plan: P, arg: T) -> Result<R, String> {
        let mut step = Box::new(plan).step(arg);
        loop {
            match step {
                StepResult::Execute(plan) => step = plan.step(()),
                StepResult::Return(result) => return Ok(result),
                StepResult::Error(message) => return Err(message),
            }
        }
    }

    #[test]
    fn test_make_alternatives_plan_no_alternative() {
        let plan = make_alternives_plan(InterpretedAtom(sym!("Test"), Bindings::new()),
            vec![], |_res: InterpretedAtom| StepResult::ret(vec![]));

        let result = test_interpret(plan, ());

        assert_eq!(Err("No alternatives to interpret further".into()), result);
    }

    #[test]
    fn test_alternatives_plan_single_alternative() {
        let plan = AlternativeInterpretationsPlan::new(sym!("Test"),
            vec![Box::new(StepResult::ret(vec!["A", "B"]))]);

        let result = test_interpret(plan, ());

        assert_eq!(Ok(vec!["A", "B"]), result);
    }

    #[test]
    fn test_alternatives_plan_few_alternatives() {
        let plan = AlternativeInterpretationsPlan::new(sym!("Test"),
            vec![Box::new(StepResult::ret(vec!["A", "B"])),
                Box::new(StepResult::ret(vec!["C", "D"]))]);

        let result = test_interpret(plan, ());

        assert_eq!(Ok(vec!["A", "B", "C", "D"]), result);
    }

    #[test]
    fn test_alternatives_plan_error_present() {
        let plan = AlternativeInterpretationsPlan::new(sym!("Test"),
            vec![Box::new(StepResult::err("Expected error")),
                Box::new(StepResult::ret(vec!["C", "D"]))]);

        let result = test_interpret(plan, ());

        assert_eq!(Ok(vec!["C", "D"]), result);
    }

    #[test]
    fn test_alternatives_plan_only_errors() {
        let plan: AlternativeInterpretationsPlan<&'static str> =
            AlternativeInterpretationsPlan::new(sym!("Test"),
            vec![Box::new(StepResult::err("Expected error")),
                Box::new(StepResult::err("Another expected error"))]);

        let result = test_interpret(plan, ());

        assert_eq!(Err("No successful alternatives".into()), result);
    }

    #[test]
    fn test_variable_defined_via_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("if" "True" y) y));
        space.add(expr!("=" ("not" "False") "True"));
        space.add(expr!("=" ("a" z) ("not" ("b" z))));
        space.add(expr!("=" ("b" "d") "False"));
        let expr = expr!("if" ("a" x) x);

        assert_eq!(interpret(space, &expr), Ok(vec![expr!("d")]));
    }

    #[test]
    fn test_variable_name_conflict() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("a" (W)) {true}));
        let expr = expr!("a" W);

        assert_eq!(interpret(space, &expr), Ok(vec![expr!({true})]));
    }

    #[test]
    fn test_variable_name_conflict_renaming() {
        let space = metta_space("
            (= (b ($x $y)) (c $x $y))
        ");
        let expr = metta_atom("(a (b $a) $x $y)");

        let result = interpret(space, &expr);

        assert!(results_are_equivalent(&result,
            &Ok(vec![metta_atom("(a (c $a $b) $c $d)")])));
    }

    #[derive(PartialEq, Clone, Debug)]
    struct ThrowError();

    impl Grounded for ThrowError {
        fn type_(&self) -> Atom {
            expr!("->" "&str" "Error")
        }
        fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
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

    #[test]
    fn test_return_runtime_error_from_grounded_atom() {
        let space = GroundingSpace::new();
        let expr = Atom::expr([Atom::gnd(ThrowError()), Atom::value("Runtime test error")]);

        assert_eq!(interpret(space, &expr), 
            Ok(vec![Atom::expr([ERROR_SYMBOL, expr, Atom::sym("Runtime test error")])]));
    }

    #[derive(PartialEq, Clone, Debug)]
    struct NonReducible();

    impl Grounded for NonReducible {
        fn type_(&self) -> Atom {
            expr!("->" "&str" "u32")
        }
        fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
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

    #[test]
    fn test_execute_non_reducible_atom() {
        let space = GroundingSpace::new();
        let expr = Atom::expr([Atom::gnd(NonReducible()), Atom::value("32")]);

        assert_eq!(interpret(space, &expr), Ok(vec![expr]));
    }

    #[test]
    fn test_interpret_empty_expression() {
        let space = GroundingSpace::new();
        let expr = Atom::expr([]);

        assert_eq!(interpret(space, &expr), Ok(vec![expr]));
    }

    #[test]
    fn test_interpret_non_executable_grounded_atom() {
        let space = GroundingSpace::new();
        let expr = Atom::expr([Atom::value(1)]);

        assert_eq!(interpret(space, &expr), Ok(vec![expr]));
    }

    #[derive(PartialEq, Clone, Debug)]
    struct MulXUndefinedType(i32);

    impl Grounded for MulXUndefinedType {
        fn type_(&self) -> Atom {
            ATOM_TYPE_UNDEFINED
        }
        fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
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

    #[test]
    fn test_interpret_undefined_grounded_atom() {
        let space = GroundingSpace::new();
        let expr = expr!({MulXUndefinedType(3)} {2});

        assert_eq!(interpret(space, &expr), Ok(vec![Atom::value(6)]));
    }
}

