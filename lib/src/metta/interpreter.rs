//! MeTTa interpreter implementation.
//!
//! # Algorithm
//!
//! For an atom and type on input (when type is not set `Undefined` is used):
//! * [Atom::Variable] is returned as is.
//! * [Atom::Symbol] and [Atom::Grounded] are type checked; each applicable
//!   type of the atom is checked vs expected type:
//!   * If type is correct then atom is returned as is.
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
//! * If one of previous steps returned error then original expression is
//!   returned. Otherwise the result of the interpretation is returned.
//!   It may be empty if one of expression is grounded expression which
//!   returns empty result.
//!
//! Summary on possible results:
//! * empty result for expression is returned when grounded operation
//!   returns empty result, or one of the arguments returned empty result
//! * error is returned when atom cannot be casted to the type expected
//!   or all alternative interpretations are errors; the overall result includes
//!   successfully interpreted alternatives only
//! * call of the expression returns either successful result or original expression

use crate::*;
use crate::common::plan::*;
use crate::atom::subexpr::*;
use crate::atom::matcher::*;
use crate::space::*;
use crate::common::collections::ListMap;
use crate::metta::*;
use crate::metta::types::{is_func, get_arg_types, get_type_bindings,
    get_atom_types, match_reducted_types};
use crate::common::ReplacingMapper;

use std::ops::Deref;
use std::rc::Rc;
use std::fmt::{Debug, Display, Formatter};

/// Wrapper, So the old interpreter can present the same public interface as the new intperpreter
pub struct InterpreterState<'a, T: SpaceRef<'a>> {
    step_result: StepResult<'a, Results, InterpreterError>,
    phantom: core::marker::PhantomData<T>
}

impl<'a, T: SpaceRef<'a>> InterpreterState<'a, T> {

    /// INTERNAL USE ONLY. Create an InterpreterState that is ready to yield results
    #[cfg(not(feature = "minimal"))]
    pub(crate) fn new_finished(_space: T, results: Vec<Atom>) -> Self {
        Self {
            step_result: StepResult::Return(results.into_iter().map(|atom| InterpretedAtom(atom, Bindings::new())).collect()),
            phantom: <_>::default(),
        }
    }

    pub fn has_next(&self) -> bool {
        self.step_result.has_next()
    }
    pub fn into_result(self) -> Result<Vec<Atom>, String> {
        match self.step_result {
            StepResult::Return(mut res) => {
                let res = res.drain(0..).map(|res| res.into_tuple().0).collect();
                Ok(res)
            },
            StepResult::Error((atom, err)) => Ok(vec![Atom::expr([ERROR_SYMBOL, atom, err])]),
            StepResult::Execute(_) => Err("Evaluation is not finished".into())
        }
    }
}

impl<'a, T: SpaceRef<'a>> Debug for InterpreterState<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.step_result, f)
    }
}

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
type InterpreterError = (Atom, Atom);
type NoInputPlan<'a> = Box<dyn Plan<'a, (), Results, InterpreterError> + 'a>;

/// Initialize interpreter and returns the result of the zero step.
/// It can be error, immediate result or interpretation plan to be executed.
/// See [crate::metta::interpreter] for algorithm explanation.
///
/// # Arguments
/// * `space` - atomspace to query for interpretation
/// * `expr` - atom to interpret
pub fn interpret_init<'a, T: Space + 'a>(space: T, expr: &Atom) -> InterpreterState<'a, T> {
    let step_result = interpret_init_internal(space, expr);
    InterpreterState { step_result: step_result, phantom: <_>::default() }
}

fn interpret_init_internal<'a, T: Space + 'a>(space: T, expr: &Atom) -> StepResult<'a, Results, InterpreterError> {
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
pub fn interpret_step<'a, T: Space + 'a>(step: InterpreterState<'a, T>) -> InterpreterState<'a, T> {
    log::debug!("current plan:\n{:?}", step);
    match step.step_result {
        StepResult::Execute(plan) => InterpreterState { step_result: plan.step(()), phantom: <_>::default() },
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
pub fn interpret<T: Space>(space: T, expr: &Atom) -> Result<Vec<Atom>, String> {
    let mut step = interpret_init(space, expr);
    while step.step_result.has_next() {
        step = interpret_step(step);
    }
    match step.step_result {
        StepResult::Return(mut result) => Ok(result.drain(0..)
            .map(|InterpretedAtom(atom, _)| atom).collect()),
        // TODO: return (Error atom err) expression
        StepResult::Error((atom, err)) => Ok(vec![Atom::expr([ERROR_SYMBOL, atom, err])]),
        _ => panic!("Not expected step result: {:?}", step),
    }
}

// TODO: ListMap is not effective but we cannot use HashMap here without
// requiring hash functions for the grounded atoms.
#[derive(Debug)]
struct InterpreterCache(ListMap<Atom, Results>);

impl InterpreterCache {
    fn new() -> Self {
        Self(ListMap::new())
    }

    fn get(&self, key: &Atom) -> Option<Results> {
        let mut var_mapper = ReplacingMapper::new(VariableAtom::make_unique);
        key.iter().filter_type::<&VariableAtom>()
            .for_each(|v| { var_mapper.mapping_mut().insert(v.clone(), v.clone()); });

        self.0.get(key).map(|results| {
            let mut var_mapper = var_mapper.clone();
            let mut result = Vec::new();
            for res in results {
                let mut atom = res.atom().clone();
                atom.iter_mut().filter_type::<&mut VariableAtom>()
                    .for_each(|var| var_mapper.replace(var));
                let bindings = res.bindings().clone().rename_vars(var_mapper.as_fn_mut());
                result.push(InterpretedAtom(atom, bindings));
            }
            result
        })
    }

    fn insert(&mut self, key: Atom, mut value: Results) {
        value.iter_mut().for_each(|res| {
            let vars = key.iter().filter_type::<&VariableAtom>().collect();
            res.0 = apply_bindings_to_atom(&res.0, &res.1);
            res.1.cleanup(&vars);
        });
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

use std::marker::PhantomData;

pub trait SpaceRef<'a> : Space + 'a {}
impl<'a, T: Space + 'a> SpaceRef<'a> for T {}

struct InterpreterContext<'a, T: SpaceRef<'a>> {
    space: T,
    cache: SpaceObserverRef<InterpreterCache>,
    phantom: PhantomData<&'a T>,
}

struct InterpreterContextRef<'a, T: SpaceRef<'a>>(Rc<InterpreterContext<'a, T>>);

impl<'a, T: SpaceRef<'a>> InterpreterContextRef<'a, T> {
    fn new(space: T) -> Self {
        let cache = space.common().register_observer(InterpreterCache::new());

        Self(Rc::new(InterpreterContext{ space, cache, phantom: PhantomData }))
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

fn interpret_as_type_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>,
        input: InterpretedAtom, typ: Atom) -> StepResult<'a, Results, InterpreterError> {
    log::debug!("interpret_as_type_plan: input: {}, type: {}", input, typ);
    match input.atom() {

        _ if typ == ATOM_TYPE_ATOM => StepResult::ret(vec![input]),
        Atom::Symbol(_) if typ == ATOM_TYPE_SYMBOL => StepResult::ret(vec![input]),
        Atom::Variable(_) if typ == ATOM_TYPE_VARIABLE => StepResult::ret(vec![input]),
        Atom::Expression(_) if typ == ATOM_TYPE_EXPRESSION => StepResult::ret(vec![input]),
        Atom::Grounded(_) if typ == ATOM_TYPE_GROUNDED => StepResult::ret(vec![input]),

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

fn cast_atom_to_type_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>,
        input: InterpretedAtom, typ: Atom) -> StepResult<'a, Results, InterpreterError> {
    // TODO: implement this via interpreting of the (:cast atom typ) expression
    let typ = apply_bindings_to_atom(&typ, input.bindings());
    let mut results = get_type_bindings(&context.space, input.atom(), &typ);
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
        StepResult::err((input.0, BAD_TYPE_SYMBOL))
    }
}

fn get_type_of_atom_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>, atom: Atom) -> StepResult<'a, Vec<Atom>, InterpreterError> {
    // TODO: implement this via interpreting of the (:? atom)
    StepResult::ret(get_atom_types(&context.space, &atom))
}

fn interpret_expression_as_type_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>,
        input: InterpretedAtom, typ: Atom) -> OperatorPlan<'a, Vec<Atom>, Results, InterpreterError> {
    let descr = format!("form alternative plans for expression {} using types", input);
    OperatorPlan::new(move |op_types: Vec<Atom>| {
        make_alternives_plan(input.0.clone(), op_types, move |op_typ| {
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

fn interpret_expression_as_type_op<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>,
        input: InterpretedAtom, op_typ: Atom, ret_typ: Atom) -> NoInputPlan<'a> {
    log::debug!("interpret_expression_as_type_op: input: {}, operation type: {}, expected return type: {}", input, op_typ, ret_typ);
    if ret_typ == ATOM_TYPE_ATOM || ret_typ == ATOM_TYPE_EXPRESSION {
        Box::new(StepResult::ret(vec![input]))
    } else if is_func(&op_typ) {
        let InterpretedAtom(input_atom, mut input_bindings) = input;
        let expr = get_expr(&input_atom);
        let (op_arg_types, op_ret_typ) = get_arg_types(&op_typ);
        // TODO: supertypes should be checked as well
        if !match_reducted_types(op_ret_typ, &ret_typ, &mut input_bindings) {
            Box::new(StepResult::err((input_atom, BAD_TYPE_SYMBOL)))
        } else if op_arg_types.len() != (expr.children().len() - 1) {
            Box::new(StepResult::err((input_atom, INCORRECT_NUMBER_OF_ARGUMENTS_SYMBOL)))
        } else {
            let input = InterpretedAtom(input_atom, input_bindings);
            let expr = get_expr(input.atom());
            assert!(!expr.children().is_empty(), "Empty expression is not expected");
            let mut plan: NoInputPlan = Box::new(StepResult::ret(vec![input.clone()]));
            for expr_idx in 0..(expr.children().len()) {
                let arg = expr.children()[expr_idx].clone();
                let arg_typ = if expr_idx > 0 {
                    op_arg_types[expr_idx - 1].clone()
                } else {
                    op_typ.clone()
                };
                let context = context.clone();
                plan = Box::new(SequencePlan::new(
                    plan,
                    OperatorPlan::new(move |results: Results| {
                        make_alternives_plan(arg.clone(), results, move |result| -> NoInputPlan {
                            let arg_typ = apply_bindings_to_atom(&arg_typ, result.bindings());
                            Box::new(SequencePlan::new(
                                interpret_as_type_plan(context.clone(),
                                    InterpretedAtom(arg.clone(), result.bindings().clone()),
                                    arg_typ),
                                insert_reducted_arg_plan(result, expr_idx)))
                        })
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
                OperatorPlan::new(move |results: Results| {
                    make_alternives_plan(arg.clone(), results, move |result| -> NoInputPlan {
                        Box::new(SequencePlan::new(
                            interpret_as_type_plan(context.clone(),
                                InterpretedAtom(arg.clone(), result.bindings().clone()),
                                ATOM_TYPE_UNDEFINED),
                            insert_reducted_arg_plan(result, expr_idx)))
                    })
                }, format!("Interpret {} argument", expr_idx))
            ))
        }
        call_alternatives_plan(plan, context, input)
    }
}

fn call_alternatives_plan<'a, T: SpaceRef<'a>>(plan: NoInputPlan<'a>, context: InterpreterContextRef<'a, T>,
    input: InterpretedAtom) -> NoInputPlan<'a> {
    Box::new(SequencePlan::new(plan, OperatorPlan::new(move |results: Results| {
        make_alternives_plan(input.0, results, move |result| {
            call_plan(context.clone(), result)
        })
    }, "interpret each alternative")))
}

fn insert_reducted_arg_plan<'a>(expr: InterpretedAtom, atom_idx: usize) -> OperatorPlan<'a, Results, Results, InterpreterError> {
    let descr = format!("insert right element as child {} of left element", atom_idx);
    OperatorPlan::new(move |arg_variants| insert_reducted_arg_op(expr, atom_idx, arg_variants), descr)
}

fn insert_reducted_arg_op<'a>(expr: InterpretedAtom, atom_idx: usize, mut arg_variants: Results) -> StepResult<'a, Results, InterpreterError> {
    let result = arg_variants.drain(0..).map(|arg| {
        let InterpretedAtom(arg, bindings) = arg;
        let mut expr_with_arg = expr.atom().clone();
        get_expr_mut(&mut expr_with_arg).children_mut()[atom_idx] = arg;
        InterpretedAtom(apply_bindings_to_atom(&expr_with_arg, &bindings), bindings)
    }).collect();
    log::debug!("insert_reducted_arg_op: result: {:?}", result);
    StepResult::ret(result)
}

fn call_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>, input: InterpretedAtom) -> NoInputPlan<'a> {
    let descr = format!("call {}", input);
    Box::new(OperatorPlan::new(|_| call_op(context, input), descr))
}

fn call_op<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>, input: InterpretedAtom) -> StepResult<'a, Results, InterpreterError> {
    log::debug!("call_op: {}", input);

    let cached = context.cache.borrow().get(input.atom());
    if let Some(result) = cached {
        let result = result.into_iter().flat_map(|InterpretedAtom(atom, bindings)| {
            bindings.merge_v2(input.bindings()).into_iter()
                .map(move |b| InterpretedAtom(atom.clone(), b))
        }).collect();
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

fn return_cached_result_plan<'a>(results: Results) -> StepResult<'a, Results, InterpreterError> {
    let descr = format!("return cached results {:?}", results);
    StepResult::execute(OperatorPlan::new(|_| StepResult::ret(results), descr))
}

fn save_result_in_cache_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>, key: Atom) -> OperatorPlan<'a, Results, Results, InterpreterError> {
    let descr = format!("save results in cache for key {}", key);
    OperatorPlan::new(move |results: Results| {
        context.cache.borrow_mut().insert(key, results.clone());
        StepResult::ret(results)
    }, descr)
}

fn interpret_reducted_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>,
        input: InterpretedAtom) -> NoInputPlan<'a> {
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


fn execute_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>, input: InterpretedAtom) -> OperatorPlan<'a, (), Results, InterpreterError> {
    let descr = format!("execute {}", input);
    OperatorPlan::new(|_| execute_op(context, input), descr)
}

fn execute_op<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>, input: InterpretedAtom) -> StepResult<'a, Results, InterpreterError> {
    log::debug!("execute_op: {}", input);
    match input {
        InterpretedAtom(Atom::Expression(ref expr), ref bindings) => {
            let op = expr.children().get(0);
            if let Some(Atom::Grounded(op)) = op {
                let args = expr.children();
                match op.execute(&args[1..]) {
                    Ok(mut vec) => {
                        let results: Vec<InterpretedAtom> = vec.drain(0..)
                            .map(|atom| InterpretedAtom(atom, bindings.clone()))
                            .collect();
                        if results.is_empty() {
                            StepResult::ret(results)
                        } else {
                            make_alternives_plan(input.0, results, move |result| {
                                interpret_as_type_plan(context.clone(),
                                    result, ATOM_TYPE_UNDEFINED)
                            })
                        }
                    },
                    Err(ExecError::Runtime(msg)) => StepResult::ret(vec![InterpretedAtom(
                           Atom::expr([ERROR_SYMBOL, input.0, Atom::sym(msg)]), input.1)]),
                    Err(ExecError::NoReduce) => StepResult::err((input.0, NOT_REDUCIBLE_SYMBOL)),
                }
            } else {
                panic!("Trying to execute non grounded atom: {}", expr)
            }
        },
        _ => panic!("Unexpected non expression argument: {}", input),
    }
}

fn match_plan<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>, input: InterpretedAtom) -> OperatorPlan<'a, (), Results, InterpreterError> {
    let descr = format!("match {}", input);
    OperatorPlan::new(|_| match_op(context, input), descr)
}

fn match_op<'a, T: SpaceRef<'a>>(context: InterpreterContextRef<'a, T>, input: InterpretedAtom) -> StepResult<'a, Results, InterpreterError> {
    log::debug!("match_op: {}", input);
    let var_x = VariableAtom::new("X").make_unique();
    let query = Atom::expr(vec![EQUAL_SYMBOL, input.atom().clone(), Atom::Variable(var_x.clone())]);
    let mut query_bindings = context.space.query(&query);
    let results: Vec<InterpretedAtom> = query_bindings
        .drain(0..)
        .map(|mut query_binding| {
            let result = query_binding.resolve_and_remove(&var_x).unwrap();
            let result = apply_bindings_to_atom(&result, &query_binding);
            // TODO: sometimes we apply bindings twice: first time here,
            // second time when inserting matched argument into nesting
            // expression.  It should be enough doing it only once.
            let bindings = apply_bindings_to_bindings(&query_binding, input.bindings());
            let bindings = bindings.and_then(|bindings| {
                Bindings::merge(&query_binding, &bindings).ok_or(())
            });
            log::debug!("match_op: query: {}, bindings: {:?}, result: {}", input, bindings, result);
            (result, bindings)
        })
        .filter(|(_, bindings)| bindings.is_ok())
        .map(|(result, bindings)| InterpretedAtom(result, bindings.unwrap()))
        .collect();
    make_alternives_plan(input.0, results, move |result| {
        interpret_as_type_plan(context.clone(), result, ATOM_TYPE_UNDEFINED)
    })
}

fn make_alternives_plan<'a, T: Debug, F, P>(input: Atom, mut results: Vec<T>,
    plan: F) -> StepResult<'a, Results, InterpreterError>
where
    F: 'a + Fn(T) -> P,
    P: 'a + Plan<'a, (), Results, InterpreterError>
{
    log::debug!("make_alternives_plan: input: {:?}, alternatives: {:?}", input, results);
    match results.len() {
        0 => StepResult::err((input, NO_VALID_ALTERNATIVES)),
        1 => StepResult::execute(plan(results.pop().unwrap())),
        _ => {
            StepResult::execute(AlternativeInterpretationsPlan::new(
                input,
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
pub struct AlternativeInterpretationsPlan<'a, T> {
    atom: Atom,
    plans: VecDeque<Box<dyn Plan<'a, (), Vec<T>, InterpreterError> + 'a>>,
    results: Vec<T>,
    success: bool,
}

impl<'a, T> AlternativeInterpretationsPlan<'a, T> {
    /// Create new instance of [AlternativeInterpretationsPlan].
    ///
    /// # Arguments
    /// `atom` - atom to be printed as root of the alternative interpretations
    /// `plan` - altenative plans for the atom
    pub fn new(atom: Atom, plans: Vec<Box<dyn Plan<'a, (), Vec<T>, InterpreterError> + 'a>>) -> Self {
        Self{ atom, plans: plans.into(), results: Vec::new(), success: false }
    }
}

impl<'a, T: Debug> Plan<'a, (), Vec<T>, InterpreterError> for AlternativeInterpretationsPlan<'a, T> {
    fn step(mut self: Box<Self>, _: ()) -> StepResult<'a, Vec<T>, InterpreterError> {
        log::debug!("AlternativeInterpretationsPlan::step: {} alternatives left", self.plans.len());
        if self.plans.len() == 0 {
            if self.success {
                StepResult::ret(self.results)
            } else {
                StepResult::err((self.atom, NO_VALID_ALTERNATIVES))
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
                StepResult::Error(err) => {
                    log::debug!("skip alternative because of error returned: {:?}", err);
                    StepResult::Execute(self)
                },
            }
        }
    }
}

impl<T: Debug> Debug for AlternativeInterpretationsPlan<'_, T> {
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

        assert_eq_no_order!(interpret(&space, &expr).unwrap(),
            vec![expr!("blue"), expr!("red"), expr!("green")]);
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

        assert_eq!(interpret(&space, &expr),
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

        assert_eq!(interpret(&space, &expr!("eq" ("plus" "Z" n) n)),
            Ok(vec![expr!("True")]));
        let actual = interpret(&space, &expr!("eq" ("plus" ("S" "Z") n) n));
        let expected = Ok(vec![expr!("eq" ("S" y) y)]);
        assert!(results_are_equivalent(&actual, &expected),
            "actual: {:?} and expected: {:?} are not equivalent", actual, expected);
    }

    fn test_interpret<'a, T, R: 'a, P: Plan<'a, T, R, InterpreterError> + 'a>(plan: P, arg: T) -> Result<R, InterpreterError> {
        let mut step = Box::new(plan).step(arg);
        loop {
            match step {
                StepResult::Execute(plan) => step = plan.step(()),
                StepResult::Return(result) => return Ok(result),
                StepResult::Error(err) => return Err(err),
            }
        }
    }

    #[test]
    fn test_make_alternatives_plan_no_alternative() {
        let plan = make_alternives_plan(sym!("Test"),
            vec![], |_res: InterpretedAtom| StepResult::ret(vec![]));

        let result = test_interpret(plan, ());

        assert_eq!(Err((sym!("Test"), NO_VALID_ALTERNATIVES)), result);
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
            vec![Box::new(StepResult::err((sym!("Test"), BAD_TYPE_SYMBOL))),
                Box::new(StepResult::ret(vec!["C", "D"]))]);

        let result = test_interpret(plan, ());

        assert_eq!(Ok(vec!["C", "D"]), result);
    }

    #[test]
    fn test_alternatives_plan_only_errors() {
        let plan: AlternativeInterpretationsPlan<&'static str> =
            AlternativeInterpretationsPlan::new(sym!("Test"),
            vec![Box::new(StepResult::err((sym!("Test"), sym!("Expected error")))),
                Box::new(StepResult::err((sym!("Test"), sym!("Another expected error"))))]);

        let result = test_interpret(plan, ());

        assert_eq!(Err((sym!("Test"), NO_VALID_ALTERNATIVES)), result);
    }

    #[test]
    fn test_variable_defined_via_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("if" "True" y) y));
        space.add(expr!("=" ("not" "False") "True"));
        space.add(expr!("=" ("a" z) ("not" ("b" z))));
        space.add(expr!("=" ("b" "d") "False"));
        let expr = expr!("if" ("a" x) x);

        assert_eq!(interpret(&space, &expr), Ok(vec![expr!("d")]));
    }

    #[test]
    fn test_variable_name_conflict() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("a" (W)) {true}));
        let expr = expr!("a" W);

        assert_eq!(interpret(&space, &expr), Ok(vec![expr!({true})]));
    }

    #[test]
    fn test_variable_name_conflict_renaming() {
        let space = metta_space("
            (= (b ($x $y)) (c $x $y))
        ");
        let expr = metta_atom("(a (b $a) $x $y)");

        let result = interpret(&space, &expr);

        assert!(results_are_equivalent(&result,
            &Ok(vec![metta_atom("(a (c $a $b) $c $d)")])));
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

    #[test]
    fn test_return_runtime_error_from_grounded_atom() {
        let space = GroundingSpace::new();
        let expr = Atom::expr([Atom::gnd(ThrowError()), Atom::value("Runtime test error")]);

        assert_eq!(interpret(&space, &expr),
            Ok(vec![Atom::expr([ERROR_SYMBOL, expr, Atom::sym("Runtime test error")])]));
    }

    #[derive(PartialEq, Clone, Debug)]
    struct NonReducible();

    impl Grounded for NonReducible {
        fn type_(&self) -> Atom {
            expr!("->" "&str" "u32")
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

    #[test]
    fn test_execute_non_reducible_atom() {
        let space = GroundingSpace::new();
        let expr = Atom::expr([Atom::gnd(NonReducible()), Atom::value("32")]);

        assert_eq!(interpret(&space, &expr), Ok(vec![expr]));
    }

    #[test]
    fn test_interpret_empty_expression() {
        let space = GroundingSpace::new();
        let expr = Atom::expr([]);

        assert_eq!(interpret(&space, &expr), Ok(vec![expr]));
    }

    #[test]
    fn test_interpret_non_executable_grounded_atom() {
        let space = GroundingSpace::new();
        let expr = Atom::expr([Atom::value(1)]);

        assert_eq!(interpret(&space, &expr), Ok(vec![expr]));
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

    #[test]
    fn test_interpret_undefined_grounded_atom() {
        let space = GroundingSpace::new();
        let expr = expr!({MulXUndefinedType(3)} {2});

        assert_eq!(interpret(&space, &expr), Ok(vec![Atom::value(6)]));
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
    fn return_bad_type_error() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "myAtom" "myType"));
        space.add(expr!(":" "id_a" ("->" "A" "A")));
        space.add(expr!("=" ("id_a" a) a));

        assert_eq!(interpret(&space, &expr!({ID_NUM} "myAtom")),
            Ok(vec![Atom::expr([ERROR_SYMBOL, sym!("myAtom"), BAD_TYPE_SYMBOL])]));
        assert_eq!(interpret(&space, &expr!("id_a" "myAtom")),
            Ok(vec![Atom::expr([ERROR_SYMBOL, sym!("myAtom"), BAD_TYPE_SYMBOL])]));
    }

    #[test]
    fn operation_is_expression() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "foo" ("->" ("->" "A" "A"))));
        space.add(expr!(":" "a" "A"));
        space.add(expr!("=" ("foo") "bar"));
        space.add(expr!("=" ("bar" x) x));

        assert_eq!(interpret(&space, &expr!(("foo") "a")), Ok(vec![expr!("a")]));
    }

    #[test]
    fn interpreter_cache_variables_are_not_changed_when_atom_was_not_transformed() {
        let mut cache = InterpreterCache::new();
        cache.insert(expr!("P" x), vec![InterpretedAtom(expr!("P" x), bind!{})]);
        assert_eq!(cache.get(&expr!("P" x)), Some(vec![InterpretedAtom(expr!("P" x), bind!{})]));
    }

    #[test]
    fn interpreter_cache_only_same_variables_are_matched() {
        let mut cache = InterpreterCache::new();
        cache.insert(expr!("P" x), vec![InterpretedAtom(expr!("P" x), bind!{})]);
        assert_eq!(cache.get(&expr!("P" y)), None);
    }

    #[test]
    fn interpreter_cache_variables_from_result_are_applied() {
        let mut cache = InterpreterCache::new();
        cache.insert(expr!("foo" "a"), vec![InterpretedAtom(expr!("P" x), bind!{ x: expr!("a") })]);
        assert_eq!(cache.get(&expr!("foo" "a")), Some(vec![InterpretedAtom(expr!("P" "a"), bind!{})]));
    }

    #[test]
    fn interpreter_cache_variables_from_key_are_kept_unique() {
        let mut cache = InterpreterCache::new();
        cache.insert(expr!("bar" x), vec![InterpretedAtom(expr!("P" x), bind!{})]);
        assert_eq!(cache.get(&expr!("bar" x)), Some(vec![InterpretedAtom(expr!("P" x), bind!{})]));
    }

    #[test]
    fn interpreter_cache_variables_absent_in_key_are_removed() {
        let mut cache = InterpreterCache::new();
        cache.insert(expr!("foo" x), vec![InterpretedAtom(expr!("bar"), bind!{ x: expr!("a"), y: expr!("Y") })]);
        assert_eq!(cache.get(&expr!("foo" x)), Some(vec![InterpretedAtom(expr!("bar"), bind!{ x: expr!("a") })]));
    }

    #[test]
    fn interpreter_cache_variables_from_result_becom_unique() {
        let mut cache = InterpreterCache::new();
        cache.insert(expr!(("bar")), vec![InterpretedAtom(expr!("P" x), bind!{})]);
        if let Some(results) = cache.get(&expr!(("bar"))) {
            assert_eq!(results.len(), 1);
            assert!(atoms_are_equivalent(results[0].atom(), &expr!("P" x)));
            assert_eq!(*results[0].bindings(), bind!{});
        } else {
            panic!("Non-empty result is expected");
        }
    }

    #[test]
    fn interpreter_cache_returns_variable_from_bindings() {
        let mut cache = InterpreterCache::new();
        cache.insert(expr!("bar" x), vec![InterpretedAtom(expr!(y), bind!{ x: expr!("A" y)})]);
        if let Some(mut results) = cache.get(&expr!("bar" x)) {
            let InterpretedAtom(atom, bindings) = results.pop().unwrap();
            let value = bindings.resolve(&VariableAtom::new("x")).unwrap();
            assert_eq!(Atom::expr([sym!("A"), atom]), value);
        } else {
            panic!("Non-empty result is expected");
        }
    }
}

