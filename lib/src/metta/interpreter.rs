use crate::*;
use crate::common::plan::*;
use crate::atom::subexpr::*;
use crate::atom::matcher::*;
use crate::space::grounding::*;
use crate::common::collections::ListMap;
use crate::metta::types::{AtomType, is_func, get_arg_types, check_type, get_reducted_types};

use std::ops::Deref;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};

#[inline]
fn equal_symbol() -> Atom { sym!("=") }


#[derive(Clone)]
pub struct InterpretedAtom(Atom, Bindings);

impl InterpretedAtom {
    fn atom(&self) -> &Atom {
        &self.0
    }

    fn bindings(&self) -> &Bindings {
        &self.1
    }
    
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

pub fn interpret_init(space: GroundingSpace, expr: &Atom) -> StepResult<Vec<InterpretedAtom>> {
    let context = InterpreterContextRef::new(space);
    interpret_as_type_plan(context,
        InterpretedAtom(expr.clone(), Bindings::new()),
        AtomType::Undefined)
}

pub fn interpret_step(step: StepResult<Vec<InterpretedAtom>>) -> StepResult<Vec<InterpretedAtom>> {
    log::debug!("current plan:\n{:?}", step);
    match step {
        StepResult::Execute(plan) => plan.step(()),
        StepResult::Return(_) => panic!("Plan execution is finished already"),
        StepResult::Error(_) => panic!("Plan execution is finished with error"),
    }
}

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

fn is_grounded(expr: &ExpressionAtom) -> bool {
    matches!(expr.children().get(0), Some(Atom::Grounded(_)))
}

fn has_grounded_sub_expr(expr: &Atom) -> bool {
    return SubexprStream::from_expr(expr.clone(), TOP_DOWN_DEPTH_WALK)
        .any(|sub| if let Atom::Expression(sub) = sub {
            is_grounded(&sub)
        } else {
            panic!("Expression is expected");
        });
}

fn interpret_as_type_plan(context: InterpreterContextRef,
        input: InterpretedAtom, typ: AtomType) -> StepResult<Results> {
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
        input: InterpretedAtom, typ: AtomType) -> StepResult<Results> {
    // TODO: implement this via interpreting of the (:cast atom typ) expression
    if check_type(&context.space, input.atom(), &typ) {
        log::debug!("cast_atom_to_type_plan: input: {} is casted to type: {}", input, typ);
        StepResult::ret(vec![input])
    } else {
        log::debug!("cast_atom_to_type_plan: input: {} cannot be casted to type: {}", input, typ);
        StepResult::err(format!("Incorrect type, input: {}, type: {}", input, typ))
    }
}

fn get_type_of_atom_plan(context: InterpreterContextRef, atom: Atom) -> StepResult<Vec<Atom>> {
    // TODO: implement this via interpreting of the (:? atom)
    StepResult::ret(get_reducted_types(&context.space, &atom))
}

fn interpret_expression_as_type_plan(context: InterpreterContextRef,
        input: InterpretedAtom, typ: AtomType) -> OperatorPlan<Vec<Atom>, Results> {
    let descr = format!("form alternative plans for expression {} using types", input);
    OperatorPlan::new(move |mut op_types: Vec<Atom>| {
        let alts = op_types.drain(0..)
            .map(|op_typ| {
                interpret_expression_as_type_op(context.clone(),
                    input.clone(), op_typ, typ.clone())
            }).collect();
        StepResult::execute(AlternativeInterpretationsPlan::new(input.0, alts))
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
        input: InterpretedAtom, op_typ: Atom, ret_typ: AtomType) -> NoInputPlan {
    log::debug!("interpret_expression_as_type_op: input: {}, operation type: {}, expected return type: {}", input, op_typ, ret_typ);
    let expr = get_expr(input.atom());
    if ret_typ == AtomType::Specific(Atom::sym("Atom")) ||
            ret_typ == AtomType::Specific(Atom::sym("Expression")) {
        Box::new(StepResult::ret(vec![input]))
    } else if is_func(&op_typ) {
        let (op_arg_types, op_ret_typ) = get_arg_types(&op_typ);
        // TODO: supertypes should be checked as well
        if !ret_typ.map_or(|typ| *op_ret_typ == *typ, true) {
            Box::new(StepResult::err(format!("Operation returns wrong type: {}, expected: {}", op_ret_typ, ret_typ)))
        } else if op_arg_types.len() != (expr.children().len() - 1) {
            Box::new(StepResult::err(format!("Operation arity is not equal to call arity: operation type, {}, call: {}", op_typ, expr)))
        } else {
            assert!(!expr.children().is_empty(), "Empty expression is not expected");
            let mut plan: NoInputPlan = Box::new(StepResult::ret(vec![input.clone()]));
            for expr_idx in 1..(expr.children().len()) {
                let arg = expr.children()[expr_idx].clone();
                let arg_typ = AtomType::Specific(op_arg_types[expr_idx - 1].clone());
                plan = Box::new(SequencePlan::new(
                    ParallelPlan::new(
                        plan,
                        interpret_as_type_plan(context.clone(),
                            InterpretedAtom(arg, input.bindings().clone()),
                            arg_typ)),
                    insert_reducted_arg_plan(expr_idx)
                ))
            }
            call_alternatives_plan(plan, context, input)
        }
    } else {
        let mut plan: NoInputPlan = Box::new(StepResult::ret(vec![input.clone()]));
        for expr_idx in 0..(expr.children().len()) {
            let arg = expr.children()[expr_idx].clone();
            plan = Box::new(SequencePlan::new(
                ParallelPlan::new(
                    plan,
                    interpret_as_type_plan(context.clone(),
                        InterpretedAtom(arg, input.bindings().clone()),
                        AtomType::Undefined)),
                insert_reducted_arg_plan(expr_idx)
            ))
        }
        call_alternatives_plan(plan, context, input)
    }
}

fn call_alternatives_plan(plan: NoInputPlan, context: InterpreterContextRef,
        input: InterpretedAtom) -> NoInputPlan {
    Box::new(SequencePlan::new(plan, OperatorPlan::new(
        move |mut results: Results| {
            if !results.is_empty() {
                let alts = results.drain(0..).map(|result| -> NoInputPlan {
                    Box::new(call_plan(context.clone(), result))
                }).collect();
                StepResult::execute(AlternativeInterpretationsPlan::new(input.0, alts))
            } else {
                StepResult::ret(vec![input])
            }
        }, "interpret each alternative")
    ))
}

fn insert_reducted_arg_plan(atom_idx: usize) -> OperatorPlan<(Results, Results), Results> {
    let descr = format!("insert right element as child {} of left element", atom_idx);
    OperatorPlan::new(move |prev_result| insert_reducted_arg_op(atom_idx, prev_result), descr)
}

fn insert_reducted_arg_op(atom_idx: usize, (mut atoms, args): (Results, Results)) -> StepResult<Results> {
    let result = atoms.drain(0..).flat_map(|interpreted_atom| {
        args.iter().map(move |arg| {
            let mut atom = interpreted_atom.atom().clone();
            get_expr_mut(&mut atom).children_mut()[atom_idx] = arg.atom().clone();
            // TODO: after variables conflicts are fixed apply_bindings_to_bindings
            // call can be removed and only Bindings::merge should be left
            let applied_bindings = apply_bindings_to_bindings(arg.bindings(),
                interpreted_atom.bindings());
            if let Result::Ok(atom_bindings) = applied_bindings {
                Bindings::merge(&atom_bindings, arg.bindings())
                    .map(|bindings| InterpretedAtom(apply_bindings_to_atom(&atom, &bindings), bindings))
            } else {
                log::debug!("insert_reducted_arg_op: skip bindings: {} which cannot be applied to atom bindings: {}, reason: {:?}",
                    arg.bindings(), interpreted_atom.bindings(), applied_bindings);
                None
            }
        })
    }).filter(Option::is_some).map(Option::unwrap).collect();
    log::debug!("insert_reducted_arg_op: result: {:?}", result);
    StepResult::ret(result)
}

fn call_plan(context: InterpreterContextRef, input: InterpretedAtom) -> OperatorPlan<(), Results> {
    let descr = format!("call {}", input);
    OperatorPlan::new(|_| call_op(context, input), descr)
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
        input: InterpretedAtom) -> Box<dyn Plan<(), Results>> {
    if let Atom::Expression(ref expr) = input.atom() {
        if is_grounded(expr) {
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
                        let results = vec.drain(0..)
                            .map(|atom| InterpretedAtom(atom, bindings.clone()))
                            .collect();
                        StepResult::execute(interpret_results_plan(context,
                            input, results))
                    },
                    Err(msg) => StepResult::err(msg),
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
    let query = Atom::expr(vec![equal_symbol(), input.atom().clone(), atom_x]);
    let mut local_bindings = context.space.query(&query);
    let results: Vec<InterpretedAtom> = local_bindings
        .drain(0..)
        .map(|mut binding| {
            let result = binding.remove(&var_x).unwrap(); 
            let result = apply_bindings_to_atom(&result, &binding);
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
    if results.is_empty() {
        StepResult::err("Match is not found")
    } else {
        StepResult::execute(interpret_results_plan(context, input, results))
    }
}

// FIXME: this plan looks like pattern: get results from prev step, apply
// operation to each item and from instance of AlternativeInterpretationsPlan
fn interpret_results_plan(context: InterpreterContextRef, input: InterpretedAtom,
        mut result: Results) -> Box<dyn Plan<(), Results>> {
    match result.len() {
        0 => Box::new(StepResult::ret(result)),
        1 => {
            let result = result.pop().unwrap();
            Box::new(interpret_as_type_plan(context, result, AtomType::Undefined))
        },
        _ => {
        Box::new(AlternativeInterpretationsPlan::new(input.0,
                result.drain(0..).map(|result| -> Box<dyn Plan<(), Results>> {
                    Box::new(interpret_as_type_plan(context.clone(), result, AtomType::Undefined))
                }).collect()))
        },
    }
}

use std::collections::VecDeque;

pub struct AlternativeInterpretationsPlan<T> {
    atom: Atom,
    plans: VecDeque<Box<dyn Plan<(), Vec<T>>>>,
    results: Vec<T>,
    success: bool,
}

impl<T> AlternativeInterpretationsPlan<T> {
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
        space.add(expr!("=", ("color"), "blue"));
        space.add(expr!("=", ("color"), "red"));
        space.add(expr!("=", ("color"), "green"));
        let expr = expr!(("color"));

        assert_eq!(interpret(space, &expr),
            Ok(vec![expr!("blue"), expr!("red"), expr!("green")]));
    }

    #[test]
    fn test_frog_reasoning() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=", ("and", "True", "True"), "True"));
        space.add(expr!("=", ("if", "True", then, else), then));
        space.add(expr!("=", ("if", "False", then, else), else));
        space.add(expr!("=", ("Fritz", "croaks"), "True"));
        space.add(expr!("=", ("Fritz", "eats-flies"), "True"));
        space.add(expr!("=", ("Tweety", "chirps"), "True"));
        space.add(expr!("=", ("Tweety", "yellow"), "True"));
        space.add(expr!("=", ("Tweety", "eats-flies"), "True"));
        let expr = expr!("if", ("and", (x, "croaks"), (x, "eats-flies")),
            ("=", (x, "frog"), "True"), "nop");

        assert_eq!(interpret(space, &expr),
            Ok(vec![expr!("=", ("Fritz", "frog"), "True")]));
    }

    #[test]
    fn test_variable_keeps_value_in_different_sub_expressions() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=", ("eq", x, x), "True"));
        space.add(expr!("=", ("plus", "Z", y), y));
        space.add(expr!("=", ("plus", ("S", k), y), ("S", ("plus", k, y))));

        assert_eq!(interpret(space.clone(), &expr!("eq", ("plus", "Z", n), n)),
            Ok(vec![expr!("True")]));
        assert_eq!(interpret(space.clone(), &expr!("eq", ("plus", ("S", "Z"), n), n)),
            Ok(vec![expr!("eq", ("S", y), y)]));
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

    // FIXME: add unit test on variable x(1) is bounded to variable x(2) then
    // new binding is found for variable x(2).
}

