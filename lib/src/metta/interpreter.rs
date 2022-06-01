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

#[inline]
fn equal_symbol() -> Atom { sym!("=") }

pub type InterpreterResult = Vec<(Atom, Bindings)>;
type NoInputPlan = Box<dyn Plan<(), InterpreterResult>>;

pub fn interpret_init(space: GroundingSpace, expr: &Atom) -> StepResult<InterpreterResult> {
    let context = InterpreterContextRef::new(space);
    interpret_as_type_plan(context, (expr.clone(), Bindings::new()), AtomType::Undefined)
}

pub fn interpret_step(step: StepResult<InterpreterResult>) -> StepResult<InterpreterResult> {
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
        StepResult::Return(mut result) => Ok(result.drain(0..).map(|(atom, _)| atom).collect()),
        StepResult::Error(message) => Err(message),
        _ => panic!("Not expected step result: {:?}", step),
    }
}

// TODO: ListMap is not effective but we cannot use HashMap here without
// requiring hash functions for the grounded atoms.
struct InterpreterCache(ListMap<Atom, InterpreterResult>);

impl InterpreterCache {
    fn new() -> Self {
        Self(ListMap::new())
    }

    fn get(&self, key: &Atom, current_bindings: &Bindings) -> Option<InterpreterResult> {
        self.0.get(key).map(|res| -> Option<InterpreterResult> {
                let mut inconsistent = Vec::new();
                let mut result = Vec::new();
                for (atom, bindings) in res {
                    let merged = Bindings::merge(&bindings, &current_bindings);
                    if let Some(merged) = merged {
                        result.push((atom.clone(), merged));
                    } else {
                        inconsistent.push((atom, bindings));
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

    fn insert(&mut self, key: Atom, value: InterpreterResult) {
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

fn format_bindings(bindings: &Bindings) -> String {
    if bindings.is_empty() {
        "".into()
    } else {
        format!(", bindings {}", bindings)
    }
}

fn interpret_as_type_plan(context: InterpreterContextRef, (atom, bindings):
        (Atom, Bindings), typ: AtomType) -> StepResult<InterpreterResult> {
    log::debug!("interpret_as_type_plan: atom: {}, bindings: {}, type: {}", atom, bindings, typ);
    match atom {
        Atom::Symbol(_) | Atom::Grounded(_) =>
            cast_atom_to_type_plan(context, (atom, bindings), typ),

        Atom::Expression(ref expr) if expr.children().is_empty() =>
            cast_atom_to_type_plan(context, (atom, bindings), typ),

        Atom::Expression(ref expr) => {
            let op = &expr.children()[0];
            StepResult::execute(SequencePlan::new(
                    get_type_of_atom_plan(context.clone(), op.clone()),
                    interpret_expression_as_type_plan(context, (atom, bindings), typ)
            ))
        },

        Atom::Variable(_) => {
            StepResult::ret(vec![(atom, bindings)])
        },
    }
}

fn cast_atom_to_type_plan(context: InterpreterContextRef, (atom, bindings):
        (Atom, Bindings), typ: AtomType) -> StepResult<InterpreterResult> {
    // TODO: implement this via interpreting of the (:cast atom typ) expression
    if check_type(&context.space, &atom, &typ) {
        log::debug!("cast_atom_to_type_plan: atom: {} is casted to type: {}", atom, typ);
        StepResult::ret(vec![(atom, bindings)])
    } else {
        log::debug!("cast_atom_to_type_plan: atom: {} cannot be casted to type: {}", atom, typ);
        StepResult::err(format!("Incorrect type, atom: {}, type: {}", atom, typ))
    }
}

fn get_type_of_atom_plan(context: InterpreterContextRef, atom: Atom) -> StepResult<Vec<Atom>> {
    // TODO: implement this via interpreting of the (:? atom)
    StepResult::ret(get_reducted_types(&context.space, &atom))
}

fn interpret_expression_as_type_plan(context: InterpreterContextRef,
    (atom, bindings): (Atom, Bindings), typ: AtomType) ->
                       OperatorPlan<Vec<Atom>, InterpreterResult> {
    let descr = format!("form alternative plans for expression {}{} using types",
        atom, format_bindings(&bindings));
    OperatorPlan::new(move |mut op_types: Vec<Atom>| {
        let alts = op_types.drain(0..)
            .map(|op_typ| {
                interpret_expression_as_type_op(context.clone(),
                    (atom.clone(), bindings.clone()), op_typ, typ.clone())
            }).collect();
        StepResult::execute(AlternativeInterpretationsPlan::new(atom, alts))
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

// FIXME: replace pair (atom, bindings) by new type, redefine Display for it
fn interpret_expression_as_type_op(context: InterpreterContextRef,
    (atom, bindings): (Atom, Bindings), op_typ: Atom, ret_typ: AtomType) ->
                       NoInputPlan {
    log::debug!("interpret_expression_as_type_op: {}{}, operation type: {}, expected return type: {}",
        atom, format_bindings(&bindings), op_typ, ret_typ);
    let expr = get_expr(&atom);
    if ret_typ == AtomType::Specific(Atom::sym("Atom")) ||
            ret_typ == AtomType::Specific(Atom::sym("Expression")) {
        Box::new(StepResult::ret(vec![(atom, bindings)]))
    } else if is_func(&op_typ) {
        // FIXME: replace get_arg_types by decompose_op_type
        let (op_arg_types, op_ret_typ) = get_arg_types(&op_typ);
        // TODO: supertypes should be checked as well
        if !ret_typ.map_or(|typ| *op_ret_typ == *typ, true) {
            Box::new(StepResult::err(format!("Operation returns wrong type: {}, expected: {}", op_ret_typ, ret_typ)))
        } else if op_arg_types.len() != (expr.children().len() - 1) {
            Box::new(StepResult::err(format!("Operation arity is not equal to call arity: operation type, {}, call: {}", op_typ, expr)))
        } else {
            assert!(!expr.children().is_empty(), "Empty expression is not expected");
            let mut plan: NoInputPlan = Box::new(StepResult::ret(vec![(atom.clone(), bindings.clone())]));
            for expr_idx in 1..(expr.children().len()) {
                let arg = expr.children()[expr_idx].clone();
                let arg_typ = AtomType::Specific(op_arg_types[expr_idx - 1].clone());
                plan = Box::new(SequencePlan::new(
                    ParallelPlan::new(
                        plan,
                        interpret_as_type_plan(context.clone(), (arg, bindings.clone()), arg_typ)),
                    insert_reducted_arg_plan(expr_idx)
                ))
            }
            call_alternatives_plan(plan, context, (atom, bindings))
        }
    } else {
        let mut plan: NoInputPlan = Box::new(StepResult::ret(vec![(atom.clone(), bindings.clone())]));
        for expr_idx in 0..(expr.children().len()) {
            let arg = expr.children()[expr_idx].clone();
            plan = Box::new(SequencePlan::new(
                ParallelPlan::new(
                    plan,
                    interpret_as_type_plan(context.clone(), (arg, bindings.clone()), AtomType::Undefined)),
                insert_reducted_arg_plan(expr_idx)
            ))
        }
        call_alternatives_plan(plan, context, (atom, bindings))
    }
}

fn call_alternatives_plan(plan: NoInputPlan, context: InterpreterContextRef,
        (atom, bindings): (Atom, Bindings)) -> NoInputPlan {
    Box::new(SequencePlan::new(plan, OperatorPlan::new(
        move |mut results: InterpreterResult| {
            if !results.is_empty() {
                let alts = results.drain(0..).map(|result| -> NoInputPlan {
                    Box::new(call_plan(context.clone(), result))
                }).collect();
                StepResult::execute(AlternativeInterpretationsPlan::new(atom, alts))
            } else {
                StepResult::ret(vec![(atom, bindings)])
            }
        }, "interpret each alternative")
    ))
}

fn insert_reducted_arg_plan(atom_idx: usize) -> OperatorPlan<(InterpreterResult, InterpreterResult), InterpreterResult> {
    let descr = format!("insert right element as child {} of left element", atom_idx);
    OperatorPlan::new(move |prev_result| insert_reducted_arg_op(atom_idx, prev_result), descr)
}

fn insert_reducted_arg_op(atom_idx: usize, (mut atoms, arg): (InterpreterResult, InterpreterResult)) -> StepResult<InterpreterResult> {
    let result = atoms.drain(0..).flat_map(|(atom, atom_bindings)| {
        arg.iter().map(move |(arg, arg_bindings)| {
            let mut atom = atom.clone();
            get_expr_mut(&mut atom).children_mut()[atom_idx] = arg.clone();
            // TODO: after variables conflicts are fixed apply_bindings_to_bindings
            // call can be removed and only Bindings::merge should be left
            let applied_bindings = apply_bindings_to_bindings(&arg_bindings, &atom_bindings);
            if let Result::Ok(atom_bindings) = applied_bindings {
                Bindings::merge(&atom_bindings, &arg_bindings)
                    .map(|bindings| (apply_bindings_to_atom(&atom, &bindings), bindings))
            } else {
                log::debug!("insert_reducted_arg_op: skip bindings: {} which cannot be applied to atom bindings: {}, reason: {:?}",
                    arg_bindings, atom_bindings, applied_bindings);
                None
            }
        })
    }).filter(Option::is_some).map(Option::unwrap).collect();
    log::debug!("insert_reducted_arg_op: result: {:?}", result);
    StepResult::ret(result)
}

fn call_plan(context: InterpreterContextRef, (atom, bindings): (Atom, Bindings)) -> OperatorPlan<(), InterpreterResult> {
    let descr = format!("call {}{}", atom, format_bindings(&bindings));
    OperatorPlan::new(|_| call_op(context, (atom, bindings)), descr)
}

fn call_op(context: InterpreterContextRef, (atom, bindings): (Atom, Bindings)) -> StepResult<InterpreterResult> {
    log::debug!("call_op: {}, {}", atom, bindings);
    let input = (atom, bindings);

    let cached = context.cache.borrow().get(&input.0, &input.1);
    if let Some(result) = cached {
        return_cached_result_plan(result)
    } else {
        if let Atom::Expression(_) = input.0 {
            if !has_grounded_sub_expr(&input.0) {
                StepResult::execute(SequencePlan::new(
                    OrPlan::new(interpret_reducted_plan(context.clone(), input.clone()),
                        StepResult::ret(vec![input.clone()])),
                    save_result_in_cache_plan(context, input.0)
                ))
            } else {
                StepResult::execute(OrPlan::new(
                        interpret_reducted_plan(context.clone(), input.clone()),
                        StepResult::ret(vec![input.clone()])))
            }
        } else {
            panic!("Only expressions are expected to be called");
        }
    }
}

fn return_cached_result_plan(result: InterpreterResult) -> StepResult<InterpreterResult> {
    let descr = format!("return cached result {:?}", result);
    StepResult::execute(OperatorPlan::new(|_| StepResult::ret(result), descr))
}

fn save_result_in_cache_plan(context: InterpreterContextRef, key: Atom) -> OperatorPlan<InterpreterResult, InterpreterResult> {
    let descr = format!("save result in cache for key {}", key);
    OperatorPlan::new(move |result: InterpreterResult| {
        context.cache.borrow_mut().insert(key, result.clone());
        StepResult::ret(result)
    }, descr)
}

fn interpret_reducted_plan(context: InterpreterContextRef, (atom, bindings): (Atom, Bindings)) -> Box<dyn Plan<(), InterpreterResult>> {
    if let Atom::Expression(ref expr) = atom {
        if is_grounded(expr) {
            Box::new(execute_plan(context, atom, bindings))
        } else {
            Box::new(match_plan(context, atom, bindings))
        }
    } else {
        panic!("Only expression is expected, received: {}", atom);
    }
}


fn execute_plan(context: InterpreterContextRef, atom: Atom, bindings: Bindings) -> OperatorPlan<(), InterpreterResult> {
    let descr = format!("execute {}", atom);
    OperatorPlan::new(|_| execute_op(context, atom, bindings), descr)
}

fn execute_op(context: InterpreterContextRef, atom: Atom, bindings: Bindings) -> StepResult<InterpreterResult> {
    log::debug!("execute_op: {}", atom);
    if let Atom::Expression(mut expr) = atom.clone() {
        let op = expr.children().get(0).cloned();
        if let Some(Atom::Grounded(op)) = op {
            let mut args = expr.children_mut().drain(1..).collect();
            match op.execute(&mut args) {
                Ok(mut vec) => {
                    let results = vec.drain(0..).map(|atom| (atom, bindings.clone())).collect();
                    StepResult::execute(interpret_results_plan(context, atom, results))
                },
                Err(msg) => StepResult::err(msg),
            }
        } else {
            panic!("Trying to execute non grounded atom: {}", expr)
        }
    } else {
        panic!("Unexpected non expression argument: {}", atom)
    }
}

fn match_plan(context: InterpreterContextRef, expr: Atom, bindings: Bindings) -> OperatorPlan<(), InterpreterResult> {
    let descr = format!("match {}{}", expr, format_bindings(&bindings));
    OperatorPlan::new(|_| match_op(context, expr, bindings), descr)
}

fn match_op(context: InterpreterContextRef, expr: Atom, prev_bindings: Bindings) -> StepResult<InterpreterResult> {
    log::debug!("match_op: {}", expr);
    let var_x = VariableAtom::new("%X%");
    // TODO: unique variable?
    let atom_x = Atom::Variable(var_x.clone());
    let query = Atom::expr(vec![equal_symbol(), expr.clone(), atom_x]);
    let mut local_bindings = context.space.query(&query);
    let results: Vec<(Atom, Bindings)> = local_bindings
        .drain(0..)
        .map(|mut binding| {
            let result = binding.remove(&var_x).unwrap(); 
            let result = apply_bindings_to_atom(&result, &binding);
            let bindings = apply_bindings_to_bindings(&binding, &prev_bindings);
            let bindings = bindings.map(|mut bindings| {
                binding.drain().for_each(|(k, v)| { bindings.insert(k, v); });
                bindings
            });
            log::debug!("match_op: query: {}, binding: {:?}, result: {}", expr.clone(), bindings, result);
            (result, bindings)
        })
        .filter(|(_, bindings)| bindings.is_ok())
        .map(|(result, bindings)| (result, bindings.unwrap()))
        .collect();
    if results.is_empty() {
        StepResult::err("Match is not found")
    } else {
        StepResult::execute(interpret_results_plan(context, expr, results))
    }
}

// FIXME: this plan looks like pattern: get results from prev step, apply
// operation to each item and from instance of AlternativeInterpretationsPlan
fn interpret_results_plan(context: InterpreterContextRef, atom: Atom, mut result: InterpreterResult) -> Box<dyn Plan<(), InterpreterResult>> {
    match result.len() {
        0 => Box::new(StepResult::ret(result)),
        1 => {
            let (result, binding) = result.pop().unwrap();
            Box::new(interpret_as_type_plan(context, (result, binding), AtomType::Undefined))
        },
        _ => {
        Box::new(AlternativeInterpretationsPlan::new(atom,
                result.drain(0..).map(|(result, bindings)| -> Box<dyn Plan<(), InterpreterResult>> {
                    Box::new(interpret_as_type_plan(context.clone(), (result, bindings), AtomType::Undefined))
                }).collect()))
        },
    }
}

use std::fmt::{Debug, Formatter};
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

