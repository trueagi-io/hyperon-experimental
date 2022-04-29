use crate::*;
use crate::common::plan::*;
use crate::atom::subexpr::*;
use crate::atom::matcher::*;
use crate::space::grounding::*;
use crate::common::collections::ListMap;

use std::ops::Deref;
use std::rc::Rc;
use std::cell::RefCell;

pub type InterpreterResult = Vec<(Atom, Bindings)>;

pub fn interpret_init(space: GroundingSpace, expr: &Atom) -> StepResult<InterpreterResult> {
    let context = InterpreterContextRef::new(space);
    StepResult::execute(interpret_plan(context, expr.clone(), Bindings::new()))
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

fn interpret_plan(context: InterpreterContextRef, atom: Atom, bindings: Bindings) -> OperatorPlan<(), InterpreterResult> {
    let descr = format!("interpret {}{}", atom, format_bindings(&bindings));
    OperatorPlan::new(|_| interpret_op(context, atom, bindings), descr)
}

fn interpret_op(context: InterpreterContextRef, atom: Atom, bindings: Bindings) -> StepResult<InterpreterResult> {
    log::debug!("interpret_op: {}, {}", atom, bindings);
    let atom = apply_bindings_to_atom(&atom, &bindings);
    let input = (atom, bindings);

    let cached = context.cache.borrow().get(&input.0, &input.1);
    if let Some(result) = cached {
        return_cached_result_plan(result)
    } else {
        if let Atom::Expression(_) = input.0 {
            if !has_grounded_sub_expr(&input.0) {
                StepResult::execute(SequencePlan::new(
                    OrPlan::new(interpret_expression_plan(context.clone(), input.clone()),
                        StepResult::ret(vec![input.clone()])),
                    save_result_in_cache_plan(context, input.0)
                ))
            } else {
                StepResult::execute(OrPlan::new(
                        interpret_expression_plan(context.clone(), input.clone()),
                        StepResult::ret(vec![input.clone()])))
            }
        } else {
            StepResult::ret(vec![input])
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

fn interpret_expression_plan(context: InterpreterContextRef, (atom, bindings): (Atom, Bindings)) -> Box<dyn Plan<(), InterpreterResult>> {
    match atom {
        Atom::Expression(ref expr) if expr.is_plain() => 
            interpret_reducted_plan(context,  atom, bindings),
        Atom::Expression(ref expr) if is_grounded(expr) => 
            reduct_args_plan(context, atom, bindings),
        Atom::Expression(_) => {
            Box::new(OrPlan::new(
                    match_plan(context.clone(), atom.clone(), bindings.clone()),
                    reduct_arg_by_arg_plan(context, atom, bindings)
            ))
        }
        _ => panic!("Only expression is expected, received: {}", atom),
    }
}

fn interpret_reducted_plan(context: InterpreterContextRef, atom: Atom, bindings: Bindings) -> Box<dyn Plan<(), InterpreterResult>> {
    let atom = apply_bindings_to_atom(&atom, &bindings);
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

fn reduct_arg_by_arg_plan(context: InterpreterContextRef, expr: Atom, bindings: Bindings) -> OperatorPlan<(), InterpreterResult> {
    let descr = format!("reduct expression arg by arg {}", expr);
    OperatorPlan::new(|_| reduct_arg_by_arg_op(context, expr, bindings), descr)
}

fn reduct_arg_by_arg_op(context: InterpreterContextRef, expr: Atom, bindings: Bindings) -> StepResult<InterpreterResult> {
    log::debug!("reduct_arg_by_arg_op: {}", expr);
    if let Atom::Expression(_) = expr {
        let iter = SubexprStream::from_expr(expr, BOTTOM_UP_DEPTH_WALK);
        try_reduct_next_arg_op(context, iter, bindings)
    } else {
        panic!("Atom::Expression is expected as an argument, found: {}", expr)
    }
}

fn try_reduct_next_arg_plan(context: InterpreterContextRef, iter: SubexprStream, bindings: Bindings) -> OperatorPlan<(), InterpreterResult> {
    let descr = format!("try reducting next arg in {:?}", iter);
    OperatorPlan::new(|_| try_reduct_next_arg_op(context, iter, bindings), descr)
}

fn try_reduct_next_arg_op(context: InterpreterContextRef, mut iter: SubexprStream, bindings: Bindings) -> StepResult<InterpreterResult> {
    if let Some(arg) = iter.next().cloned() {
        StepResult::execute(OrPlan::new(
                SequencePlan::new(
                    interpret_reducted_plan(context.clone(), arg, bindings.clone()),
                    replace_arg_and_interpret_plan(context.clone(), iter.clone())),
                try_reduct_next_arg_plan(context, iter, bindings)
        ))
    } else {
        StepResult::err("No results for reducted found")
    }
}

fn replace_arg_and_interpret_plan(context: InterpreterContextRef, iter: SubexprStream) -> OperatorPlan<InterpreterResult, InterpreterResult> {
    let descr = format!("interpret after reduction of {:?}", iter);
    OperatorPlan::new(|reduction_result| replace_arg_and_interpret_op(context, iter, reduction_result), descr)
}

fn replace_arg_and_interpret_op(context: InterpreterContextRef, iter: SubexprStream, mut reduction_result: InterpreterResult) -> StepResult<InterpreterResult> {
    log::debug!("replace_arg_and_interpret_op: reduction_result: {:?}", reduction_result);
    if reduction_result.is_empty() {
        //panic!("Unexpected empty result while reducting: {}, it should be either error or non-empty, full expression: {}", iter.get(), iter.as_atom());
        // TODO: Reducting next argument instead of panic allows creating
        // grounded atom NOP which is not reducted when met inside
        // expression but returns nothing when executed.
        StepResult::err("NOP special case")
    } else {
        let plan = reduction_result.drain(0..)
            .map(|(result, bindings)| -> Box<dyn Plan<(), InterpreterResult>> {
                let mut iter = iter.clone();
                *iter.get_mut() = result;
                Box::new(interpret_plan(context.clone(), iter.into_atom(), bindings))
            }).collect();
        StepResult::execute(AlternativeInterpretationsPlan::new(iter.into_atom(), plan))
    }
}

fn find_next_sibling_skip_last<'a>(levels: &mut Vec<usize>, expr: &'a ExpressionAtom, level: usize) -> Option<&'a Atom> {
    let mut idx = usize::wrapping_add(levels[level], 1);
    while idx < expr.children().len() - 1 {
        let child = &expr.children()[idx];
        if let Atom::Expression(_) = child {
            levels[level] = idx;
            log::trace!("find_next_sibling_expr: return: {}", child);
            return Some(child);
        }
        idx += 1;
    }
    levels.pop();
    log::trace!("find_next_sibling_expr: return None");
    return None;
}


fn reduct_args_plan(context: InterpreterContextRef, expr: Atom, bindings: Bindings) -> Box<dyn Plan<(), InterpreterResult>> {
    log::debug!("reduct_args_plan: {}", expr);
    if let Atom::Expression(ref e) = expr {
        // TODO: remove this hack when it is possible to use types in order
        // to prevent reducing of the last argument of the match
        let mut iter = if format!("{}", e.children()[0]) == "match" {
            log::trace!("skip reducing the last argument of the match");
            SubexprStream::from_expr(expr, find_next_sibling_skip_last)
        } else {
            SubexprStream::from_expr(expr, FIND_NEXT_SIBLING_WALK)
        };
        let sub = iter.next().expect("Non plain expression expected").clone();
        Box::new(SequencePlan::new(
                interpret_plan(context.clone(), sub, bindings),
                reduct_next_arg_plan(context, iter)
        ))
    } else {
        panic!("Only expression is expected, received: {}", expr);
    }
}

fn reduct_next_arg_plan(context: InterpreterContextRef, iter: SubexprStream) -> OperatorPlan<InterpreterResult, InterpreterResult> {
    let descr = format!("reduct next arg in {:?}", iter);
    OperatorPlan::new(|prev_result| reduct_next_arg_op(context, iter, prev_result), descr)
}

fn reduct_next_arg_op(context: InterpreterContextRef, iter: SubexprStream, mut prev_result: InterpreterResult) -> StepResult<InterpreterResult> {
    let plan = prev_result.drain(0..)
        .map(|(reducted, bindings)| {
            let mut iter = iter.clone();
            log::debug!("reduct_next_arg_op: reducted: {}, bindings: {:?}", reducted, bindings);
            *iter.get_mut() = reducted;
            log::debug!("reduct_next_arg_op: expression: {}", iter.as_atom());

            let next_sub = if let Some(next_sub) = iter.next().cloned() {
                log::debug!("reduct_next_arg_op: next_sub after reduction: {}", next_sub);
                Some(next_sub)
            } else { None };

            (next_sub, bindings, iter)
        })
        .map(|(next_sub, bindings, iter)| -> Box<dyn Plan<(), InterpreterResult>> {
            if let Some(next_sub) = next_sub {
                Box::new(SequencePlan::new(
                        interpret_plan(context.clone(), next_sub, bindings),
                        reduct_next_arg_plan(context.clone(), iter)
                ))
            } else {
                let expr = iter.into_atom();
                Box::new(interpret_reducted_plan(context.clone(), expr, bindings))
            }
        }).collect();
    StepResult::execute(AlternativeInterpretationsPlan::new(iter.into_atom().clone(), plan))
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
    let var_x = VariableAtom::from("%X%");
    // TODO: unique variable?
    let atom_x = Atom::Variable(var_x.clone());
    let mut local_bindings = context.space.query(&Atom::expr(&[Atom::sym("="), expr.clone(), atom_x]));
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

fn interpret_results_plan(context: InterpreterContextRef, atom: Atom, mut result: InterpreterResult) -> Box<dyn Plan<(), InterpreterResult>> {
    match result.len() {
        0 => Box::new(StepResult::ret(result)),
        1 => {
            let (result, binding) = result.pop().unwrap();
            Box::new(interpret_plan(context, result, binding))
        },
        _ => {
        Box::new(AlternativeInterpretationsPlan::new(atom,
                result.drain(0..).map(|(result, bindings)| -> Box<dyn Plan<(), InterpreterResult>> {
                    Box::new(interpret_plan(context.clone(), result, bindings))
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
}

impl<T> AlternativeInterpretationsPlan<T> {
    pub fn new(atom: Atom, plans: Vec<Box<dyn Plan<(), Vec<T>>>>) -> Self {
        Self{ atom, plans: plans.into(), results: Vec::new() }
    }
}

impl<T: 'static> Plan<(), Vec<T>> for AlternativeInterpretationsPlan<T> {
    fn step(mut self: Box<Self>, _: ()) -> StepResult<Vec<T>> {
        if self.plans.len() == 0 {
            StepResult::ret(self.results)
        } else {
            let plan = self.plans.pop_front().unwrap();
            match plan.step(()) {
                StepResult::Execute(next) => {
                    self.plans.push_front(next);
                    StepResult::Execute(self)
                },
                StepResult::Return(mut result) => {
                    self.results.append(&mut result);
                    StepResult::Execute(self)
                },
                StepResult::Error(message) => StepResult::Error(message),
            }
        }
    }
}

impl<T> Debug for AlternativeInterpretationsPlan<T> {  
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut res = write!(f, "interpret alternatives for {}:\n", self.atom);
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
}

