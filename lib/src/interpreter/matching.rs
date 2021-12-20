use super::*;
use crate::*;
use crate::matcher::*;

use std::rc::Rc;

// TODO: error handing and logging can also be moved into Plan structures
// or implemented atop of them
type InterpreterResult = Result<Vec<(Atom, Bindings)>, String>;

// Merge results:
// - if at least one result is Ok() it will be returned
// - if more than one result is Ok() the content of results is merged
// - if all results are error the Err("All results are error") is returned
fn merge_results(plan_res: InterpreterResult, step_res: InterpreterResult) -> InterpreterResult {
    match (step_res, plan_res) {
        (Ok(mut step_res), Ok(mut plan_res)) => {
            plan_res.append(&mut step_res);
            Ok(plan_res)
        },
        (Ok(step_res), Err(_)) => Ok(step_res),
        (Err(msg), Ok(vec)) if vec.is_empty() => {
            log::debug!("Skip result because of error: {}", msg);
            Err(format!("All results are error"))
        },
        (Err(msg), plan_res) => {
            log::debug!("Skip result because of error: {}", msg);
            plan_res
        },
    }
}

pub fn interpret(space: Rc<GroundingSpace>, expr: &Atom) -> Result<Vec<Atom>, String> {
    execute_plan(interpret_or_default_op, (space, expr.clone(), Bindings::new()))
        .map(|mut res| res.drain(0..).map(|(atom, _)| atom).collect())
}

fn is_grounded(expr: &ExpressionAtom) -> bool {
    matches!(expr.children().get(0), Some(Atom::Grounded(_)))
}

fn interpret_or_default_op((space, atom, bindings): (Rc<GroundingSpace>, Atom, Bindings)) -> StepResult<(), InterpreterResult> {
    log::debug!("interpret_or_default_op: {}", atom);
    let default = (atom.clone(), bindings.clone());
    StepResult::execute(SequencePlan::new(
        ApplyPlan::new(interpret_op, (space, atom, bindings)),
        PartialApplyPlan::new(return_default_if_err_op, default)
    ))
}

fn interpret_op((space, atom, bindings): (Rc<GroundingSpace>, Atom, Bindings)) -> StepResult<(), InterpreterResult> {
    let atom = apply_bindings_to_atom(&atom, &bindings);
    log::debug!("interpret_op: {}", atom);
    if let Atom::Expression(ref expr) = atom {
        if expr.is_plain() {
            StepResult::execute(ApplyPlan::new(interpret_reducted_op, (space,  atom, bindings)))
        } else {
            if is_grounded(expr) {
                StepResult::execute(ApplyPlan::new(reduct_args_op, (space,  atom, bindings)))
            } else {
                StepResult::execute(SequencePlan::new(
                    SequencePlan::new(
                        ApplyPlan::new(match_op, (Rc::clone(&space), atom.clone(), bindings.clone())),
                        PartialApplyPlan::new(reduct_args_if_not_matched, (Rc::clone(&space), atom.clone(), bindings.clone()))),
                    PartialApplyPlan::new(interpret_results_further_op, space),
                ))
            }
        }
    } else {
        StepResult::ret(Ok(vec![(atom, bindings)]))
    }
}

fn return_default_if_err_op(((atom, bindings), result): ((Atom, Bindings), InterpreterResult)) -> StepResult<(), InterpreterResult> {
    // Return original atom in case of error.
    // 
    // For grounded atoms it helps for the cases when part of symbolic expression contains
    // variables and cannot be executed before properly matched. But we can try to execute when
    // trying to match expression after reduction. See reduct_args_if_not_matched()
    //
    // For symbolic atoms it allows us considering them non-interpretable.
    match result {
        Err(msg) => {
            log::debug!("return_default_if_err_op: return original atom: {} with bindings: {:?}, because of error: {}", atom, bindings, msg);
            let atom = apply_bindings_to_atom(&atom, &bindings);
            StepResult::ret(Ok(vec![(atom, bindings)]))
        },
        Ok(_) => StepResult::ret(result),
    }
}

fn interpret_reducted_op((space, atom, bindings): (Rc<GroundingSpace>, Atom, Bindings)) -> StepResult<(), InterpreterResult> {
    let atom = apply_bindings_to_atom(&atom, &bindings);
    log::debug!("interpret_reducted_op: {}", atom);
    if let Atom::Expression(ref expr) = atom {
        if is_grounded(expr) {
            StepResult::execute(SequencePlan::new(
                ApplyPlan::new(execute_op, (atom.clone(), bindings.clone())),
                PartialApplyPlan::new(interpret_results_further_op, space)
            ))
        } else {
            StepResult::execute(SequencePlan::new(
                ApplyPlan::new(match_op, (Rc::clone(&space), atom.clone(), bindings.clone())),
                PartialApplyPlan::new(interpret_results_further_op, space)
            ))
        }
    } else {
        StepResult::ret(Err(format!("Expression is expected")))
    }
}

fn interpret_results_further_op((space, result): (Rc<GroundingSpace>, InterpreterResult)) -> StepResult<(), InterpreterResult> {
    match result {
        Err(_) => StepResult::ret(result),
        Ok(mut vec) => StepResult::Execute(
            // Start from empty vector, because empty result is not an error for
            // this operation. It should just process what was passed.
            vec.drain(0..).into_parallel_plan(Ok(vec![]),
            |(result, bindings)| Box::new(
                ApplyPlan::new(interpret_or_default_op, (Rc::clone(&space), result, bindings))),
                merge_results)),
    }
}

fn reduct_args_if_not_matched(((space, expr, bindings), match_result): ((Rc<GroundingSpace>, Atom, Bindings), InterpreterResult)) -> StepResult<(), InterpreterResult> {
    match match_result {
        Err(_) => {
            log::debug!("reduct_args_if_not_matched: match is not successful, trying to reduct");
            StepResult::execute(ApplyPlan::new(reduct_first_op, (space, expr, bindings)))
        },
        _ => StepResult::ret(match_result),
    }
}

fn reduct_first_op((space, expr, bindings): (Rc<GroundingSpace>, Atom, Bindings)) -> StepResult<(), InterpreterResult> {
    log::debug!("reduct_first_op: {}", expr);
    if let Atom::Expression(_) = expr {
        let mut iter = SubexprStream::from_expr(expr, BOTTOM_UP_DEPTH_WALK);
        let sub = iter.next().expect("Non plain expression expected").clone();
        StepResult::execute(SequencePlan::new(
                ApplyPlan::new(interpret_reducted_op, (Rc::clone(&space), sub, bindings.clone())),
                PartialApplyPlan::new(interpret_if_reducted_op, (space, iter, bindings))
        ))
    } else {
        StepResult::ret(Err(format!("Atom::Expression is expected as an argument, found: {}", expr)))
    }
}

fn interpret_if_reducted_op(((space, mut iter, bindings), reduction_result): ((Rc<GroundingSpace>, SubexprStream, Bindings), InterpreterResult)) -> StepResult<(), InterpreterResult> {
    log::debug!("interpret_if_reducted_op: reduction_result: {:?}", reduction_result);
    match reduction_result {
        Err(_) => {
            if let Some(next_sub) = iter.next().cloned() {
                log::debug!("interpret_if_reducted_op: trying to reduct next_sub: {}", next_sub);
                StepResult::execute(SequencePlan::new(
                        ApplyPlan::new(interpret_reducted_op, (Rc::clone(&space), next_sub, bindings.clone())),
                        PartialApplyPlan::new(interpret_if_reducted_op, (space, iter, bindings))
                ))
            } else {
                StepResult::ret(Err(format!("No results for reducted found")))
            }
        },
        Ok(mut vec) => {
            if vec.is_empty() {
                panic!("Unexpected empty result, it should be either error or non-empty, atom: {}", iter.as_atom());
            }
            let plan = vec.drain(0..)
                .into_parallel_plan(Ok(vec![]),
                    |(result, bindings)| {
                        let mut iter = iter.clone();
                        *iter.get_mut() = result;
                        Box::new(ApplyPlan::new(interpret_or_default_op, (Rc::clone(&space), iter.into_atom(), bindings)))
                    },
                    merge_results);
            StepResult::Execute(plan)
        },
    }
}

fn reduct_args_op((space, expr, bindings): (Rc<GroundingSpace>, Atom, Bindings)) -> StepResult<(), InterpreterResult> {
    log::debug!("reduct_args_op: {}", expr);
    if let Atom::Expression(_) = expr {
        let mut iter = SubexprStream::from_expr(expr, FIND_NEXT_SIBLING_WALK);
        let sub = iter.next().expect("Non plain expression expected").clone();
        StepResult::execute(SequencePlan::new(
                ApplyPlan::new(interpret_or_default_op, (Rc::clone(&space), sub, bindings)),
                PartialApplyPlan::new(reduct_next_arg_op, (space, iter))
        ))
    } else {
        StepResult::ret(Err(format!("Atom::Expression is expected as an argument, found: {}", expr)))
    }
}

fn reduct_next_arg_op(((space, iter), prev_result): ((Rc<GroundingSpace>, SubexprStream), InterpreterResult)) -> StepResult<(), InterpreterResult> {
    match prev_result {
        Err(_) => StepResult::ret(prev_result),
        Ok(mut results) => {
            let plan = results.drain(0..)
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
                .into_parallel_plan(Ok(vec![]),
                    |(next_sub, bindings, iter)| {
                        if let Some(next_sub) = next_sub {
                            Box::new(SequencePlan::new(
                                    ApplyPlan::new(interpret_or_default_op, (Rc::clone(&space), next_sub, bindings)),
                                    PartialApplyPlan::new(reduct_next_arg_op, (Rc::clone(&space), iter))
                            ))
                        } else {
                            let expr = iter.into_atom();
                            Box::new(ApplyPlan::new(interpret_reducted_op,
                                            (Rc::clone(&space), expr, bindings)))
                        }
                    },
                    merge_results);
            StepResult::Execute(plan)
        },
    }
}

fn execute_op((atom, bindings): (Atom, Bindings)) -> StepResult<(), InterpreterResult> {
    log::debug!("execute_op: {}", atom);
    if let Atom::Expression(mut expr) = atom {
        let op = expr.children().get(0).cloned();
        if let Some(Atom::Grounded(op)) = op {
            let mut args = expr.children_mut().drain(1..).collect();
            match op.execute(&mut args) {
                Ok(mut vec) => StepResult::ret(Ok(vec.drain(0..).map(|atom| (atom, bindings.clone())).collect())),
                Err(msg) => StepResult::ret(Err(msg)),
            }
        } else {
            StepResult::ret(Err(format!("Trying to execute non grounded atom: {}", expr)))
        }
    } else {
        StepResult::ret(Err(format!("Unexpected non expression argument: {}", atom)))
    }
}

fn match_op((space, expr, prev_bindings): (Rc<GroundingSpace>, Atom, Bindings)) -> StepResult<(), InterpreterResult> {
    log::debug!("match_op: {}", expr);
    let var_x = VariableAtom::from("X");
    // TODO: unique variable?
    let atom_x = Atom::Variable(var_x.clone());
    let mut local_bindings = space.query(&Atom::expr(&[Atom::sym("="), expr.clone(), atom_x]));
    let results: Vec<(Atom, Bindings)> = local_bindings
        .drain(0..)
        .map(|mut binding| {
            let result = binding.get(&var_x).unwrap(); 
            let result = apply_bindings_to_atom(result, &binding);
            let bindings = apply_bindings_to_bindings(&binding, &prev_bindings);
            let bindings = bindings.map(|mut bindings| {
                binding.drain().for_each(|(k, v)| { bindings.insert(k, v); });
                bindings
            });
            log::debug!("match_op: query: {}, binding: {:?}, result: {}", expr, bindings, result);
            (result, bindings)
        })
        .filter(|(_, bindings)| bindings.is_ok())
        .map(|(result, bindings)| (result, bindings.unwrap()))
        .collect();
    if results.is_empty() {
        StepResult::ret(Err(format!("Match is not found")))
    } else {
        StepResult::ret(Ok(results))
    }
}

fn unify_op((space, expr, prev_bindings): (Rc<GroundingSpace>, Atom, Bindings)) -> StepResult<(), InterpreterResult> {
    log::debug!("unify_op: {}", expr);
    let var_x = VariableAtom::from("X");
    // TODO: unique variable?
    let atom_x = Atom::Variable(var_x.clone());
    let mut unifications = space.unify(&Atom::expr(&[Atom::sym("="), expr.clone(), atom_x]));
    let mut results: Vec<(Atom, Bindings, Unifications)>  = unifications
        .drain(0..)
        .map(|(mut binding, unifications)| {
            let result = binding.get(&var_x).unwrap(); 
            let result = apply_bindings_to_atom(result, &binding);
            let bindings = apply_bindings_to_bindings(&binding, &prev_bindings);
            let bindings = bindings.map(|mut bindings| {
                binding.drain().for_each(|(k, v)| { bindings.insert(k, v); });
                bindings
            });
            log::debug!("unify_op: query: {}, binding: {:?}, result: {}", expr, bindings, result);
            (result, bindings, unifications)
        })
        .filter(|(_, bindings, _)| bindings.is_ok())
        .map(|(result, bindings, unifications)| (result, bindings.unwrap(), unifications))
        .collect();
    if results.is_empty() {
        StepResult::ret(Err(format!("Match is not found")))
    } else {
        let plan: Box<dyn Plan<(), InterpreterResult>>  = 
            results.drain(0..).into_parallel_plan(Ok(vec![]),
                |(result, bindings, mut unifications)| {
                    unifications.drain(0..).fold(
                        Box::new(|_: ()| StepResult::ret(Ok(vec![(result, bindings)]))),
                        |plan, pair| {
                            let candidate = pair.candidate;
                            let pattern = pair.pattern;
                            let space = Rc::clone(&space);
                            Box::new(SequencePlan::new(
                                plan,
                                move |step_result: InterpreterResult| match step_result {
                                    Err(_) => StepResult::ret(step_result),
                                    Ok(mut vec) => {
                                        StepResult::execute(vec.drain(0..).into_parallel_plan(Ok(vec![]),
                                            move |(result, bindings)| Box::new(
                                                StepResult::execute(ApplyPlan::new(interpret_op, (Rc::clone(&space), pattern.clone(), bindings.clone())))
                                                //StepResult::ret(Err(format!("Not implemented")))
                                            ),
                                            merge_results))
                                    },
                                },
                            ))
                        }
                    )
                },
                merge_results);
        StepResult::execute(plan)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;
    
    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_match_all() {
        init();
        let mut space = GroundingSpace::new();
        space.add(expr!("=", ("color"), "blue"));
        space.add(expr!("=", ("color"), "red"));
        space.add(expr!("=", ("color"), "green"));
        let expr = expr!(("color"));

        assert_eq!(interpret(Rc::new(space), &expr),
            Ok(vec![expr!("blue"), expr!("red"), expr!("green")]));
    }

    #[test]
    fn test_frog_reasoning() {
        init();
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

        assert_eq!(interpret(Rc::new(space), &expr),
            Ok(vec![expr!("=", ("Fritz", "frog"), "True")]));
    }

    #[test]
    fn test_variable_keeps_value_in_different_sub_expressions() {
        init();
        let mut space = GroundingSpace::new();
        space.add(expr!("=", ("eq", x, x), "True"));
        space.add(expr!("=", ("plus", "Z", y), y));
        space.add(expr!("=", ("plus", ("S", k), y), ("S", ("plus", k, y))));
        let space = Rc::new(space);

        assert_eq!(interpret(space.clone(), &expr!("eq", ("plus", "Z", n), n)),
            Ok(vec![expr!("True")]));
        assert_eq!(interpret(space.clone(), &expr!("eq", ("plus", ("S", "Z"), n), n)),
            Ok(vec![expr!("eq", ("S", y), y)]));
    }
}

