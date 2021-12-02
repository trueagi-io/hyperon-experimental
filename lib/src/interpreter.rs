use crate::*;
use crate::matcher::*;

use std::rc::Rc;

pub fn interpret(space: Rc<GroundingSpace>, expr: &Atom) -> InterpreterResult {
    let mut interpreter = Interpreter::new(space, expr);
    // see https://github.com/rust-lang/rust/issues/86654
    interpreter.push(interpret_op as StepFunc);
    
    loop {
        let result = interpreter.step();
        match result {
            Some(result) => {
                return Ok(result);
            },
            _ => continue,
        }
    }
}

fn is_grounded(expr: &ExpressionAtom) -> bool {
    matches!(expr.children().get(0), Some(Atom::Grounded(_)))
}

type InterpreterResult = Result<Vec<Atom>, String>;

trait Step {
    fn call(&mut self, data: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult;
    fn clone_step(&self) -> Box<dyn Step>;
}

type StepFunc = fn(InterpreterResult, &mut Interpreter) -> InterpreterResult;

impl Step for StepFunc {
    fn call(&mut self, data: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
        self(data, interpreter)
    }
    fn clone_step(&self) -> Box<dyn Step> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Step> {
    fn clone(&self) -> Self {
        self.clone_step()
    }
}

#[derive(Clone)]
struct Branch {
    result: InterpreterResult,
    steps: Vec<Box<dyn Step>>,
    bindings_stack: Vec<Bindings>,
    space: Rc<GroundingSpace>,
}

fn result_cloned<T: Clone, E: Clone>(src: &Result<T, E>) -> Result<T, E> {
    let src = src.as_ref();
    if src.is_err() {
        Err(src.err().unwrap().clone())
    } else {
        Ok(src.ok().unwrap().clone())
    }
}

impl Branch {

    fn new(space: Rc<GroundingSpace>, expr: &Atom) -> Self {
        Self {
            result: Ok(vec![expr.clone()]),
            steps: Vec::new(),
            bindings_stack: Vec::new(),
            space: space,
        }
    }

    fn push<T: 'static + Step>(&mut self, step: T) {
        self.steps.push(Box::new(step));
    }

    fn bindings_mut_ref(&mut self) -> &mut Vec<Bindings> {
        &mut self.bindings_stack
    }
}

struct Interpreter {
    branches: Vec<Branch>,
    working: usize,
    results: Vec<Atom>,
}

impl Interpreter {

    fn new(space: Rc<GroundingSpace>, expr: &Atom) -> Self {
        Self {
            branches: vec![Branch::new(space, expr)],
            working: 0,
            results: vec![],
        }
    }

    fn working_branch(&mut self) -> &mut Branch {
        &mut self.branches[self.working]
    }

    fn push<T: 'static + Step>(&mut self, step: T) {
        self.working_branch().push(step);
    }

    fn branch(&mut self, result: InterpreterResult) -> &mut Branch {
        let mut branch = self.working_branch().clone();
        branch.result = result;
        self.branches.push(branch);
        self.branches.last_mut().unwrap()
    }

    fn step(&mut self) -> Option<Vec<Atom>> {
        if self.branches.is_empty() {
            Some(self.results.clone())
        } else {
            let idx = self.branches.len() - 1;
            self.working = idx;
            if self.branches[idx].steps.is_empty() {
                let branch = self.branches.pop().unwrap();
                if let Ok(mut result) = branch.result {
                    result.drain(0..).for_each(|atom| self.results.push(atom))
                }
                None
            } else {
                let mut next = self.branches[idx].steps.pop().unwrap();
                self.branches[idx].result = next.call(result_cloned(&self.branches[idx].result), self);
                None
            }
        }
    }

    fn bindings_mut_ref(&mut self) -> &mut Vec<Bindings> {
        self.working_branch().bindings_mut_ref()
    }
}

fn apply_bindings_stack_to_atom(atom: &Atom, bindings_stack: &Vec<Bindings>) -> Atom {
    let mut atom = atom.clone();
    for binding in bindings_stack {
        atom = apply_bindings_to_atom(&atom, binding);
    }
    atom
}

fn interpret_op(args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 1 {
        Err(format!("Expected Atom as argument, found: {:?}", args))
    } else {
        let atom = args[0].clone();
        log::debug!("interpret_op({})", atom);
        if let Some(ref expr) = atom.as_expr() {
            if !expr.is_plain() {
                interpreter.push(reduct as StepFunc);
                Ok(vec![atom])
            } else {
                interpreter.push(interpret_reducted_op as StepFunc);
                Ok(vec![atom])
            }
        } else {
            Ok(vec![atom])
        }
    }
}

fn interpret_reducted_op(args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 1 {
        Err(format!("Expected Atom as argument, found: {:?}", args))
    } else {
        let atom = &args[0];
        let atom = apply_bindings_stack_to_atom(atom, interpreter.bindings_mut_ref());
        log::debug!("interpret_reducted_op({})", atom);
        if let Some(ref expr) = atom.as_expr() {
            if is_grounded(expr) {
                interpreter.push(execute as StepFunc);
                Ok(vec![atom])
            } else {
                interpreter.push(match_op as StepFunc);
                Ok(vec![atom])
            }
        } else {
            Ok(vec![atom])
        }
    }
}

#[derive(Clone)]
struct ReductStep {
    iter: SubexpressionStream,
}

impl ReductStep {
    fn new(iter: SubexpressionStream) -> Self {
        Self { iter }
    }
}

impl Step for ReductStep {
    fn call(&mut self, data: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
        reduct_next(self.iter.clone(), data, interpreter)
    }

    fn clone_step(&self) -> Box<dyn Step> {
        Box::new((*self).clone())
    }
}

fn reduct(args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 1 {
        Err(format!("1 argument is expected, found: {:?}", args))
    } else {
        let expr_atom = args[0].clone();
        log::debug!("reduct({})", expr_atom);
        if let Some(ref expr) = expr_atom.as_expr() {
            if expr.is_plain() {
                interpreter.push(interpret_reducted_op as StepFunc);
                Ok(vec![expr_atom])
            } else {
                let mut iter = expr.sub_expr_iter();
                let sub;
                {
                    iter.next();
                    sub = iter.get_mut().clone();
                }
                interpreter.push(ReductStep::new(iter));
                interpreter.push(interpret_reducted_op as StepFunc);
                Ok(vec![sub])
            }
        } else {
            Err(format!("Atom::Expression is expected as an argument, found: {}", expr_atom))
        }
    }
}

fn reduct_next(mut iter: SubexpressionStream, args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 1 {
        Err(format!("Intepreted subexpression is expected as an argument, found: {:?}", args))
    } else {
        let reducted = args[0].clone();
        log::debug!("reduct_next({})", reducted);
        let next_sub;
        {
            *iter.get_mut() = reducted;
            iter.next();
            next_sub = iter.get_mut().clone();
            log::debug!("reduct_next: next_sub after reduction: {}", next_sub);
        }
        if iter.has_next() {
            interpreter.push(ReductStep::new(iter));
            interpreter.push(interpret_reducted_op as StepFunc);
            Ok(vec![next_sub])
        } else {
            interpreter.push(interpret_reducted_op as StepFunc);
            Ok(vec![next_sub])
        }
    }
}

fn execute(args: InterpreterResult, _interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 1 {
        Err("No arguments".to_string())
    } else {
        let mut atom = args[0].clone();
        log::debug!("execute({})", atom);
        if let Some(expr) = atom.as_expr_mut() {
            let op = expr.children().get(0).cloned();
            match op {
                Some(Atom::Grounded(op)) => {
                    // TODO: change API, remove boilerplate
                    let mut ops: Vec<Atom> = Vec::new();
                    let mut data :Vec<Atom> = Vec::new();
                    expr.children_mut().drain(1..).into_iter().rev().for_each(|atom| data.push(atom));
                    op.execute(&mut ops, &mut data)?;
                    Ok(data)
                },
                _ => Err(format!("Trying to execute non grounded atom: {:?}", expr)),
            }
        } else {
            Err(format!("Unexpected non expression argument: {}", atom))
        }
    }
}

fn match_op(args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 1 {
        Err(format!("Atom::Expression is expected as an argument, found: {:?}", args))
    } else {
        let expr = args[0].clone();
        log::debug!("match_op({})", expr);
        let var_x = VariableAtom::from("X");
        // TODO: unique variable?
        let atom_x = Atom::Variable(var_x.clone());
        let bindings = interpreter.working_branch().space.query(&Atom::expr(&[Atom::sym("="), expr.clone(), atom_x]));
        if bindings.is_empty() {
            log::debug!("match_op: no matches found, return: {}", expr);
            Ok(vec![expr])
        } else {
            for binding in bindings {
                let result = binding.get(&var_x).unwrap(); 
                let result = apply_bindings_to_atom(result, &binding);
                log::debug!("match_op: query: {}, binding: {:?}, result: {}", expr, binding, result);
                let branch = interpreter.branch(Ok(vec![result]));
                branch.bindings_mut_ref().push(binding);
                branch.push(interpret_op as StepFunc);
            }
            // Default answer is expression without any variables bound
            // is not returned because in such case many garbage results are
            // returned
            Err(format!("Cancel branch"))
        }
    }
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

    use super::*;
    use std::rc::Rc;
    
    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_sum_ints() {
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
