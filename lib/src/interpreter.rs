use crate::*;
use crate::common::*;
use crate::matcher::*;

use std::rc::Rc;

pub fn interpret(space: Rc<GroundingSpace>, expr: &Atom) -> InterpreterResult {
    let mut interpreter = Interpreter { result: Ok(vec![Atom::gnd(space), expr.clone()]), tasks: Vec::new(), bindings_stack: Vec::new() };
    interpreter.push(interpret_op);
    
    loop {
        let result = interpreter.step();
        match result {
            Some(result) => {
                return result_cloned(result);
            },
            _ => continue,
        }
    }
}

fn is_grounded(expr: &ExpressionAtom) -> bool {
    matches!(expr.children().get(0), Some(Atom::Grounded(_)))
}

type ExpressionAtomIterGnd = Rc<GndRefCell<SubexpressionStream>>;

type InterpreterResult = Result<Vec<Atom>, String>;

struct Interpreter {
    result: InterpreterResult,
    tasks: Vec<Box<dyn FnOnce(InterpreterResult, &mut Interpreter) -> InterpreterResult>>,
    bindings_stack: Vec<Bindings>,
}

fn result_cloned<T: Clone, E: Clone>(src: &Result<T, E>) -> Result<T, E> {
    let src = src.as_ref();
    if src.is_err() {
        Err(src.err().unwrap().clone())
    } else {
        Ok(src.ok().unwrap().clone())
    }
}

impl Interpreter {

    fn push<F>(&mut self, task: F)
        where F: 'static + FnOnce(InterpreterResult, &mut Interpreter) -> InterpreterResult {
            self.tasks.push(Box::new(task));
    }

    fn step(&mut self) -> Option<&InterpreterResult> {
        if self.tasks.is_empty() {
            Some(&self.result)
        } else {
            let next = self.tasks.pop().unwrap();
            self.result = next(result_cloned(&self.result), self);
            None
        }
    }

    fn bindings_ref(&self) -> &Vec<Bindings> {
        &self.bindings_stack
    }

    fn bindings_mut_ref(&mut self) -> &mut Vec<Bindings> {
        &mut self.bindings_stack
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
    if args.len() < 2 {
        Err(format!("Expected GroundingSpace and Atom as arguments, found: {:?}", args))
    } else {
        let (space, atom) = (args[0].clone(), args[1].clone());
        log::debug!("interpret_op({}, {})", space, atom);
        if let Some(ref expr) = atom.as_expr() {
            if !expr.is_plain() {
                interpreter.push(reduct);
                Ok(vec![space, atom])
            } else {
                interpreter.push(interpret_reducted_op);
                Ok(vec![space, atom])
            }
        } else {
            Ok(vec![atom])
        }
    }
}

fn interpret_reducted_op(args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 2 {
        Err(format!("Expected GroundingSpace and Atom as arguments, found: {:?}", args))
    } else {
        let (space, atom) = (args[0].clone(), &args[1]);
        let atom = apply_bindings_stack_to_atom(atom, interpreter.bindings_ref());
        log::debug!("interpret_reducted_op({}, {})", space, atom);
        if let Some(ref expr) = atom.as_expr() {
            if is_grounded(expr) {
                interpreter.push(execute);
                Ok(vec![atom])
            } else {
                interpreter.push(match_op);
                Ok(vec![space, atom])
            }
        } else {
            Ok(vec![atom])
        }
    }
}

fn reduct(args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 2 {
        Err(format!("Two arguments are expected, found: {:?}", args))
    } else {
        let (space, expr_atom) = (args[0].clone(), args[1].clone());
        log::debug!("reduct({}, {})", space, expr_atom);
        if let Some(ref expr) = expr_atom.as_expr() {
            if expr.is_plain() {
                interpreter.push(interpret_reducted_op);
                Ok(vec![space.clone(), expr_atom])
            } else {
                let iter = Rc::new(GndRefCell::new(expr.sub_expr_iter()));
                let sub;
                {
                    let mut stream = iter.raw().borrow_mut();
                    stream.next();
                    sub = stream.get_mut().clone();
                }
                let space_2 = space.clone();
                interpreter.push(move |res, interpreter| reduct_next(space_2, iter, res, interpreter));
                interpreter.push(interpret_reducted_op);
                Ok(vec![space.clone(), sub])
            }
        } else {
            Err(format!("GroundingSpace and Atom::Expression are expected as arguments, found: {}, {}", space, expr_atom))
        }
    }
}

fn reduct_next(space: Atom, iter: ExpressionAtomIterGnd, args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    let args = args?;
    if args.len() < 1 {
        Err(format!("Intepreted subexpression is expected as argument, found: {:?}", args))
    } else {
        let reducted = args[0].clone();
        log::debug!("reduct_next({}, {})", space, reducted);
        let next_sub;
        {
            let mut stream = iter.raw().borrow_mut();
            *stream.get_mut() = reducted;
            stream.next();
            next_sub = stream.get_mut().clone();
            log::debug!("reduct_next: next_sub after reduction: {}", next_sub);
        }
        if iter.raw().borrow().has_next() {
            let space_2 = space.clone();
            interpreter.push(move |res, interpreter| reduct_next(space_2, iter, res, interpreter));
            interpreter.push(interpret_reducted_op);
            Ok(vec![space.clone(), next_sub])
        } else {
            interpreter.push(interpret_reducted_op);
            Ok(vec![space.clone(), next_sub])
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
    if args.len() < 2 {
        Err(format!("Atom::Grounded and Atom::Expression are expected as arguments, found: {:?}", args))
    } else {
        let (space, expr) = (args[0].clone(), args[1].clone());
        log::debug!("match_op({}, {})", space, expr);
        if let Some(space) = space.as_gnd::<Rc<GroundingSpace>>() {
            let var_x = VariableAtom::from("X");
            // TODO: unique variable?
            let atom_x = Atom::Variable(var_x.clone());
            let bindings = space.query(&Atom::expr(&[Atom::sym("="), expr.clone(), atom_x]));
            if bindings.is_empty() {
                log::debug!("match_op: no matches found, return: {}", expr);
                Ok(vec![expr])
            } else {
                let mut matches: Vec<(Atom, Bindings)> = Vec::new();
                {
                    let res = expr;
                    log::debug!("match_op: default: {}", res);
                    matches.push((res, Bindings::new()));
                }
                let mut num: usize = 0;
                for binding in bindings {
                    let res = binding.get(&var_x).unwrap(); 
                    let res = apply_bindings_to_atom(res, &binding);
                    log::debug!("match_op: res: {}", res);
                    matches.push((res, binding));
                    num = num + 1;
                }
                let first = matches.pop().unwrap();
                // If this construction is inlined compiler complains, because clone()
                // is called in the context of the closure
                let space_2 = Rc::clone(space);
                interpreter.push(| result, interpreter | match_next(space_2, matches, result, interpreter));
                interpreter.push(interpret_op);
                interpreter.bindings_mut_ref().push(first.1);
                Ok(vec![Atom::gnd(Rc::clone(space)), first.0])
            }
        } else {
            Err(format!("Rc<GroundingSpace> is expected as a first arguments, found: {}", space))
        }
    }
}

fn match_next(space: Rc<GroundingSpace>, mut matches: Vec<(Atom, Bindings)>, args: InterpreterResult, interpreter: &mut Interpreter) -> InterpreterResult {
    if args.is_ok() {
        log::debug!("match_next: return: {:?}", args);
        args
    } else {
        if matches.len() == 1 {
            log::debug!("match_next: return default");
            let next = matches.pop().unwrap();
            interpreter.push(interpret_reducted_op);
            interpreter.bindings_mut_ref().pop();
            interpreter.bindings_mut_ref().push(next.1);
            Ok(vec![Atom::gnd(Rc::clone(&space)), next.0])
        } else {
            log::debug!("match_next: return: {:?}", args);
            let next = matches.pop().unwrap();
            let space_2 = Rc::clone(&space);
            interpreter.push(| result, interpreter | match_next(space_2, matches, result, interpreter));
            interpreter.push(interpret_op);
            interpreter.bindings_mut_ref().pop();
            interpreter.bindings_mut_ref().push(next.1);
            Ok(vec![Atom::gnd(Rc::clone(&space)), next.0])
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
}
