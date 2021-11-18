use crate::*;
use crate::common::*;
use crate::matcher::*;

use std::rc::Rc;

pub static SWAP_DATA: &Operation = &Operation{ name: "swap_data", execute: |ops, data| swap(ops, data, KindOfStack::Data) };
pub static POP_DATA: &Operation = &Operation{ name: "pop_data", execute: |ops, data| pop(ops, data, KindOfStack::Data) };
pub static POP_OPS: &Operation = &Operation{ name: "pop_ops", execute: |ops, data| pop(ops, data, KindOfStack::Ops) };

pub static INTERPRET: &Operation = &Operation{ name: "interpret", execute: |ops, data| interpret_op(ops, data, false) };
pub static EXECUTE: &Operation = &Operation{ name: "execute", execute: execute };
pub static REDUCT: &Operation = &Operation{ name: "reduct", execute: reduct };
pub static REDUCT_NEXT: &Operation = &Operation{ name: "reduct_next", execute: reduct_next };
pub static INTERPRET_REDUCTED: &Operation = &Operation{ name: "interpret_reducted", execute: |ops, data| interpret_op(ops, data, true) };
pub static MATCH: &Operation = &Operation{ name: "match", execute: match_op };
pub static MATCH_NEXT: &Operation = &Operation{ name: "match_next", execute: match_next };

pub fn interpret(space: Rc<GroundingSpace>, expr: &Atom) -> Result<Atom, String> {
    match expr {
        Atom::Expression(_) => {
            let mut ops: Vec<Atom> = Vec::new();
            let mut data: Vec<Atom> = Vec::new();

            data.push(expr.clone());
            data.push(Atom::gnd(Rc::clone(&space)));

            ops.push(Atom::gnd(INTERPRET));

            while !ops.is_empty() {
                let op = ops.pop();
                match op {
                    Some(Atom::Grounded(atom)) => atom.execute(&mut ops, &mut data),
                    Some(_) => Err("Ops stack contains non grounded atom".to_string()),
                    None => Err("Ops stack is empty".to_string()),
                }?;
            }

            if data.len() == 1 {
                Ok(data.pop().unwrap())
            } else if data.is_empty() {
                Err("No result".to_string())
            } else {
                Err("Too many data in stack".to_string())
            }
        },
        _ => Err(format!("Function supports only interpreting Atom::Expression, found: {}", expr)),
    }
}

fn is_grounded(expr: &ExpressionAtom) -> bool {
    matches!(expr.children().get(0), Some(Atom::Grounded(_)))
}

enum KindOfStack {
    Ops,
    Data,
}

fn choose_stack<'a>(ops: &'a mut Vec<Atom>, data: &'a mut Vec<Atom>, stack: KindOfStack) -> &'a mut Vec<Atom> {
    match stack {
        KindOfStack::Ops => ops,
        KindOfStack::Data => data,
    }
}

fn swap(ops: &mut Vec<Atom>, data: &mut Vec<Atom>, stack: KindOfStack) -> Result<(), String> {
    let stack = choose_stack(ops, data, stack);
    let args = (stack.pop(), stack.pop()); 
    match args {
        (Some(a), Some(b)) => {
            log::debug!("swap({}, {})", a, b);
            stack.push(a);
            stack.push(b);
            Ok(())
        },
        _ => Err(format!("Expected two arguments, found: {:?}", args)),
    }
}

fn pop(ops: &mut Vec<Atom>, data: &mut Vec<Atom>, stack: KindOfStack) -> Result<(), String> {
    let arg = data.pop();
    match arg {
        Some(gnd) => if let Some(num) = gnd.as_gnd::<usize>() {
            let stack = choose_stack(ops, data, stack);
            stack.truncate(stack.len() - num);
            Ok(())
        } else {
            Err(format!("usize argument expected, found: {}", gnd))
        },
        _ => Err(format!("Atom::Grounded argument expected, found: {:?}", arg)),
    }
}

fn interpret_op(ops: &mut Vec<Atom>, data: &mut Vec<Atom>, reducted: bool) -> Result<(), String> {
    let args = (data.pop(), data.pop()); 
    match args {
        (Some(space), Some(atom)) => {
            log::debug!("interpret_op{}({}, {})", if reducted { "<reducted>" } else { "" }, space, atom);
            match atom {
                Atom::Expression(ref expr) => {
                    if !expr.is_plain() && !reducted {
                        data.push(atom);
                        data.push(space);
                        ops.push(Atom::gnd(REDUCT));
                    } else {
                        if is_grounded(expr) {
                            data.push(atom);
                            ops.push(Atom::gnd(EXECUTE));
                        } else {
                            data.push(atom);
                            data.push(space);
                            ops.push(Atom::gnd(MATCH));
                        }
                    }
                    Ok(())
                },
                _ => {
                    data.push(atom);
                    Ok(())
                }
            }
        },
        _ => Err(format!("Expected GroundingSpace and Atom as arguments, found: {:?}", args)),
    }
}


fn execute(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let arg = data.pop(); 
    match arg {
        Some(Atom::Expression(mut expr)) => {
            log::debug!("execute({})", expr);
            let op = expr.children().get(0).cloned();
            match op {
                Some(Atom::Grounded(op)) => {
                    expr.children_mut().drain(1..).into_iter().rev().for_each(|atom| data.push(atom));
                    op.execute(ops, data)
                },
                _ => Err(format!("Trying to execute non grounded atom: {:?}", expr)),
            }
        },
        Some(atom) => Err(format!("Unexpected argument: {}", atom)),
        _ => Err("No arguments".to_string()),
    }
}

fn reduct(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    // TODO: think about leaving data on stack when parameters are wrong let arg = data.pop(); 
    let args = (data.pop(), data.pop());
    match args {
        (Some(space), Some(expr_atom)) => {
            log::debug!("reduct({}, {})", space, expr_atom);
            if let Atom::Expression(ref expr) = expr_atom {
                if expr.is_plain() {
                    data.push(expr_atom);
                    data.push(space);
                    ops.push(Atom::gnd(INTERPRET_REDUCTED));
                    Ok(())
                } else {
                    let iter = Rc::new(GndRefCell::new(expr.sub_expr_iter()));
                    let sub;
                    {
                        let mut stream = iter.raw().borrow_mut();
                        stream.next();
                        sub = stream.get_mut().clone();
                    }

                    data.push(Atom::gnd(iter));
                    data.push(space.clone());
                    ops.push(Atom::gnd(REDUCT_NEXT));

                    ops.push(Atom::gnd(SWAP_DATA));

                    data.push(sub);
                    data.push(space);
                    ops.push(Atom::gnd(INTERPRET_REDUCTED));
                    Ok(())
                }
            } else {
                Err(format!("GroundingSpace and Atom::Expression are expected as arguments, found: {}, {}", space, expr_atom))
            }
        },
        _ => Err(format!("Two arguments are expected, found: {:?}", args)),
    }
}

type ExpressionAtomIterGnd = Rc<GndRefCell<SubexpressionStream>>;

fn reduct_next(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let args = (data.pop(), data.pop(), data.pop());
    match args {
        (Some(space), Some(reducted), Some(iter_atom)) => {
            log::debug!("reduct_next({}, {}, {})", space, reducted, iter_atom);
            if let Some(iter) = iter_atom.as_gnd::<ExpressionAtomIterGnd>() {
                let next_sub;
                {
                    let mut stream = iter.raw().borrow_mut();
                    *stream.get_mut() = reducted;
                    stream.next();
                    next_sub = stream.get_mut().clone();
                    log::debug!("reduct_next: next_sub after reduction: {}", next_sub);
                }
                if iter.raw().borrow().has_next() {
                    // TODO: think about implementing Copy for the GroundedAtom
                    data.push(iter_atom);
                    data.push(space.clone());
                    ops.push(Atom::gnd(REDUCT_NEXT));

                    ops.push(Atom::gnd(SWAP_DATA));

                    data.push(next_sub);
                    data.push(space);
                    ops.push(Atom::gnd(INTERPRET_REDUCTED));
                } else {
                    data.push(next_sub);
                    data.push(space);
                    ops.push(Atom::gnd(INTERPRET_REDUCTED));
                }
                Ok(())
            } else {
                Err(format!("Reference to expression being reducted is expected as a third argument, found: {}", iter_atom))
            }
        },
        _ => Err(format!("Three arguments expected, found: {:?}", args)),
    }
}

fn match_op(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let args = (data.pop(), data.pop());
    match args {
        (Some(space), Some(expr)) => {
            log::debug!("match_op({}, {})", space, expr);
            if let Some(space) = space.as_gnd::<Rc<GroundingSpace>>() {
                let var_x = VariableAtom::from("X");
                // TODO: unique variable?
                let atom_x = Atom::Variable(var_x.clone());
                let bindings = space.query(&Atom::expr(&[Atom::sym("="), expr.clone(), atom_x]));
                {
                    let res = expr;
                    log::debug!("match_op: default: {}", res);
                    data.push(res);
                }
                let mut num: usize = 0;
                for binding in &bindings {
                    let res = binding.get(&var_x).unwrap(); 
                    let res = apply_bindings_to_atom(res, binding);
                    log::debug!("match_op: res: {}", res);
                    data.push(Atom::gnd(num));
                    ops.push(Atom::gnd(MATCH_NEXT));

                    data.push(res);
                    data.push(Atom::gnd(Rc::clone(space)));
                    ops.push(Atom::gnd(INTERPRET));
                    num = num + 1;
                }
                Ok(())
            } else {
                Err(format!("Rc<GroundingSpace> is expected as a first arguments, found: {}", space))
            }
        },
        _ => Err(format!("Atom::Grounded and Atom::Expression are expected as arguments, found: {:?}", args)),
    }
}

fn match_next(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let args = (data.pop(), data.pop());
    match args {
        (Some(atom), Some(num)) => if let Some(num) = num.as_gnd::<usize>() {
            log::debug!("match_next({}, {})", atom, num);
            match atom {
                // TODO: find out whether we can change Atom implementation
                // to match expressions using Rust matchers.
                Atom::Expression(ref expr) => {
                    // TODO: not used yet, the idea is to return error
                    // expression from INTERPRET to move to the next 
                    // alternative
                    if let Some(atom) = expr.children().get(0) {
                        if *atom == Atom::sym("error") {
                            // Return is used here because I would like
                            // to have one branch which truncates stacks
                            // instead of two (see code below).
                            return Ok(())
                        }
                    }
                    log::debug!("match_next: cleanup other alternatives, result: {}", atom);
                    ops.truncate(ops.len() - num * 2);
                    data.truncate(data.len() - num * 3 - 1);
                    data.push(atom);
                    Ok(())
                },
                _ => {
                    log::debug!("match_next: cleanup other alternatives, result: {}", atom);
                    ops.truncate(ops.len() - num * 2);
                    data.truncate(data.len() - num * 3 - 1);
                    data.push(atom);
                    Ok(())
                },
            }
        } else {
            Err(format!("Number of alternatives is expected as first argument, found: {}", num))
        },
        _ => Err(format!("Expected two arguments, found: {:?}", args)),
    }
}
