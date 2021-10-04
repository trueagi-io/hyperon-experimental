use crate::*;
use crate::common::*;
use crate::matcher::*;

use std::rc::Rc;
use std::iter::Peekable;

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

// FIXME: There should be the way to pass reference to the GroundingSpace
// instead of moving it into. Trying to pass reference and convert it to
// to the atom leads to compiler complaining the link doesn't have 'static
// lifetime.
pub fn interpret(space: GroundingSpace, expr: &Atom) -> Result<Atom, String> {
    match expr {
        Atom::Expression(_) => {
            let mut ops: Vec<Atom> = Vec::new();
            let mut data: Vec<Atom> = Vec::new();

            let space = Rc::new(space);

            data.push(expr.clone());
            data.push(Atom::gnd(Rc::clone(&space)));

            ops.push(Atom::gnd(INTERPRET));

            while !ops.is_empty() {
                if let Err(res) = space.interpret(&mut ops, &mut data) {
                    return Err(res);
                }
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
    matches!(expr.children.get(0), Some(Atom::Grounded(_)))
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
    println!("swap{:?}", args);
    match args {
        (Some(a), Some(b)) => {
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
        Some(Atom::Grounded(num)) => {
            match num.downcast_ref::<usize>() {
                Some(num) => {
                    let stack = choose_stack(ops, data, stack);
                    stack.truncate(stack.len() - num);
                    Ok(())
                },
                _ => Err(format!("usize argument expected, found: {}", num)),
            }
        },
        _ => Err(format!("Atom::Grounded argument expected, found: {:?}", arg)),
    }
}

fn interpret_op(ops: &mut Vec<Atom>, data: &mut Vec<Atom>, reducted: bool) -> Result<(), String> {
    let args = (data.pop(), data.pop()); 
    println!("interpret_op{:?}", args);
    match args {
        (Some(space), Some(atom)) => match atom {
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
        },
        _ => Err(format!("Expected GroundingSpace and Atom as arguments, found: {:?}", args)),
    }
}


fn execute(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let arg = data.pop(); 
    println!("execute({:?})", arg);
    match arg {
        Some(Atom::Expression(expr)) => match &expr.children[0] {
            Atom::Grounded(op) => {
                &expr.children.iter().skip(1).rev().for_each(|atom| data.push(atom.clone()));
                op.execute(ops, data)
            },
            _ => Err(format!("Trying to execute non grounded atom: {:?}", expr)),
        },
        Some(atom) => Err(format!("Unexpected argument: {}", atom)),
        _ => Err("No arguments".to_string()),
    }
}

fn reduct(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    // TODO: think about leaving data on stack when parameters are wrong let arg = data.pop(); 
    let args = (data.pop(), data.pop());
    println!("reduct{:?}", args);
    match args {
        (Some(space), Some(expr_atom)) => {
            if let Atom::Expression(ref expr) = expr_atom {
                if expr.is_plain() {
                    data.push(expr_atom);
                    data.push(space);
                    ops.push(Atom::gnd(INTERPRET));
                    Ok(())
                } else {
                    let iter = Rc::new(GndRefCell::new(expr.iter().peekable()));
                    let (sub, ..) = iter.raw().borrow_mut().peek().unwrap().clone();

                    data.push(Atom::gnd(iter));
                    data.push(space.clone());
                    ops.push(Atom::gnd(REDUCT_NEXT));

                    ops.push(Atom::gnd(SWAP_DATA));
                    
                    data.push(Atom::Expression(sub.clone()));
                    data.push(space);
                    ops.push(Atom::gnd(INTERPRET));
                    Ok(())
                }
            } else {
                Err(format!("GroundingSpace and Atom::Expression are expected as arguments, found: {}, {}", space, expr_atom))
            }
        },
        _ => Err(format!("Two arguments are expected, found: {:?}", args)),
    }
}

fn reduct_next(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let args = (data.pop(), data.pop(), data.pop());
    println!("reduct_next{:?}", args);
    match args {
        (Some(space), Some(reducted), Some(iter_atom)) => {
            if let Atom::Grounded(ref iter) = iter_atom {
                let iter = iter.downcast_ref::<Rc<GndRefCell<Peekable<ExpressionAtomIter<'_>>>>>();
                if let Some(iter) = iter {
                    let (_, parent, idx) = iter.raw().borrow_mut().next().unwrap();
                    let mut parent = parent.clone();
                    // FIXME: add method to access children indirectly or move iterator into ExpressionAtom
                    parent.children[idx] = reducted;
                    if let Some((sub, ..)) = iter.raw().borrow_mut().peek() {
                        // TODO: think about implementing Copy for the GroundedAtom
                        data.push(iter_atom.clone());
                        data.push(space.clone());
                        ops.push(Atom::gnd(REDUCT_NEXT));

                        ops.push(Atom::gnd(SWAP_DATA));

                        data.push(Atom::Expression((*sub).clone()));
                        data.push(space);
                        ops.push(Atom::gnd(INTERPRET));
                    } else {
                        data.push(Atom::Expression(parent.clone()));
                        data.push(space);
                        ops.push(Atom::gnd(INTERPRET_REDUCTED));
                    }
                    Ok(())
                } else {
                    Err(format!("Reference to expression being reducted is expected as second argument, found: {}", iter_atom))
                }
            } else {
                Err(format!("Grounded atom is expected as a second argument, found: {}", iter_atom))
            }
        },
        _ => Err(format!("Two arguments expected, found: {:?}", args)),
    }
}

fn match_op(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let args = (data.pop(), data.pop());
    println!("match_op{:?}", args);
    match args {
        (Some(Atom::Grounded(space)), Some(expr)) => {
            if let Some(space) = space.downcast_ref::<Rc<GroundingSpace>>() {
                let var_x = VariableAtom::from("X");
                let atom_x = Atom::Variable(var_x.clone());
                let bindings = space.query(&Atom::expr(&[Atom::sym("="), expr, atom_x]));
                let mut num: usize = 0;
                for binding in &bindings {
                    let res = binding.get(&var_x).unwrap(); 
                    let res = apply_bindings_to_atom(res, binding);
                    println!("match_op: res: {}", res);
                    data.push(Atom::gnd(num));
                    ops.push(Atom::gnd(MATCH_NEXT));

                    data.push(res.clone());
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
    println!("match_next{:?}", args);
    match args {
        (Some(atom), Some(num)) => {
            match &num {
                Atom::Grounded(num) if num.is::<usize>() => {
                    let num = num.downcast_ref::<usize>().unwrap();
                    match atom {
                        // TODO: find out whether we can change Atom implementation
                        // to match expressions using Rust matchers.
                        Atom::Expression(ExpressionAtom{ children: ref expr }) => {
                            // TODO: not used yet, the idea is to return error
                            // expression from INTERPRET to move to the next 
                            // alternative
                            if let [Atom::Symbol{symbol}, _] = &expr[..] {
                                if symbol == "error" {
                                    // Return is used here because I would like
                                    // to have one branch which truncates stacks
                                    // instead of two (see code below).
                                    return Ok(())
                                }
                            }
                            println!("match_next: cleanup other alternatives");
                            ops.truncate(ops.len() - num * 2);
                            data.truncate(data.len() - num * 3);
                            data.push(atom.clone());
                            Ok(())
                        },
                        _ => {
                            println!("match_next: cleanup other alternatives");
                            ops.truncate(ops.len() - num * 2);
                            data.truncate(data.len() - num * 3);
                            data.push(atom.clone());
                            Ok(())
                        },
                    }
                },
                _ => Err(format!("Number of alternatives is expected as first argument, found: {}", num))
            }
        },
        _ => Err(format!("Expected two arguments, found: {:?}", args)),
    }
}
