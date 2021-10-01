use crate::*;
use crate::common::*;
use std::rc::Rc;
use std::iter::Peekable;

pub static SWAP_DATA: &Operation = &Operation{ name: "swap_data", execute: swap_data };
pub static INTERPRET: &Operation = &Operation{ name: "interpret", execute: |ops, data| interpret_op(ops, data, false) };
pub static EXECUTE: &Operation = &Operation{ name: "execute", execute: execute };
pub static REDUCT: &Operation = &Operation{ name: "reduct", execute: reduct };
pub static REDUCT_NEXT: &Operation = &Operation{ name: "reduct_next", execute: reduct_next };
pub static INTERPRET_REDUCTED: &Operation = &Operation{ name: "interpret_reducted", execute: |ops, data| interpret_op(ops, data, true) };
pub static MATCH: &Operation = &Operation{ name: "match", execute: match_op };

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

fn swap_data(_ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let args = (data.pop(), data.pop()); 
    match args {
        (Some(a), Some(b)) => {
            data.push(a);
            data.push(b);
            Ok(())
        },
        _ => Err(format!("Expected two arguments, found: {:?}", args)),
    }
}

fn interpret_op(ops: &mut Vec<Atom>, data: &mut Vec<Atom>, reducted: bool) -> Result<(), String> {
    let args = (data.pop(), data.pop()); 
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
            _ => Err("Interpret is not implemented for atoms other than Atom::Expression".to_string()),
        },
        _ => Err(format!("Expected GroundingSpace and Atom as arguments, found: {:?}", args)),
    }
}


fn execute(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let arg = data.pop(); 
    match arg {
        Some(Atom::Expression(expr)) => match &expr.children[0] {
            Atom::Grounded(op) => {
                &expr.children.iter().skip(1).for_each(|atom| data.push(atom.clone()));
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

fn match_op(_ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let args = (data.pop(), data.pop());
    match args {
        (Some(Atom::Grounded(space)), Some(Atom::Expression(expr))) => {
            Ok(())
        },
        _ => Err(format!("Atom::Grounded and Atom::Expression are expected as arguments, found: {:?}", args)),
    }
}
