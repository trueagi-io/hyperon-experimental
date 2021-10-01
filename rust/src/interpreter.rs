use crate::*;
use crate::common::*;
use std::rc::Rc;
use std::iter::Peekable;

pub static INTERPRET: &Operation = &Operation{ name: "interpret", execute: |ops, data| interpret(ops, data, false) };
pub static EXECUTE: &Operation = &Operation{ name: "execute", execute: execute };
pub static REDUCT: &Operation = &Operation{ name: "reduct", execute: reduct };
pub static REDUCT_NEXT: &Operation = &Operation{ name: "reduct_next", execute: reduct_next };
pub static INTERPRET_REDUCTED: &Operation = &Operation{ name: "interpret", execute: |ops, data| interpret(ops, data, true) };

fn is_grounded(expr: &ExpressionAtom) -> bool {
    matches!(expr.children.get(0), Some(Atom::Grounded(_)))
}

fn interpret(ops: &mut Vec<Atom>, data: &mut Vec<Atom>, reducted: bool) -> Result<(), String> {
    let arg = data.last(); 
    match arg {
        Some(atom) => match atom {
            Atom::Expression(expr) => {
                if is_grounded(expr) {
                    if (expr.is_plain() || reducted) && is_grounded(expr) {
                        ops.push(Atom::gnd(EXECUTE));
                        Ok(())
                    } else {
                        ops.push(Atom::gnd(REDUCT));
                        Ok(())
                    }
                } else {
                    Err("Not implemented".to_string())
                }
            },
            _ => Err("Not implemented".to_string()),
        },
        None => Err("No args passed".to_string()),
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
    // TODO: think about leaving data on stack when parameters are wrong
    let arg = data.pop(); 
    match arg {
        Some(Atom::Expression(ref expr)) => {
            if expr.is_plain() {
                ops.push(Atom::gnd(INTERPRET));
                data.push(arg.unwrap());
                Ok(())
            } else {
                let iter = Rc::new(GndRefCell::new(expr.iter().peekable()));
                let (sub, ..) = iter.raw().borrow_mut().peek().unwrap().clone();
                data.push(Atom::gnd(iter));
                ops.push(Atom::gnd(REDUCT_NEXT));
                data.push(Atom::Expression(sub.clone()));
                ops.push(Atom::gnd(INTERPRET));
                Ok(())
            }
        },
        _ => Err(format!("Expression is expected as an argument, found instead: {:?}", arg)),
    }
}

fn reduct_next(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let args = (data.pop(), data.pop());
    match args {
        (Some(reducted), Some(gnd)) => {
            if let Atom::Grounded(ref iter) = gnd {
                let iter = iter.downcast_ref::<Rc<GndRefCell<Peekable<ExpressionAtomIter<'_>>>>>();
                if let Some(iter) = iter {
                    let (_, parent, idx) = iter.raw().borrow_mut().next().unwrap();
                    let mut parent = parent.clone();
                    // FIXME: add method to access children indirectly or move iterator into ExpressionAtom
                    parent.children[idx] = reducted;
                    if let Some((sub, ..)) = iter.raw().borrow_mut().peek() {
                        data.push(gnd.clone());
                        ops.push(Atom::gnd(REDUCT_NEXT));
                        data.push(Atom::Expression((*sub).clone()));
                        ops.push(Atom::gnd(INTERPRET));
                    } else {
                        data.push(Atom::Expression(parent.clone()));
                        ops.push(Atom::gnd(INTERPRET_REDUCTED));
                    }
                    Ok(())
                } else {
                    Err(format!("Reference to expression being reducted is expected as second argument, found: {}", gnd))
                }
            } else {
                Err(format!("Grounded atom is expected as a second argument, found: {}", gnd))
            }
        },
        _ => Err(format!("Two arguments expected, found: {:?}", args)),
    }
}
