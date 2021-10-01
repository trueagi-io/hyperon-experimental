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

struct ExpressionLevel<'a> {
    expr: &'a Atom,
    idx: Option<usize>,
    iter: std::iter::Enumerate<std::slice::Iter<'a, Atom>>,
}

impl<'a> ExpressionLevel<'a> {
    fn from(expr: *const Atom, idx: Option<usize>) -> Self {
        unsafe {
            let rexpr = &*expr as &Atom;
            // FIXME: add method to access children indirectly or move iterator into ExpressionAtom
            // implementation
            ExpressionLevel{ expr: rexpr, idx, iter: rexpr.as_expr().unwrap().children.iter().enumerate() }
        }
    }
}

struct ExpressionAtomIter<'a> {
    expr: *mut Atom,
    levels: Vec<ExpressionLevel<'a>>,
}

impl Drop for ExpressionAtomIter<'_> {
    fn drop(&mut self) {
        unsafe {
            // free heap memory by wrapping by box and dropping it
            Box::from_raw(self.expr);
        }
    }
}

impl<'a> ExpressionAtomIter<'a> {
    fn from(expr: &Atom) -> Self {
        // get memory on heap under our control
        let ptr = Box::into_raw(Box::new(expr.clone()));
        Self{ expr: ptr, levels: vec![ExpressionLevel::from(ptr, None)] }
    }

}

impl<'a> Iterator for ExpressionAtomIter<'a> {
    type Item = (&'a Atom, &'a Atom, usize);

    fn next(&mut self) -> Option<Self::Item> {
        while !self.levels.is_empty() {
            let level = self.levels.last_mut().unwrap();
            let next = level.iter.next();
            if None == next {
                let expr = level.expr;
                let idx = level.idx;
                self.levels.pop();
                if !self.levels.is_empty() {
                    let parent = self.levels.last().unwrap().expr;
                    return Some((expr, parent, idx.unwrap()))
                }
            } else {
                let (idx, next) = next.unwrap();
                if let Atom::Expression(_) = next {
                    self.levels.push(ExpressionLevel::from(next, Some(idx)))
                }
            }
        }
        None
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
                let iter = Rc::new(GndRefCell::new(ExpressionAtomIter::from(&arg.unwrap()).peekable()));
                let (sub, ..) = iter.raw().borrow_mut().peek().unwrap().clone();
                data.push(Atom::gnd(iter));
                ops.push(Atom::gnd(REDUCT_NEXT));
                data.push(sub.clone());
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
                    parent.as_expr_mut().unwrap().children[idx] = reducted;
                    if let Some((sub, ..)) = iter.raw().borrow_mut().peek() {
                        data.push(gnd.clone());
                        ops.push(Atom::gnd(REDUCT_NEXT));
                        data.push((*sub).clone());
                        ops.push(Atom::gnd(INTERPRET));
                    } else {
                        data.push(parent.clone());
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

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

    use super::*;

    // Aliases to have a shorter notation
    fn S(name: &str) -> Atom { Atom::sym(name) }
    fn E(children: &[Atom]) -> Atom { Atom::expr(children) }
    fn V(name: &str) -> Atom { Atom::var(name) }

    #[test]
    fn test_subexpression_iterator() {
        // (+ (* 3 (+ 1 1)) (- 4 3))
        let plus11 = E(&[S("+"), S("1"), V("n")]);
        let mul3plus11 = E(&[S("*"), S("3"), plus11.clone()]);
        let minus43 = E(&[S("-"), S("4"), S("3")]);
        let expr = E(&[S("+"), mul3plus11.clone(), minus43.clone()]);

        let iter = ExpressionAtomIter::from(&expr);

        assert_eq!(iter
            .map(|(a, p, i)| (a.clone(), p.clone(), i))
            .collect::<Vec<_>>(), vec![
                (plus11, mul3plus11.clone(), 2),
                (mul3plus11, expr.clone(), 1),
                (minus43, expr, 2),
            ]);
    }

}
