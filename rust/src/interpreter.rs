use crate::*;
use crate::common::*;

pub static INTERPRET: &Operation = &Operation{ name: "interpret", execute: interpret };
pub static EXECUTE: &Operation = &Operation{ name: "execute", execute: execute };

fn is_grounded(expr: &ExpressionAtom) -> bool {
    matches!(expr.children.get(0), Some(Atom::Grounded(_)))
}

fn interpret(ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let arg = data.last(); 
    match arg {
        Some(atom) => match atom {
            Atom::Expression(expr) => {
                if expr.is_plain() && is_grounded(expr) {
                    ops.push(Atom::gnd(EXECUTE));
                    Ok(())
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


#[cfg(test)]
mod tests {
}
