use crate::*;
use crate::common::*;

pub static SUM: &Operation = &Operation{ name: "+", execute: sum };

fn sum(_ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    // TODO: getting arguments from stack and checking their type can be
    // done in separate helper function or macros.
    let arg1 = data.pop().expect("Sum operation called without arguments"); 
    let arg2 = data.pop().expect("Sum operation called with only argument");
    match (&arg1, &arg2) {
        (Atom::Grounded(arg1), Atom::Grounded(arg2)) => {
            let arg1 = (*arg1).downcast_ref::<i32>().expect(&format!("First argument is not Int: {}", arg1));
            let arg2 = (*arg2).downcast_ref::<i32>().expect(&format!("Second argument is not Int: {}", arg2));
            data.push(Atom::gnd(arg1 + arg2));
            Ok(())
        },
        _ => Err(format!("One of the arguments is not grounded: {}, {}", arg1, arg2)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::*;

    #[test]
    fn test_sum_ints() {
        let space = GroundingSpace::new();
        let mut ops = vec![Atom::gnd(INTERPRET)];
        let mut data = vec![Atom::expr(&[Atom::gnd(SUM), Atom::gnd(3), Atom::gnd(5)])];

        assert_eq!(space.interpret(&mut ops, &mut data), Ok(()));
        assert_eq!(space.interpret(&mut ops, &mut data), Ok(()));

        assert_eq!(ops, vec![]);
        assert_eq!(data, vec![Atom::gnd(8)]);
    }
}
