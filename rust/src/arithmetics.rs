use crate::*;
use crate::common::*;

pub type Int = GroundedValue<i32>;

pub static SUM: StaticGroundedAtomRef<Operation> = StaticGroundedAtomRef{ r: &Operation{ name: "+", execute: sum }};

fn sum(_ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
    let arg1 = data.pop().expect("Not enough arguments for a sum operation"); 
    let arg2 = data.pop().expect("Not enough arguments for a sum operation");
    match (&arg1, &arg2) {
        (Atom::Grounded(arg1),
                Atom::Grounded(arg2)) => {
            let _arg1 = (*arg1).downcast_ref::<Int>();
            let _arg2 = (*arg2).downcast_ref::<Int>();
            match (_arg1, _arg2) {
                (Some(arg1), Some(arg2)) => {
                    data.push(Int::new(arg1.get() + arg2.get()));
                    Ok(())
                },
                _ => Err(format!("One of the arguments is not Int: {}, {}", arg1, arg2)),
            }
            
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
        let mut data = vec![Atom::expr(&[Atom::gnd(SUM), Int::new(3), Int::new(5)])];

        assert_eq!(space.interpret(&mut ops, &mut data), Ok(()));
        assert_eq!(space.interpret(&mut ops, &mut data), Ok(()));

        assert_eq!(ops, vec![]);
        assert_eq!(data, vec![Int::new(8)]);
    }
}
