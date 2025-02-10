
use crate::*;
use crate::metta::*;
use super::{grounded_op, regex};
use crate::metta::text::Tokenizer;
use crate::metta::runner::number::*;
use crate::metta::runner::bool::*;

use std::fmt::Display;
use std::cell::RefCell;
use rand::{Rng, SeedableRng, rngs::StdRng};

thread_local!(static GLOBAL_RNG: RefCell<StdRng> = RefCell::new(StdRng::from_os_rng()));

//TODO: In the current version of rand it is possible for rust to hang if range end's value is too
// big. In future releases (0.9+) of rand signature of sample_single will be changed and it will be
// possible to use match construction to cover overflow and other errors. So after library will be
// upgraded RandomInt and RandomFloat codes should be altered.
// see comment https://github.com/trueagi-io/hyperon-experimental/pull/791#discussion_r1824355414
#[derive(Clone, Debug)]
pub struct RandomIntOp {}

grounded_op!(RandomIntOp, "random-int");

impl Grounded for RandomIntOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RandomIntOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("random-int expects two arguments: number (start) and number (end)");
        let start: i64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let end: i64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let range = start..end;
        if range.is_empty() {
            return Err(ExecError::from("Range is empty"));
        }
        Ok(vec![Atom::gnd(Number::Integer(GLOBAL_RNG.with(|generator| generator.borrow_mut().random_range(range))))])
    }
}

#[derive(Clone, Debug)]
pub struct RandomFloatOp {}

grounded_op!(RandomFloatOp, "random-float");

impl Grounded for RandomFloatOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RandomFloatOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("random-float expects two arguments: number (start) and number (end)");
        let start: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let end: f64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let range = start..end;
        if range.is_empty() {
            return Err(ExecError::from("Range is empty"));
        }
        Ok(vec![Atom::gnd(Number::Float(GLOBAL_RNG.with(|generator| generator.borrow_mut().random_range(range))))])
    }
}

#[derive(Clone, Debug)]
pub struct SetRandomSeedOp {}

grounded_op!(SetRandomSeedOp, "set-random-seed");

impl Grounded for crate::metta::runner::stdlib::random::SetRandomSeedOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for crate::metta::runner::stdlib::random::SetRandomSeedOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("set-random-seed expects one argument: number (seed)");
        let seed: i64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        GLOBAL_RNG.with(|generator| {*generator.borrow_mut() = StdRng::seed_from_u64(seed as u64)});
        Ok(vec![UNIT_ATOM])
    }
}


// NOTE: flip is absent in Python intentionally for conversion testing
#[derive(Clone, PartialEq, Debug)]
pub struct FlipOp{}

impl Display for FlipOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "flip")
    }
}

impl Grounded for FlipOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_BOOL])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FlipOp {
    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        Ok(vec![Atom::gnd(Bool(rand::random()))])
    }
}

pub fn register_common_tokens(tref: &mut Tokenizer) {
    let random_int_op = Atom::gnd(RandomIntOp{});
    tref.register_token(regex(r"random-int"), move |_| { random_int_op.clone() });
    let random_float_op = Atom::gnd(RandomFloatOp{});
    tref.register_token(regex(r"random-float"), move |_| { random_float_op.clone() });
    let set_seed_op = Atom::gnd(SetRandomSeedOp{});
    tref.register_token(regex(r"set-random-seed"), move |_| { set_seed_op.clone() });
    let flip_op = Atom::gnd(FlipOp{});
    tref.register_token(regex(r"flip"), move |_| { flip_op.clone() });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::stdlib::tests::run_program;

    #[test]
    fn metta_random() {
        assert_eq!(run_program(&format!("!(chain (eval (random-int 0 5)) $rint (and (>= $rint 0) (< $rint 5)))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(random-int 0 0)")), Ok(vec![vec![expr!("Error" ({ RandomIntOp{} } {Number::Integer(0)} {Number::Integer(0)}) "Range is empty")]]));
        assert_eq!(run_program(&format!("!(chain (eval (random-float 0.0 5.0)) $rfloat (and (>= $rfloat 0.0) (< $rfloat 5.0)))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(random-float 0 -5)")), Ok(vec![vec![expr!("Error" ({ RandomFloatOp{} } {Number::Integer(0)} {Number::Integer(-5)}) "Range is empty")]]));
        assert_eq!(run_program(&format!("!(set-random-seed 0)")), Ok(vec![vec![UNIT_ATOM]]));
    }

    #[test]
    fn random_op() {
        let res = RandomIntOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0..5;
        let res_i64: i64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(range.contains(&res_i64));
        let res = RandomIntOp{}.execute(&mut vec![expr!({Number::Integer(2)}), expr!({Number::Integer(-2)})]);
        assert_eq!(res, Err(ExecError::from("Range is empty")));

        let res = RandomFloatOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0.0..5.0;
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(range.contains(&res_f64));
        let res = RandomFloatOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(0)})]);
        assert_eq!(res, Err(ExecError::from("Range is empty")));

        let res = SetRandomSeedOp{}.execute(&mut vec![expr!({Number::Integer(0)})]);
        assert_eq!(res, Ok(vec![UNIT_ATOM]));
    }
}
