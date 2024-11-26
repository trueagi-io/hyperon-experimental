use crate::*;
use crate::metta::*;
#[cfg(feature = "pkg_mgmt")]

use crate::space::DynSpace;
use crate::common::shared::Shared;
use crate::metta::text::Tokenizer;
use crate::metta::runner::arithmetics::*;
use crate::metta::runner::stdlib::grounded_op;
use crate::metta::runner::Metta;
use crate::metta::runner::stdlib::regex;

use std::fmt::Display;
use rand::Rng;

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
        let mut rng = rand::thread_rng();
        Ok(vec![Atom::gnd(Number::Integer(rng.gen_range(range)))])
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
        let mut rng = rand::thread_rng();
        Ok(vec![Atom::gnd(Number::Float(rng.gen_range(range)))])
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

pub fn register_common_tokens(tref: &mut Tokenizer, _tokenizer: Shared<Tokenizer>, _space: &DynSpace, metta: &Metta) {
    let random_int_op = Atom::gnd(RandomIntOp{});
    tref.register_token(regex(r"random-int"), move |_| { random_int_op.clone() });
    let random_float_op = Atom::gnd(RandomFloatOp{});
    tref.register_token(regex(r"random-float"), move |_| { random_float_op.clone() });
    #[cfg(feature = "pkg_mgmt")]
    metta::runner::stdlib::pkg_mgmt_ops::register_pkg_mgmt_tokens(tref, metta);
}

pub fn register_rust_stdlib_tokens(tref: &mut Tokenizer) {
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
    }
}