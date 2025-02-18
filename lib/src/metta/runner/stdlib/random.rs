
use crate::*;
use crate::metta::*;
use super::{grounded_op, regex};
use crate::metta::text::Tokenizer;
use crate::metta::runner::number::*;
use crate::metta::runner::bool::*;

use std::fmt::{Display, Formatter};
use std::cell::RefCell;
use rand::{Rng, SeedableRng, rngs::StdRng};
use std::rc::Rc;

//TODO: In the current version of rand it is possible for rust to hang if range end's value is too
// big. In future releases (0.9+) of rand signature of sample_single will be changed and it will be
// possible to use match construction to cover overflow and other errors. So after library will be
// upgraded RandomInt and RandomFloat codes should be altered.
// see comment https://github.com/trueagi-io/hyperon-experimental/pull/791#discussion_r1824355414

pub const ATOM_TYPE_RANDOM_GENERATOR : Atom = sym!("RandomGenerator");

#[derive(Clone, Debug)]
pub struct RandomGenerator(Rc<RefCell<StdRng>>);

impl RandomGenerator {
    fn from_os_rng() -> Self {
        Self(Rc::new(RefCell::new(StdRng::from_os_rng())))
    }

    fn from_seed_u64(seed: u64) -> Self {
        Self(Rc::new(RefCell::new(StdRng::seed_from_u64(seed))))
    }

    fn reseed_from_u64(&self, seed: u64) {
        *self.0.borrow_mut() = StdRng::seed_from_u64(seed);
    }

    fn reset(&self) {
        *self.0.borrow_mut() = StdRng::from_os_rng();
    }

    fn random_range<T, R>(&self, range: R) -> T 
        where
            T: rand::distr::uniform::SampleUniform,
            R: rand::distr::uniform::SampleRange<T>,
    {
        self.0.borrow_mut().random_range(range)
    }
}

impl Grounded for RandomGenerator {
    fn type_(&self) -> Atom {
        ATOM_TYPE_RANDOM_GENERATOR 
    }
}

impl Display for RandomGenerator {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "RandomGenerator-{:?}", self.0.as_ptr())
    }
}

impl PartialEq for RandomGenerator {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

#[derive(Clone, Debug)]
pub struct RandomIntOp {}

grounded_op!(RandomIntOp, "random-int");

impl Grounded for RandomIntOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_RANDOM_GENERATOR, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RandomIntOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("random-int expects three arguments: random generator, number (start) and number (end)");
        let generator = args.get(0).ok_or_else(arg_error)?.into();
        let start: i64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let end: i64 = args.get(2).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let generator = Atom::as_gnd::<RandomGenerator>(generator).ok_or("random-int expects a random generator as its argument")?;
        let range = start..end;
        if range.is_empty() {
            return Err(ExecError::from("RangeIsEmpty"));
        }
        Ok(vec![Atom::gnd(Number::Integer(generator.random_range(range)))])
    }
}

#[derive(Clone, Debug)]
pub struct RandomFloatOp {}

grounded_op!(RandomFloatOp, "random-float");

impl Grounded for RandomFloatOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_RANDOM_GENERATOR, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RandomFloatOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("random-float expects three arguments: random generator, number (start) and number (end)");
        let generator = args.get(0).ok_or_else(arg_error)?.into();
        let start: f64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let end: f64 = args.get(2).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let generator = Atom::as_gnd::<RandomGenerator>(generator).ok_or("random-float expects a random generator as its argument")?;
        let range = start..end;
        if range.is_empty() {
            return Err(ExecError::from("RangeIsEmpty"));
        }
        Ok(vec![Atom::gnd(Number::Float(generator.random_range(range)))])
    }
}

#[derive(Clone, Debug)]
pub struct SetRandomSeedOp {}

grounded_op!(SetRandomSeedOp, "set-random-seed");

impl Grounded for SetRandomSeedOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_RANDOM_GENERATOR, ATOM_TYPE_NUMBER, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SetRandomSeedOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("set-random-seed expects two arguments: random generator and number (seed)");
        let generator = args.get(0).ok_or_else(arg_error)?.into();
        let seed: i64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let generator = Atom::as_gnd::<RandomGenerator>(generator).ok_or("set-random-seed expects a random generator as its argument")?;
        generator.reseed_from_u64(seed as u64);
        Ok(vec![UNIT_ATOM])
    }
}

#[derive(Clone, Debug)]
pub struct NewRandomGeneratorOp {}

grounded_op!(NewRandomGeneratorOp, "new-random-generator");

impl Grounded for NewRandomGeneratorOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_RANDOM_GENERATOR])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NewRandomGeneratorOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("new-random-generator expects one argument: number (seed)");
        let seed: i64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let new_generator = RandomGenerator::from_seed_u64(seed as u64);
        Ok(vec![Atom::gnd(new_generator)])
    }
}

#[derive(Clone, Debug)]
pub struct ResetRandomGeneratorOp {}

grounded_op!(ResetRandomGeneratorOp, "reset-random-generator");

impl Grounded for ResetRandomGeneratorOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_RANDOM_GENERATOR, UNIT_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for ResetRandomGeneratorOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("reset-random-generator expects one argument: random generator");
        let generator = args.get(0).ok_or_else(arg_error)?.into();
        let generator = Atom::as_gnd::<RandomGenerator>(generator).ok_or("set-random-seed expects a random generator as its argument")?;
        generator.reset();
        Ok(vec![UNIT_ATOM])
    }
}


// NOTE: flip is absent in Python intentionally for conversion testing
#[derive(Clone, PartialEq, Debug)]
pub struct FlipOp{}

impl Display for FlipOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
    let new_random_generator_op = Atom::gnd(NewRandomGeneratorOp{});
    tref.register_token(regex(r"new-random-generator"), move |_| { new_random_generator_op.clone() });
    let reset_random_generator_op = Atom::gnd(ResetRandomGeneratorOp{});
    tref.register_token(regex(r"reset-random-generator"), move |_| { reset_random_generator_op.clone() });
    let generator = RandomGenerator::from_os_rng();
    tref.register_token(regex(r"&rng"), move |_| { Atom::gnd(generator.clone()) });
    let flip_op = Atom::gnd(FlipOp{});
    tref.register_token(regex(r"flip"), move |_| { flip_op.clone() });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::stdlib::tests::run_program;

    #[test]
    fn metta_random() {
        assert_eq!(run_program(&format!(
            "!(chain (eval (random-int &rng 0 5)) $rint
                (and (>= $rint 0) (< $rint 5)))")),
            Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!(
            "!(assertEqual
                (random-int &rng 5 0)
                (Error (random-int &rng 5 0) RangeIsEmpty))")),
            Ok(vec![vec![UNIT_ATOM]]));
        assert_eq!(run_program(&format!(
            "!(chain (eval (random-float &rng 0.0 5.0)) $rfloat
                (and (>= $rfloat 0.0) (< $rfloat 5.0)))")),
            Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!(
            "!(assertEqual
                (random-float &rng 5 0)
                (Error (random-float &rng 5 0) RangeIsEmpty))")),
            Ok(vec![vec![UNIT_ATOM]]));

        assert_eq!(run_program(&format!("!(set-random-seed &rng 0)")), Ok(vec![vec![UNIT_ATOM]]));

        assert_eq!(run_program(&format!("
            !(bind! &newrng (new-random-generator 0)) 
            !(random-float &newrng 0 10)
        ")), run_program(&format!("
            !(bind! &newrng (new-random-generator 0)) 
            !(random-float &newrng 0 10)
        ")));

        assert_eq!(run_program(&format!(
            "!(let $newrng (new-random-generator 0)
                (let $t (set-random-seed $newrng 5)
                    (let $res_1 (random-float $newrng 0 5)
                        (let $t2 (set-random-seed $newrng 5)
                            (let $res_2 (random-float $newrng 0 5)
                                (== $res_1 $res_2))))))"
            )), Ok(vec![vec![expr!({Bool(true)})]]));

        assert_eq!(run_program(&format!(
            "!(let $seededrng (new-random-generator 0)
                (let $seededrng2 (new-random-generator 0)
                    (let $t (reset-random-generator $seededrng)
                        (let $rfloat (random-float $seededrng 0 100)
                            (let $rfloat2 (random-float $seededrng2 0 100)
                                (== $rfloat $rfloat2))))) )"
            )), Ok(vec![vec![expr!({Bool(false)})]]));

        assert_eq!(run_program(&format!("
            !(let $newrng (new-random-generator 0)
                (let $t (reset-random-generator $newrng)
                    (let $res (random-float $newrng 0 5)
                        (and (>= $res 0.0) (< $res 5.0)))))")),
            Ok(vec![vec![expr!({Bool(true)})]]));
    }

    #[test]
    fn random_op() {
        let res = RandomIntOp{}.execute(&mut vec![expr!({RandomGenerator::from_os_rng()}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0..5;
        let res_i64: i64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(range.contains(&res_i64));
        let res = RandomIntOp{}.execute(&mut vec![expr!({RandomGenerator::from_os_rng()}), expr!({Number::Integer(2)}), expr!({Number::Integer(-2)})]);
        assert_eq!(res, Err(ExecError::from("RangeIsEmpty")));

        let res = RandomFloatOp{}.execute(&mut vec![expr!({RandomGenerator::from_os_rng()}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0.0..5.0;
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(range.contains(&res_f64));
        let res = RandomFloatOp{}.execute(&mut vec![expr!({RandomGenerator::from_os_rng()}), expr!({Number::Integer(0)}), expr!({Number::Integer(0)})]);
        assert_eq!(res, Err(ExecError::from("RangeIsEmpty")));

        let gen = NewRandomGeneratorOp{}.execute(&mut vec![expr!({Number::Integer(0)})]);
        let res1 = RandomFloatOp{}.execute(&mut vec![expr!({gen}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let gen = NewRandomGeneratorOp{}.execute(&mut vec![expr!({Number::Integer(0)})]);
        let res2 = RandomFloatOp{}.execute(&mut vec![expr!({gen}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        assert_eq!(res1, res2);

        let gen = NewRandomGeneratorOp{}.execute(&mut vec![expr!({Number::Integer(0)})]);
        let _ = SetRandomSeedOp{}.execute(&mut vec![expr!({gen.clone()}), expr!({Number::Integer(0)})]);
        let res1 = RandomFloatOp{}.execute(&mut vec![expr!({gen.clone()}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let _ = SetRandomSeedOp{}.execute(&mut vec![expr!({gen.clone()}), expr!({Number::Integer(5)})]);
        let res2 = RandomFloatOp{}.execute(&mut vec![expr!({gen.clone()}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        assert_eq!(res1, res2);
    }
}
