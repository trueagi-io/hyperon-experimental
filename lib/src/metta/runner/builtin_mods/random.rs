
use hyperon_atom::*;
use crate::metta::*;
use crate::metta::text::SExprParser;
use hyperon_atom::gnd::number::*;
use hyperon_atom::gnd::bool::*;

use std::fmt::{Display, Formatter};
use std::cell::RefCell;
use rand::{Rng, SeedableRng, rngs::StdRng};
use std::rc::Rc;
use hyperon_atom::gnd::GroundedFunctionAtom;
use crate::metta::runner::modules::{MettaMod, ModuleLoader};
use crate::metta::runner::{Metta, RunContext};

use regex::Regex;
//TODO: In the current version of rand it is possible for rust to hang if range end's value is too
// big. In future releases (0.9+) of rand signature of sample_single will be changed and it will be
// possible to use match construction to cover overflow and other errors. So after library will be
// upgraded RandomInt and RandomFloat codes should be altered.
// see comment https://github.com/trueagi-io/hyperon-experimental/pull/791#discussion_r1824355414

pub static RANDOM_METTA: &'static str = include_str!("random.metta");

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


#[derive(Debug)]
pub(crate) struct RandomModLoader;

impl ModuleLoader for RandomModLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        // Initialize module's space
        let space = GroundingSpace::new();
        context.init_self_module(space.into(), None);

        // Load module's tokens
        let _ = self.load_tokens(context.module(), context.metta.clone())?;

        // Parse MeTTa code of the module
        let parser = SExprParser::new(RANDOM_METTA);
        context.push_parser(Box::new(parser));

        Ok(())
    }

    fn load_tokens(&self, target: &MettaMod, _metta: Metta) -> Result<(), String> {
        let mut tref = target.tokenizer().borrow_mut();

        tref.register_function(GroundedFunctionAtom::new(
            r"random-int".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_RANDOM_GENERATOR, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER]),
            random_int));

        tref.register_function(GroundedFunctionAtom::new(
            r"random-float".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_RANDOM_GENERATOR, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER]),
            random_float));

        tref.register_function(GroundedFunctionAtom::new(
            r"set-random-seed".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_RANDOM_GENERATOR, ATOM_TYPE_NUMBER, UNIT_TYPE]),
            set_random_seed));

        tref.register_function(GroundedFunctionAtom::new(
            r"new-random-generator".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_RANDOM_GENERATOR]),
            new_random_generator));

        tref.register_function(GroundedFunctionAtom::new(
            r"reset-random-generator".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_RANDOM_GENERATOR, UNIT_ATOM]),
            reset_random_generator));

        tref.register_function(GroundedFunctionAtom::new(
            r"flip".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_BOOL]),
            flip));

        let generator = Atom::gnd(RandomGenerator::from_os_rng());
        tref.register_token(Regex::new(r"&rng").unwrap(),move |_| { generator.clone() });

        Ok(())
    }
}


fn random_int(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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

fn random_float(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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

fn set_random_seed(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("set-random-seed expects two arguments: random generator and number (seed)");
    let generator = args.get(0).ok_or_else(arg_error)?.into();
    let seed: i64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
    let generator = Atom::as_gnd::<RandomGenerator>(generator).ok_or("set-random-seed expects a random generator as its argument")?;
    generator.reseed_from_u64(seed as u64);
    Ok(vec![UNIT_ATOM])
}

fn new_random_generator(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("new-random-generator expects one argument: number (seed)");
    let seed: i64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
    let new_generator = RandomGenerator::from_seed_u64(seed as u64);
    Ok(vec![Atom::gnd(new_generator)])
}

fn reset_random_generator(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("reset-random-generator expects one argument: random generator");
    let generator = args.get(0).ok_or_else(arg_error)?.into();
    let generator = Atom::as_gnd::<RandomGenerator>(generator).ok_or("set-random-seed expects a random generator as its argument")?;
    generator.reset();
    Ok(vec![UNIT_ATOM])
}

// NOTE: flip is absent in Python intentionally for conversion testing
fn flip(_args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    Ok(vec![Atom::gnd(Bool(rand::random()))])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::run_program;

    #[test]
    fn metta_random() {
        assert_eq!(run_program(&format!(
            "!(import! &self random)
             !(let $rint (random-int &rng 0 5)
                (and (>= $rint 0) (< $rint 5)))")),
            Ok(vec![vec![UNIT_ATOM], vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!(
            "!(import! &self random)
             !(assertEqual
                (random-int &rng 5 0)
                (Error (random-int &rng 5 0) RangeIsEmpty))")),
            Ok(vec![vec![UNIT_ATOM], vec![UNIT_ATOM]]));
        assert_eq!(run_program(&format!(
            "!(import! &self random)
             !(let $rfloat (random-float &rng 0.0 5.0)
                (and (>= $rfloat 0.0) (< $rfloat 5.0)))")),
            Ok(vec![vec![UNIT_ATOM], vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!(
            "!(import! &self random)
             !(assertEqual
                (random-float &rng 5 0)
                (Error (random-float &rng 5 0) RangeIsEmpty))")),
            Ok(vec![vec![UNIT_ATOM], vec![UNIT_ATOM]]));

        assert_eq!(run_program(&format!("
            !(import! &self random)
            !(set-random-seed &rng 0)")),
            Ok(vec![vec![UNIT_ATOM], vec![UNIT_ATOM]]));

        assert_eq!(run_program(&format!("
            !(import! &self random)
            !(bind! &newrng (new-random-generator 0)) 
            !(random-float &newrng 0 10)
        ")), run_program(&format!("
            !(import! &self random)
            !(bind! &newrng (new-random-generator 0)) 
            !(random-float &newrng 0 10)
        ")));

        assert_eq!(run_program(&format!(
            "!(import! &self random)
             !(let $newrng (new-random-generator 0)
                (let $t (set-random-seed $newrng 5)
                    (let $res_1 (random-float $newrng 0 5)
                        (let $t2 (set-random-seed $newrng 5)
                            (let $res_2 (random-float $newrng 0 5)
                                (== $res_1 $res_2))))))"
            )), Ok(vec![vec![UNIT_ATOM], vec![expr!({Bool(true)})]]));

        assert_eq!(run_program(&format!(
            "!(import! &self random)
             !(let $seededrng (new-random-generator 0)
                (let $seededrng2 (new-random-generator 0)
                    (let $t (reset-random-generator $seededrng)
                        (let $rfloat (random-float $seededrng 0 100)
                            (let $rfloat2 (random-float $seededrng2 0 100)
                                (== $rfloat $rfloat2))))) )"
            )), Ok(vec![vec![UNIT_ATOM], vec![expr!({Bool(false)})]]));

        assert_eq!(run_program(&format!("
            !(import! &self random)
            !(let $newrng (new-random-generator 0)
                (let $t (reset-random-generator $newrng)
                    (let $res (random-float $newrng 0 5)
                        (and (>= $res 0.0) (< $res 5.0)))))")),
            Ok(vec![vec![UNIT_ATOM], vec![expr!({Bool(true)})]]));
    }

    #[test]
    fn random_op() {
        let res = random_int(&mut vec![expr!({RandomGenerator::from_os_rng()}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0..5;
        let res_i64: i64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(range.contains(&res_i64));
        let res = random_int(&mut vec![expr!({RandomGenerator::from_os_rng()}), expr!({Number::Integer(2)}), expr!({Number::Integer(-2)})]);
        assert_eq!(res, Err(ExecError::from("RangeIsEmpty")));

        let res = random_float(&mut vec![expr!({RandomGenerator::from_os_rng()}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0.0..5.0;
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(range.contains(&res_f64));
        let res = random_float(&mut vec![expr!({RandomGenerator::from_os_rng()}), expr!({Number::Integer(0)}), expr!({Number::Integer(0)})]);
        assert_eq!(res, Err(ExecError::from("RangeIsEmpty")));

        let gen = new_random_generator(&mut vec![expr!({Number::Integer(0)})]);
        let res1 = random_float(&mut vec![expr!({gen}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let gen = new_random_generator(&mut vec![expr!({Number::Integer(0)})]);
        let res2 = random_float(&mut vec![expr!({gen}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        assert_eq!(res1, res2);

        let gen = new_random_generator(&mut vec![expr!({Number::Integer(0)})]);
        let _ = set_random_seed(&mut vec![expr!({gen.clone()}), expr!({Number::Integer(0)})]);
        let res1 = random_float(&mut vec![expr!({gen.clone()}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let _ = set_random_seed(&mut vec![expr!({gen.clone()}), expr!({Number::Integer(5)})]);
        let res2 = random_float(&mut vec![expr!({gen.clone()}), expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        assert_eq!(res1, res2);
    }
}
