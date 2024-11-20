use crate::*;
use crate::metta::*;
#[cfg(feature = "pkg_mgmt")]

use std::convert::TryInto;

use crate::metta::runner::arithmetics::*;
use crate::metta::runner::stdlib_minimal::grounded_op;

#[derive(Clone, Debug)]
pub struct PowMathOp {}
grounded_op!(PowMathOp, "pow-math");
impl Grounded for PowMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }
    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}
impl CustomExecute for PowMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("pow-math expects two arguments: number (base) and number (power)");
        let base: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let pow = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?;
        let res = match pow {
            Number::Integer(n) => {
                match TryInto::<i32>::try_into(n) {
                    Ok(n) => base.powi(n),
                    Err(_) => return Err(ExecError::from("power argument is too big, try using float value")),
                }
            },
            Number::Float(f) => base.powf(f),
        };
        Ok(vec![Atom::gnd(Number::Float(res))])
    }
}

#[derive(Clone, Debug)]
pub struct SqrtMathOp {}

grounded_op!(SqrtMathOp, "sqrt-math");

impl Grounded for SqrtMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SqrtMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("sqrt-math expects one argument: number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.sqrt()))])
    }
}

#[derive(Clone, Debug)]
pub struct AbsMathOp {}

grounded_op!(AbsMathOp, "abs-math");

impl Grounded for AbsMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AbsMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("abs-math expects one argument: number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.abs()))])
    }
}

#[derive(Clone, Debug)]
pub struct LogMathOp {}

grounded_op!(LogMathOp, "log-math");

impl Grounded for LogMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for LogMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("log-math expects two arguments: base (number) and input value (number)");
        let base: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let input: f64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.log(base)))])
    }
}

#[derive(Clone, Debug)]
pub struct TruncMathOp {}

grounded_op!(TruncMathOp, "trunc-math");

impl Grounded for TruncMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for TruncMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("trunc-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.trunc()))])
    }
}

#[derive(Clone, Debug)]
pub struct CeilMathOp {}

grounded_op!(CeilMathOp, "ceil-math");

impl Grounded for CeilMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CeilMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("ceil-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.ceil()))])
    }
}

#[derive(Clone, Debug)]
pub struct FloorMathOp {}

grounded_op!(FloorMathOp, "floor-math");

impl Grounded for FloorMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FloorMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("floor-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.floor()))])
    }
}

#[derive(Clone, Debug)]
pub struct RoundMathOp {}

grounded_op!(RoundMathOp, "round-math");

impl Grounded for RoundMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RoundMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("round-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.round()))])
    }
}

#[derive(Clone, Debug)]
pub struct SinMathOp {}

grounded_op!(SinMathOp, "sin-math");

impl Grounded for SinMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SinMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("sin-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.sin()))])
    }
}

#[derive(Clone, Debug)]
pub struct AsinMathOp {}

grounded_op!(AsinMathOp, "asin-math");

impl Grounded for AsinMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AsinMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("asin-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.asin()))])
    }
}

#[derive(Clone, Debug)]
pub struct CosMathOp {}

grounded_op!(CosMathOp, "cos-math");

impl Grounded for CosMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CosMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("cos-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.cos()))])
    }
}

#[derive(Clone, Debug)]
pub struct AcosMathOp {}

grounded_op!(AcosMathOp, "acos-math");

impl Grounded for AcosMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AcosMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("acos-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.acos()))])
    }
}

#[derive(Clone, Debug)]
pub struct TanMathOp {}

grounded_op!(TanMathOp, "tan-math");

impl Grounded for TanMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for TanMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("tan-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.tan()))])
    }
}

#[derive(Clone, Debug)]
pub struct AtanMathOp {}

grounded_op!(AtanMathOp, "atan-math");

impl Grounded for AtanMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AtanMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("atan-math expects one argument: input number");
        let input: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        Ok(vec![Atom::gnd(Number::Float(input.atan()))])
    }
}

#[derive(Clone, Debug)]
pub struct IsNanMathOp {}

grounded_op!(IsNanMathOp, "isnan-math");

impl Grounded for IsNanMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IsNanMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("isnan-math expects one argument: input number");
        let input = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?;
        let res = match input {
            Number::Integer(_) => false,
            Number::Float(f) => f.is_nan(),
        };
        Ok(vec![Atom::gnd(Bool(res))])
    }
}

#[derive(Clone, Debug)]
pub struct IsInfMathOp {}

grounded_op!(IsInfMathOp, "isinf-math");

impl Grounded for IsInfMathOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IsInfMathOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("isinf-math expects one argument: input number");
        let input = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?;
        let res = match input {
            Number::Integer(_) => false,
            Number::Float(f) => f.is_infinite(),
        };
        Ok(vec![Atom::gnd(Bool(res))])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::text::SExprParser;
    use crate::metta::runner::EnvBuilder;
    use crate::metta::runner::Metta;

    fn run_program(program: &str) -> Result<Vec<Vec<Atom>>, String> {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.run(SExprParser::new(program))
    }

    #[test]
    fn metta_pow_math() {
        assert_eq!(run_program(&format!("!(pow-math 5 2)")), Ok(vec![vec![expr!({Number::Integer(5_i64.pow(2))})]]));
        assert_eq!(run_program(&format!("!(pow-math 5 200000000000000)")), Ok(vec![vec![expr!("Error" ({ PowMathOp{} } {Number::Integer(5)} {Number::Integer(200000000000000)}) "power argument is too big, try using float value")]]));
        assert_eq!(run_program(&format!("!(pow-math 5.5 2.3)")), Ok(vec![vec![expr!({Number::Float(5.5_f64.powf(2.3))})]]));
        assert_eq!(run_program(&format!("!(pow-math A 2)")), Ok(vec![vec![expr!("Error" ({ PowMathOp{} } "A" {Number::Integer(2)}) "pow-math expects two arguments: number (base) and number (power)")]]));
    }

    #[test]
    fn metta_sqrt_math() {
        assert_eq!(run_program(&format!("!(sqrt-math 4)")), Ok(vec![vec![expr!({Number::Integer(2)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (sqrt-math -4)) $sqrt (isnan-math $sqrt))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(sqrt-math A)")), Ok(vec![vec![expr!("Error" ({ SqrtMathOp{} } "A") "sqrt-math expects one argument: number")]]));
    }

    #[test]
    fn metta_abs_math() {
        assert_eq!(run_program(&format!("!(abs-math 4)")), Ok(vec![vec![expr!({Number::Integer(4)})]]));
        assert_eq!(run_program(&format!("!(abs-math -5)")), Ok(vec![vec![expr!({Number::Integer(5)})]]));
        assert_eq!(run_program(&format!("!(abs-math A)")), Ok(vec![vec![expr!("Error" ({ AbsMathOp{} } "A") "abs-math expects one argument: number")]]));
    }

    #[test]
    fn metta_log_math() {
        assert_eq!(run_program(&format!("!(log-math 2 4)")), Ok(vec![vec![expr!({Number::Integer(2)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (log-math 0 0)) $log (isnan-math $log))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (log-math 5 0)) $log (isinf-math $log))")), Ok(vec![vec![expr!({Bool(true)})]]));
    }

    #[test]
    fn metta_trunc_math() {
        assert_eq!(run_program(&format!("!(trunc-math 2.4)")), Ok(vec![vec![expr!({Number::Integer(2)})]]));
        assert_eq!(run_program(&format!("!(trunc-math A)")), Ok(vec![vec![expr!("Error" ({ TruncMathOp{} } "A") "trunc-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_ceil_math() {
        assert_eq!(run_program(&format!("!(ceil-math 2.4)")), Ok(vec![vec![expr!({Number::Integer(3)})]]));
        assert_eq!(run_program(&format!("!(ceil-math -2.4)")), Ok(vec![vec![expr!({Number::Integer(-2)})]]));
        assert_eq!(run_program(&format!("!(ceil-math A)")), Ok(vec![vec![expr!("Error" ({ CeilMathOp{} } "A") "ceil-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_floor_math() {
        assert_eq!(run_program(&format!("!(floor-math 2.4)")), Ok(vec![vec![expr!({Number::Integer(2)})]]));
        assert_eq!(run_program(&format!("!(floor-math -2.4)")), Ok(vec![vec![expr!({Number::Integer(-3)})]]));
        assert_eq!(run_program(&format!("!(floor-math A)")), Ok(vec![vec![expr!("Error" ({ FloorMathOp{} } "A") "floor-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_round_math() {
        assert_eq!(run_program(&format!("!(round-math 2.4)")), Ok(vec![vec![expr!({Number::Integer(2)})]]));
        assert_eq!(run_program(&format!("!(round-math -2.7)")), Ok(vec![vec![expr!({Number::Integer(-3)})]]));
        assert_eq!(run_program(&format!("!(round-math A)")), Ok(vec![vec![expr!("Error" ({ RoundMathOp{} } "A") "round-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_sin_math() {
        assert_eq!(run_program(&format!("!(sin-math 0)")), Ok(vec![vec![expr!({Number::Integer(0)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (sin-math 1.570796327)) $sin (< (- $sin 1.0) 1e-10))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(sin-math A)")), Ok(vec![vec![expr!("Error" ({ SinMathOp{} } "A") "sin-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_asin_math() {
        assert_eq!(run_program(&format!("!(asin-math 0)")), Ok(vec![vec![expr!({Number::Integer(0)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (sin-math 1)) $sin (< (- (asin-math $sin) 1.0) 1e-10))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(asin-math A)")), Ok(vec![vec![expr!("Error" ({ AsinMathOp{} } "A") "asin-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_cos_math() {
        assert_eq!(run_program(&format!("!(cos-math 0)")), Ok(vec![vec![expr!({Number::Integer(1)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (cos-math 1.570796327)) $cos (< (- $cos 0.0) 1e-10))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(cos-math A)")), Ok(vec![vec![expr!("Error" ({ CosMathOp{} } "A") "cos-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_acos_math() {
        assert_eq!(run_program(&format!("!(acos-math 1)")), Ok(vec![vec![expr!({Number::Integer(0)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (cos-math 1)) $cos (< (- (acos-math $cos) 1.0) 1e-10))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(acos-math A)")), Ok(vec![vec![expr!("Error" ({ AcosMathOp{} } "A") "acos-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_tan_math() {
        assert_eq!(run_program(&format!("!(tan-math 0)")), Ok(vec![vec![expr!({Number::Integer(0)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (tan-math 0.78539816339)) $tan (< (- $tan 1.0) 1e-10))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(tan-math A)")), Ok(vec![vec![expr!("Error" ({ TanMathOp{} } "A") "tan-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_atan_math() {
        assert_eq!(run_program(&format!("!(atan-math 0)")), Ok(vec![vec![expr!({Number::Integer(0)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (atan-math 1)) $atan (< (- $atan 0.78539816339) 1e-10))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(atan-math A)")), Ok(vec![vec![expr!("Error" ({ AtanMathOp{} } "A") "atan-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_isnan_math() {
        assert_eq!(run_program(&format!("!(isnan-math 0)")), Ok(vec![vec![expr!({Bool(false)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (log-math 0 0)) $log (isnan-math $log))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(isnan-math A)")), Ok(vec![vec![expr!("Error" ({ IsNanMathOp{} } "A") "isnan-math expects one argument: input number")]]));
    }

    #[test]
    fn metta_isinf_math() {
        assert_eq!(run_program(&format!("!(isinf-math 0)")), Ok(vec![vec![expr!({Bool(false)})]]));
        assert_eq!(run_program(&format!("!(chain (eval (log-math 5 0)) $log (isinf-math $log))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(isinf-math A)")), Ok(vec![vec![expr!("Error" ({ IsInfMathOp{} } "A") "isinf-math expects one argument: input number")]]));
    }

    #[test]
    fn pow_math_op() {
        let res = PowMathOp {}.execute(&mut vec![expr!({Number::Integer(5)}), expr!({Number::Integer(2)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(5_i64.pow(2))})]);
        let res = PowMathOp {}.execute(&mut vec![expr!({Number::Integer(2)}), expr!({Number::Integer(200000000000000)})]);
        assert_eq!(res, Err(ExecError::from("power argument is too big, try using float value")));
        let res = PowMathOp {}.execute(&mut vec![expr!({Number::Float(5.5)}), expr!({Number::Float(2.3)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Float(5.5_f64.powf(2.3))})]);
        let res = PowMathOp {}.execute(&mut vec![expr!("A"), expr!({Number::Integer(2)})]);
        assert_eq!(res, Err(ExecError::from("pow-math expects two arguments: number (base) and number (power)")));
    }

    #[test]
    fn sqrt_math_op() {
        let res = SqrtMathOp {}.execute(&mut vec![expr!({Number::Integer(4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(2)})]);
        let res = SqrtMathOp {}.execute(&mut vec![expr!({Number::Integer(-4)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(res_f64.is_nan());
        let res = SqrtMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("sqrt-math expects one argument: number")));
    }

    #[test]
    fn abs_math_op() {
        let res = AbsMathOp {}.execute(&mut vec![expr!({Number::Integer(4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(4)})]);
        let res = AbsMathOp {}.execute(&mut vec![expr!({Number::Integer(-4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(4)})]);
        let res = AbsMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("abs-math expects one argument: number")));
    }

    #[test]
    fn log_math_op() {
        let res = LogMathOp {}.execute(&mut vec![expr!({Number::Integer(2)}), expr!({Number::Integer(4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(2)})]);
        let res = LogMathOp {}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(0)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(res_f64.is_nan());
        let res = LogMathOp {}.execute(&mut vec![expr!({Number::Integer(5)}), expr!({Number::Integer(0)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(res_f64.is_infinite());
        let res = LogMathOp {}.execute(&mut vec![expr!({Number::Integer(2)}), expr!("A")]);
        assert_eq!(res, Err(ExecError::from("log-math expects two arguments: base (number) and input value (number)")));
    }

    #[test]
    fn trunc_math_op() {
        let res = TruncMathOp {}.execute(&mut vec![expr!({Number::Float(2.4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(2)})]);
        let res = TruncMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("trunc-math expects one argument: input number")));
    }

    #[test]
    fn ceil_math_op() {
        let res = CeilMathOp {}.execute(&mut vec![expr!({Number::Float(2.4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(3)})]);
        let res = CeilMathOp {}.execute(&mut vec![expr!({Number::Float(-2.4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(-2)})]);
        let res = CeilMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("ceil-math expects one argument: input number")));
    }

    #[test]
    fn floor_math_op() {
        let res = FloorMathOp {}.execute(&mut vec![expr!({Number::Float(2.4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(2)})]);
        let res = FloorMathOp {}.execute(&mut vec![expr!({Number::Float(-2.4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(-3)})]);
        let res = FloorMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("floor-math expects one argument: input number")));
    }

    #[test]
    fn round_math_op() {
        let res = RoundMathOp {}.execute(&mut vec![expr!({Number::Float(2.4)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(2)})]);
        let res = RoundMathOp {}.execute(&mut vec![expr!({Number::Float(-2.7)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(-3)})]);
        let res = RoundMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("round-math expects one argument: input number")));
    }

    #[test]
    fn sin_math_op() {
        let res = SinMathOp {}.execute(&mut vec![expr!({Number::Integer(0)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(0)})]);
        let res = SinMathOp {}.execute(&mut vec![expr!({Number::Float(std::f64::consts::FRAC_PI_2)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        let abs_difference = (res_f64 - 1.0).abs();
        assert!(abs_difference < 1e-10);
        let res = SinMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("sin-math expects one argument: input number")));
    }

    #[test]
    fn asin_math_op() {
        let res = AsinMathOp {}.execute(&mut vec![expr!({Number::Integer(0)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(0)})]);
        let res = AsinMathOp {}.execute(&mut vec![expr!({Number::Float(1.0)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        let abs_difference = (res_f64 - std::f64::consts::FRAC_PI_2).abs();
        assert!(abs_difference < 1e-10);
        let res = AsinMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("asin-math expects one argument: input number")));
    }

    #[test]
    fn cos_math_op() {
        let res = CosMathOp {}.execute(&mut vec![expr!({Number::Integer(0)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(1)})]);
        let res = CosMathOp {}.execute(&mut vec![expr!({Number::Float(std::f64::consts::FRAC_PI_2)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        let abs_difference = (res_f64 - 0.0).abs();
        assert!(abs_difference < 1e-10);
        let res = CosMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("cos-math expects one argument: input number")));
    }

    #[test]
    fn acos_math_op() {
        let res = AcosMathOp {}.execute(&mut vec![expr!({Number::Integer(1)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(0)})]);
        let res = AcosMathOp {}.execute(&mut vec![expr!({Number::Integer(0)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        let abs_difference = (res_f64 - std::f64::consts::FRAC_PI_2).abs();
        assert!(abs_difference < 1e-10);
        let res = AcosMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("acos-math expects one argument: input number")));
    }

    #[test]
    fn tan_math_op() {
        let res = TanMathOp {}.execute(&mut vec![expr!({Number::Integer(0)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(0)})]);
        let res = TanMathOp {}.execute(&mut vec![expr!({Number::Float(std::f64::consts::FRAC_PI_4)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        let abs_difference = (res_f64 - 1.0).abs();
        assert!(abs_difference < 1e-10);
        let res = TanMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("tan-math expects one argument: input number")));
    }

    #[test]
    fn atan_math_op() {
        let res = AtanMathOp {}.execute(&mut vec![expr!({Number::Integer(0)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(0)})]);
        let res = AtanMathOp {}.execute(&mut vec![expr!({Number::Integer(1)})]);
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        let abs_difference = (res_f64 - std::f64::consts::FRAC_PI_4).abs();
        assert!(abs_difference < 1e-10);
        let res = AtanMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("atan-math expects one argument: input number")));
    }

    #[test]
    fn isnan_math_op() {
        let res = IsNanMathOp {}.execute(&mut vec![expr!({Number::Integer(0)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Bool(false)})]);
        let res = IsNanMathOp {}.execute(&mut vec![expr!({Number::Float(f64::NAN)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Bool(true)})]);
        let res = IsNanMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("isnan-math expects one argument: input number")));
    }

    #[test]
    fn isinf_math_op() {
        let res = IsInfMathOp {}.execute(&mut vec![expr!({Number::Integer(0)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Bool(false)})]);
        let res = IsInfMathOp {}.execute(&mut vec![expr!({Number::Float(f64::INFINITY)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Bool(true)})]);
        let res = IsInfMathOp {}.execute(&mut vec![expr!("A")]);
        assert_eq!(res, Err(ExecError::from("isinf-math expects one argument: input number")));
    }
}