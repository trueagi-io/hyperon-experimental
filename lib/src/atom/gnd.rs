use std::rc::Rc;

use super::*;
use crate::common::collections::ImmutableString;

/// Grounded function abstraction.
pub trait GroundedFunction {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError>;
}

impl<T: Fn(&[Atom]) -> Result<Vec<Atom>, ExecError>>  GroundedFunction for T {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        (*self)(args)
    }
}

/// Structure to wrap [GroundedFunction] instance to make an [Atom::Grounded].
pub struct GroundedFunctionAtom<T: GroundedFunction>(Rc<GroundedFunctionAtomContent<T>>);

struct GroundedFunctionAtomContent<T: GroundedFunction> {
    name: ImmutableString,
    typ: Atom,
    func: T,
}

impl<T: GroundedFunction> GroundedFunctionAtom<T> {
    /// Constructs new [GroundedFunctionAtom] instance.
    /// Name also is used to register grounded atom token, see [MettaMod::register_method].
    pub fn new(name: ImmutableString, typ: Atom, func: T) -> Self {
        Self(Rc::new(GroundedFunctionAtomContent{ name, typ, func }))
    }

    /// Returns name of the grounded function to register it as a token.
    pub fn name(&self) -> &str {
        self.0.name.as_str()
    }
}

impl<T: GroundedFunction> PartialEq for GroundedFunctionAtom<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T: GroundedFunction> std::fmt::Display for GroundedFunctionAtom<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.name)
    }
}

impl<T: GroundedFunction> std::fmt::Debug for GroundedFunctionAtom<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundedFunctionAtom[name={}, typ={:?}]", self.0.name, self.0.typ)
    }
}

impl<T: GroundedFunction> Clone for GroundedFunctionAtom<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: GroundedFunction> Grounded for GroundedFunctionAtom<T> {
    fn type_(&self) -> Atom {
        self.0.typ.clone()
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl<T: GroundedFunction> CustomExecute for GroundedFunctionAtom<T> {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        match self.0.func.execute(args) {
            Err(ExecError::Runtime(msg)) => Err(ExecError::Runtime(format!("{}: {}", self, msg))),
            Err(err) => Err(err),
            Ok(res) => Ok(res),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::metta::runner::*;

    fn execute_expr(expr: Atom) -> Result<Vec<Vec<Atom>>, String> {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.run([Atom::sym("!"), expr].as_slice())
    }

    #[test]
    fn test_lambda_into_grounded_atom() {
        let f = move |_args: &[Atom]| -> Result<Vec<Atom>, ExecError> {
            Ok(vec![Atom::sym("some-symbol")])
        };

        let op = GroundedFunctionAtom::new("get-some-symbol".into(), expr!("->" "%Undefined%"), f);

        assert_eq!(execute_expr(Atom::expr([Atom::gnd(op)])),
            Ok(vec![vec![Atom::sym("some-symbol")]]));
    }

    #[test]
    fn test_function_into_grounded_atom() {
        fn f(_args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![Atom::sym("some-symbol")])
        }

        let op = GroundedFunctionAtom::new("get-some-symbol".into(), expr!("->" "%Undefined%"), f);

        assert_eq!(execute_expr(Atom::expr([Atom::gnd(op)])),
            Ok(vec![vec![Atom::sym("some-symbol")]]));
    }

    #[test]
    fn test_method_into_grounded_atom() {
        struct S {
            sym: ImmutableString,
        }

        impl GroundedFunction for S {
            fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
                Ok(vec![Atom::sym(self.sym.as_str())])
            }
        }

        let op = GroundedFunctionAtom::new("get-some-symbol".into(), expr!("->" "%Undefined%"), S{ sym: "some-symbol".into() });

        assert_eq!(execute_expr(Atom::expr([Atom::gnd(op)])),
            Ok(vec![vec![Atom::sym("some-symbol")]]));
    }
}
