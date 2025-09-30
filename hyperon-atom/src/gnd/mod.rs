pub mod str;
pub mod number;
pub mod bool;

use std::rc::Rc;

use super::*;
use hyperon_common::immutable_string::ImmutableString;

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
        self.0.func.execute(args)
    }
}

use gnd::str::*;
use gnd::number::*;
use gnd::bool::*;

/// Compares two grounded atoms for equality
pub fn gnd_eq(a: &Atom, b: &Atom) -> bool {
    // TODO: this function is a hack which is introduced for embedded grounded
    // types only. It should be replaced by some mechanism which allows defining
    // a single grounded atom equality procedure in a MeTTa module. This could
    // be done via defining a type class or equality function overloading.
    let agnd = TryInto::<&dyn GroundedAtom>::try_into(a).expect("Grounded atom is expected");
    let bgnd = TryInto::<&dyn GroundedAtom>::try_into(b).expect("Grounded atom is expected");

    if agnd.eq_gnd(bgnd) {
        return true
    }
    if agnd.type_() != bgnd.type_() {
        return false
    }
    if agnd.type_() == ATOM_TYPE_STRING {
        Str::from_atom(a) == Str::from_atom(b)
    } else if agnd.type_() == ATOM_TYPE_NUMBER {
        Number::from_atom(a) == Number::from_atom(b)
    } else if agnd.type_() == ATOM_TYPE_BOOL {
        Bool::from_atom(a) == Bool::from_atom(b)
    } else {
        false
    }
}
