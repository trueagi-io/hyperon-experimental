// Macros to simplify expression writing

/// Constructs new Atom using symplified syntax for expressions.
/// Macro has a perfomance penalty because creates and uses an additional
/// wrapper for grounded atoms. It is intended to use mainly inside of unit tests.
/// Use string literals for symbols, identifiers for variables,
/// braces for grounded symbols and S-expressions syntax for expressions.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// use hyperon::expr;
/// use hyperon::common::MUL;
///
/// let sym = expr!("A");
/// let var = expr!(x);
/// let gnd = expr!({42});
/// let expr = expr!("=" ("*2" n) ({MUL} n {2}));
///
/// assert_eq!(sym.to_string(), "A");
/// assert_eq!(var.to_string(), "$x");
/// assert_eq!(gnd.to_string(), "42");
/// assert_eq!(expr.to_string(), "(= (*2 $n) (* $n 2))");
/// ```
#[macro_export]
macro_rules! expr {
    () => { $crate::Atom::expr(vec![]) };
    ($x:ident) => { $crate::Atom::var(stringify!($x)) };
    ($x:literal) => { $crate::sym!($x) };
    ({$x:expr}) => {{
        // required to resolve ..GroundedTypeToAtom traits
        // without compiler warnings
        use $crate::*;
        (&&$crate::Wrap($x)).to_atom()
    }};
    (($($x:tt)*)) => { $crate::Atom::expr(vec![ $( expr!($x) , )* ]) };
    ($($x:tt)*) => { $crate::Atom::expr(vec![ $( expr!($x) , )* ]) };
}

/// Constructs new symbol atom. Can be used to construct `const` instances.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// use hyperon::{Atom, sym};
///
/// const SYM: Atom = sym!("const-symbol");
/// let sym = sym!("some-symbol");
///
/// assert_eq!(SYM, Atom::sym("const-symbol"));
/// assert_eq!(sym, Atom::sym("some-symbol"));
/// ```
#[macro_export]
macro_rules! sym {
    ($x:literal) => { $crate::Atom::Symbol($crate::SymbolAtom::new($crate::common::collections::ImmutableString::Literal($x))) };
}

pub mod matcher;
pub mod subexpr;

use std::any::Any;
use std::fmt::{Display, Debug};
use std::collections::HashMap;

use crate::common::collections::ImmutableString;

// Symbol atom

/// A symbol atom structure.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolAtom {
    name: ImmutableString,
}

impl SymbolAtom {
    /// Constructs new symbol from `name`. Not intended to be used directly,
    /// use [sym!] or [Atom::sym] instead.
    pub const fn new(name: ImmutableString) -> Self {
        Self{ name }
    }

    /// Returns name of the symbol.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Display for SymbolAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

// Expression atom

/// An expression atom structure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionAtom {
    children: Vec<Atom>,
}

impl ExpressionAtom {
    /// Constructs new expression from vector of sub-atoms. Not intended to be
    /// used directly, use [Atom::expr] instead.
    fn new(children: Vec<Atom>) -> Self {
        Self{ children }
    }

    /// Returns true if expression doesn't contain sub-expressions.
    pub fn is_plain(&self) -> bool {
        self.children.iter().all(|atom| ! matches!(atom, Atom::Expression(_)))
    }

    /// Returns a reference to a vector of sub-atoms.
    pub fn children(&self) -> &Vec<Atom> {
        &self.children
    }

    /// Returns a mutable reference to a vector of sub-atoms.
    pub fn children_mut(&mut self) -> &mut Vec<Atom> {
        &mut self.children
    }
}

impl Display for ExpressionAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")
            .and_then(|_| self.children.iter().take(1).fold(Ok(()),
                |res, atom| res.and_then(|_| write!(f, "{}", atom))))
            .and_then(|_| self.children.iter().skip(1).fold(Ok(()),
                |res, atom| res.and_then(|_| write!(f, " {}", atom))))
            .and_then(|_| write!(f, ")"))
    }
}

// Variable atom

use std::sync::atomic::{AtomicUsize, Ordering};

/// Global variable id counter to provide unique variable id values.
static NEXT_VARIABLE_ID: AtomicUsize = AtomicUsize::new(1);

/// Returns next unique variable id and increments the global counter.
fn next_variable_id() -> usize {
    NEXT_VARIABLE_ID.fetch_add(1, Ordering::Relaxed)
}

/// A variable atom structure
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VariableAtom {
    name: String,
    id: usize,
}

impl VariableAtom {
    /// Constructs new variable using `name` provided. Usually [Atom::var]
    /// should be preffered. But sometimes [VariableAtom] instance is required.
    /// For example as a variable bindings instance.
    pub fn new<T: Into<String>>(name: T) -> Self {
        Self{ name: name.into(), id: 0 }
    }

    // TODO: this method is likely to be removed as name is not
    // unique and it may confuse users. `to_string()` can be used instead.
    // The same may be true for a SymbolAtom.
    /// Returns name of the variable.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Returns an unique instance of the variable with the same name.
    ///
    /// # Examples
    ///
    /// ```
    /// use hyperon::VariableAtom;
    ///
    /// let x1 = VariableAtom::new("x");
    /// let x2 = x1.make_unique();
    ///
    /// assert_eq!(x2.name(), "x");
    /// assert_eq!(x1.name(), x2.name());
    /// assert_ne!(x1, x2);
    /// ```
    pub fn make_unique(&self) -> Self {
        VariableAtom{ name: self.name.clone(), id: next_variable_id() }
    }
}

impl Display for VariableAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.id == 0 {
            write!(f, "${}", self.name)
        } else {
            write!(f, "${}-{}", self.name, self.id)
        }
    }
}

/// Returns a copy of `atom` with all variables replaced by unique instances.
pub fn make_variables_unique(atom: &Atom) -> Atom {
    fn recursion(atom: &Atom, vars: &mut HashMap<VariableAtom, Atom>) -> Atom {
        match atom {
            Atom::Variable(var) => {
                if !vars.contains_key(var) {
                    vars.insert(var.clone(), Atom::Variable(var.make_unique()));
                }
                vars[var].clone()
            },
            Atom::Expression(expr) => {
                let children: Vec<Atom> = expr.children().iter()
                    .map(|atom| recursion(&atom, vars))
                    .collect();
                Atom::expr(children)
            }
            _ => atom.clone(),
        }
    }

    recursion(atom, &mut HashMap::new())
}

// Grounded atom

// FIXME: move this comment into common module documentation section
// The main idea is to keep grounded atom behaviour implementation inside
// type rather then in type instance. To allow default behaviour overriding
// two wrappers for grounded values are introduced:
// - AutoGroundedAtom<T> for intrinsic Rust types;
// - CustomGroundedAtom<T> for customized grounded types.
//
// Both of them implement GroundedAtom trait which serves for type erasure
// and has all necessary methods to implement traits required by Atom:
// PartialEq, Clone, Debug, Display. AutoGroundedAtom<T> implements
// default behaviour (match via equality, no execution) and doesn't
// expect any specific traits implemented. And CustomGroundedAtom<T> expects
// Grounded trait to be implemented and delegates calls to it.

// Both grounded atom wrappers expect grounded type implements PartialEq,
// Clone, Debug, Sync and Any traits and use them to implement eq_gnd(),
// clone_gnd() and as_any_...() methods. This allows reusing standard
// behaviour as much as possible. CustomGroundedAtom<T> also expects Display is
// implemented. AutoGroundedAtom<T> implements Display via Debug because not
// all standard Rust types implement Display (HashMap for example).
// as_any_...() method are used to transparently convert grounded atom to
// original Rust type.

// Grounded trait contains three methods to implement customized behaviour
// for grounded values:
// - type_() to return MeTTa type of the atom;
// - execute() to represent functions as atoms;
// - match_() to implement custom matching behaviour.

// match_by_equality() method allows reusing default match_() implementation in
// 3rd party code when it is not required to be customized. 

/// Grounded function execution error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExecError {
    /// Unexpected runtime error thrown by code. When [crate::metta::interpreter]
    /// algorithm receives this kind of error it interrupts the executon
    /// and returns error expression atom.
    Runtime(String),
    /// Returned intentionally to let [crate::metta::interpreter] algorithm
    /// know that this expression should be returned "as is" without reducing.
    NoReduce,
}

impl From<String> for ExecError {
    fn from(msg: String) -> Self {
        Self::Runtime(msg)
    }
}

impl From<&str> for ExecError {
    fn from(msg: &str) -> Self {
        Self::Runtime(msg.into())
    }
}

/// A trait to erase an actual type of the grounded atom. Not intended to be
/// implemented by users. Use [Atom::value] or implement [Grounded] and use
/// [Atom::gnd] instead.
pub trait GroundedAtom : mopa::Any + Debug + Display + Sync {
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool;
    fn clone_gnd(&self) -> Box<dyn GroundedAtom>;
    fn as_any_ref(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn type_(&self) -> Atom;
    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError>;
    fn match_(&self, other: &Atom) -> matcher::MatchResultIter;
}

mopafy!(GroundedAtom);

/// Trait allows implementing fully custom behaviour of the grounded atom.
/// One can use [rust_type_atom], [match_by_equality] and [execute_not_executable]
/// functions to implement default behavior. If no custom behavior
/// is required it is simpler to use [Atom::value] function for automatic
/// grounding.
///
/// # Examples
///
/// ```
/// use hyperon::*;
/// use hyperon::matcher::{Bindings, WithMatch, MatchResultIter};
/// use std::fmt::{Display, Formatter};
/// use std::iter::once;
///
/// #[derive(Debug, PartialEq, Clone)]
/// struct MyGrounded {}
///
/// impl Grounded for MyGrounded {
///     fn type_(&self) -> Atom {
///         rust_type_atom::<MyGrounded>()
///     }
///
///     fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
///         execute_not_executable(self)
///     }
///
///     fn match_(&self, other: &Atom) -> MatchResultIter {
///         match_by_equality(self, other)
///     }
/// }
///
/// impl Display for MyGrounded {
///     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
///         write!(f, "MyGrounded")
///     }
/// }
///
/// let atom = Atom::gnd(MyGrounded{});
/// let other = Atom::gnd(MyGrounded{});
/// let gnd = if let Atom::Grounded(ref gnd) = atom { gnd } else { panic!("Non grounded atom"); };
///
/// println!("{}", gnd.type_());
///
/// assert_eq!(gnd.execute(&mut vec![]), Err("Grounded atom is not executable: MyGrounded".into()));
/// assert_eq!(atom.match_(&other).collect::<Vec<Bindings>>(), vec![Bindings::new()]);
/// assert_eq!(atom, other);
/// assert_eq!(atom.to_string(), "MyGrounded");
/// assert_ne!(atom, Atom::sym("MyGrounded"));
/// ```
///
pub trait Grounded : Display {
    /// Returns type of the grounded atom. Should return same type each time
    /// it is called.
    fn type_(&self) -> Atom;
    /// Executes grounded function on passed `args` and returns list of
    /// results as `Vec<Atom>` or [ExecError].
    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError>;
    /// Implements custom matching logic of the grounded atom.
    /// Gets `other` atom as input, returns the iterator of the
    /// [matcher::Bindings] for the variables of the `other` atom.
    /// See [matcher] for detailed explanation.
    fn match_(&self, other: &Atom) -> matcher::MatchResultIter;
}

/// Returns the name of the Rust type wrapped into [Atom::Symbol]. This is a
/// default implementation of `type_()` for the grounded types wrapped
/// automatically.
pub fn rust_type_atom<T>() -> Atom {
    Atom::sym(std::any::type_name::<T>())
}

/// Returns either single emtpy [matcher::Bindings] instance if `self` and 
/// `other` are equal or empty iterator if not. This is a default 
/// implementation of `match_()` for the grounded types wrapped automatically.
pub fn match_by_equality<T: 'static + PartialEq>(this: &T, other: &Atom) -> matcher::MatchResultIter {
    match other.as_gnd::<T>() {
        Some(other) if *this == *other => Box::new(std::iter::once(matcher::Bindings::new())),
        _ => Box::new(std::iter::empty()),
    }
}

/// Returns `ExecError::Runtime` instance with message that the atom is not
/// executable. This is a default implementation of `execute()` for the
/// grounded types wrapped automatically.
pub fn execute_not_executable<T: Debug>(this: &T) -> Result<Vec<Atom>, ExecError> {
    Err(format!("Grounded atom is not executable: {:?}", this).into())
}

/// Alias for the list of traits required for the standard Rust types to be
/// automatically wrapped into [GroundedAtom].
#[doc(hidden)]
pub trait AutoGroundedType: 'static + PartialEq + Clone + Debug + Sync {}
impl<T> AutoGroundedType for T where T: 'static + PartialEq + Clone + Debug + Sync {}

/// Wrapper of the automatically implemented grounded atoms.
#[derive(PartialEq, Clone, Debug)]
struct AutoGroundedAtom<T: AutoGroundedType>(T);

impl<T: AutoGroundedType> GroundedAtom for AutoGroundedAtom<T> {
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<Self>() {
            Some(other) => self == other,
            _ => false,
        }
    }

    fn clone_gnd(&self) -> Box<dyn GroundedAtom> {
        Box::new(self.clone())
    }

    fn as_any_ref(&self) -> &dyn Any {
        &self.0
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        &mut self.0
    }

    fn type_(&self) -> Atom {
        rust_type_atom::<T>()
    }

    fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        execute_not_executable(self)
    }

    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        match_by_equality(&self.0, other)
    }
}

impl<T: AutoGroundedType> Display for AutoGroundedAtom<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

/// Alias for the list of traits required for a custom Rust grounded type
/// to be successfully wrapped into [GroundedAtom].
#[doc(hidden)]
pub trait CustomGroundedType: AutoGroundedType + Display + Grounded {}
impl<T> CustomGroundedType for T where T: AutoGroundedType + Display + Grounded {}

/// Wrapper of the custom grounded atom implementations.
#[derive(PartialEq, Clone, Debug)]
struct CustomGroundedAtom<T: CustomGroundedType>(T);

impl<T: CustomGroundedType> GroundedAtom for CustomGroundedAtom<T> {
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<Self>() {
            Some(other) => self == other,
            _ => false,
        }
    }

    fn clone_gnd(&self) -> Box<dyn GroundedAtom> {
        Box::new(CustomGroundedAtom(self.0.clone()))
    }

    fn as_any_ref(&self) -> &dyn Any {
        &self.0
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        &mut self.0
    }

    fn type_(&self) -> Atom {
        Grounded::type_(&self.0)
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        Grounded::execute(&self.0, args)
    }

    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        Grounded::match_(&self.0, other)
    }
}

impl<T: CustomGroundedType> Display for CustomGroundedAtom<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

// Convertors below implemented for macroses only. They are not effective
// because require calling Clone. In manually written code one can always
// choose more effective moving constructor.
//
// See the explanation of the trick on the link below:
// https://lukaskalbertodt.github.io/2019/12/05/generalized-autoref-based-specialization.html

/// Allows selecting between custom and automatic wrapping of the grounded
/// value. Only for using in [expr!] macro. Not intended to be used by library users.
#[doc(hidden)]
pub struct Wrap<T>(pub T);

/// Converts Rust value into grounded atom using default behaviour.
/// Only for using in [expr!] macro.  Not intended to be used by library users.
#[doc(hidden)]
pub trait AutoGroundedTypeToAtom { fn to_atom(&self) -> Atom; }
impl<T: AutoGroundedType> AutoGroundedTypeToAtom for Wrap<T> {
    fn to_atom(&self) -> Atom {
        Atom::Grounded(Box::new(AutoGroundedAtom(self.0.clone())))
    }
}

/// Converts Rust value into grounded atom using custom behaviour.
/// Only for using in [expr!] macro.  Not intended to be used by library users.
#[doc(hidden)]
pub trait CustomGroundedTypeToAtom { fn to_atom(&self) -> Atom; }
impl<T: CustomGroundedType> CustomGroundedTypeToAtom for &Wrap<T> {
    fn to_atom(&self) -> Atom {
        Atom::Grounded(Box::new(CustomGroundedAtom(self.0.clone())))
    }
}

impl PartialEq for Box<dyn GroundedAtom> {
    fn eq(&self, other: &Self) -> bool {
        self.eq_gnd(&**other)
    }
}

impl Eq for Box<dyn GroundedAtom> {}

impl Clone for Box<dyn GroundedAtom> {
    fn clone(&self) -> Self {
        self.clone_gnd()
    }
}

// Atom enum

#[derive(Clone)]
pub enum Atom {
    Symbol(SymbolAtom),
    Expression(ExpressionAtom),
    Variable(VariableAtom),
    Grounded(Box<dyn GroundedAtom>),
}

impl Atom {
    pub fn sym<T: Into<String>>(name: T) -> Self {
        Self::Symbol(SymbolAtom::new(ImmutableString::Allocated(name.into())))
    }

    pub fn expr<T: Into<Vec<Atom>>>(children: T) -> Self {
        Self::Expression(ExpressionAtom::new(children.into()))
    }

    pub fn var<T: Into<String>>(name: T) -> Self {
        Self::Variable(VariableAtom::new(name))
    }

    pub fn gnd<T: CustomGroundedType>(gnd: T) -> Atom {
        Self::Grounded(Box::new(CustomGroundedAtom(gnd)))
    }

    pub fn value<T: AutoGroundedType>(value: T) -> Atom {
        Self::Grounded(Box::new(AutoGroundedAtom(value)))
    }

    pub fn as_gnd<T: 'static>(&self) -> Option<&T> {
        match self {
            Atom::Grounded(gnd) => gnd.as_any_ref().downcast_ref::<T>(),
            _ => None,
        }
    }

    pub fn as_gnd_mut<T: 'static>(&mut self) -> Option<&mut T> {
        match self {
            Atom::Grounded(gnd) => gnd.as_any_mut().downcast_mut::<T>(),
            _ => None,
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Symbol(sym), Atom::Symbol(other)) => PartialEq::eq(sym, other),
            (Atom::Expression(expr), Atom::Expression(other)) => PartialEq::eq(expr, other),
            (Atom::Variable(var), Atom::Variable(other)) => PartialEq::eq(var, other),
            // TODO: PartialEq cannot be derived for the Box<dyn GroundedAtom>
            // because of strange compiler error which requires Copy trait
            // to be implemented. It prevents using constant atoms as patterns
            // for matching (see COMMA_SYMBOL in grounding.rs for instance).
            (Atom::Grounded(gnd), Atom::Grounded(other)) => PartialEq::eq(gnd, other),
            _ => false,
        }
    }
}

impl Eq for Atom {}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Symbol(sym) => Display::fmt(sym, f),
            Atom::Expression(expr) => Display::fmt(expr, f),
            Atom::Variable(var) => Display::fmt(var, f),
            Atom::Grounded(gnd) => Display::fmt(gnd, f),
        }
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

#[cfg(test)]
mod test {
    #![allow(non_snake_case)]

    use super::*;
    use std::collections::HashMap;

    // Expected atom constructors to make test checks
    
    #[inline]
    fn symbol(name: &'static str) -> Atom {
        Atom::Symbol(SymbolAtom::new(ImmutableString::Literal(name)))
    }

    #[inline]
    fn expression(children: Vec<Atom>) -> Atom {
        Atom::Expression(ExpressionAtom{ children })
    }

    #[inline]
    fn variable(name: &'static str) -> Atom {
        Atom::Variable(VariableAtom{ name: name.to_string(), id: 0 })
    }

    #[inline]
    fn value<T: AutoGroundedType>(value: T) -> Atom {
        Atom::Grounded(Box::new(AutoGroundedAtom(value)))
    }

    #[inline]
    fn grounded<T: CustomGroundedType>(value: T) -> Atom {
        Atom::Grounded(Box::new(CustomGroundedAtom(value)))
    }

    #[derive(PartialEq, Clone, Debug)]
    struct TestInteger(i32);

    impl Grounded for TestInteger {
        fn type_(&self) -> Atom {
            Atom::sym("Integer")
        }
        fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
            execute_not_executable(self)
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match_by_equality(self, other)
        }
    }

    impl Display for TestInteger {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[derive(PartialEq, Clone, Debug)]
    struct TestMulX(i32);

    impl Grounded for TestMulX {
        fn type_(&self) -> Atom {
            expr!("->" "i32" "i32")
        }
        fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
            Ok(vec![Atom::value(self.0 * args.get(0).unwrap().as_gnd::<i32>().unwrap())])
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match_by_equality(self, other)
        }
    }

    impl Display for TestMulX {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "x{}", self.0)
        }
    }

    #[test]
    fn test_expr_symbol() {
        assert_eq!(expr!("="), symbol("="));
        assert_eq!(expr!("1"), symbol("1"));
        assert_eq!(expr!("*"), symbol("*"));
        assert_eq!(expr!("foo"), symbol("foo"));
    }

    #[test]
    fn test_expr_variable() {
        assert_eq!(expr!(n), variable("n"));
        assert_eq!(expr!(self), variable("self"));
    }

    #[test]
    fn test_expr_grounded() {
        assert_eq!(expr!({42}), value(42));
        assert_eq!(expr!({TestInteger(42)}), grounded(TestInteger(42)));
        assert_eq!(expr!({TestMulX(3)}), grounded(TestMulX(3)));
    }

    #[test]
    fn test_expr_expression() {
        assert_eq!(expr!("=" ("fact" n) ("*" n ("-" n "1"))), 
            expression(vec![symbol("="), expression(vec![symbol("fact"), variable("n")]),
            expression(vec![symbol("*"), variable("n"),
            expression(vec![symbol("-"), variable("n"), symbol("1") ]) ]) ]));
        assert_eq!(expr!("=" n {[1, 2, 3]}),
            expression(vec![symbol("="), variable("n"), value([1, 2, 3])]));
        assert_eq!(expr!("=" {6} ("fact" n)),
            expression(vec![symbol("="), value(6), expression(vec![symbol("fact"), variable("n")])]));
        assert_eq!(expr!({TestMulX(3)} {TestInteger(6)}),
            expression(vec![grounded(TestMulX(3)), grounded(TestInteger(6))]));
    }

    #[test]
    fn test_grounded() {
        assert_eq!(Atom::value(3), value(3));
        assert_eq!(Atom::value(42).as_gnd::<i32>().unwrap(), &42);
        assert_eq!(Atom::value("Data string"), value("Data string"));
        assert_eq!(Atom::value(vec![1, 2, 3]), value(vec![1, 2, 3]));
        assert_eq!(Atom::value([42, -42]).as_gnd::<[i32; 2]>().unwrap(), &[42, -42]);
        assert_eq!(Atom::value((-42, "42")).as_gnd::<(i32, &str)>().unwrap(), &(-42, "42"));
        assert_eq!(Atom::value(HashMap::from([("q", 0), ("a", 42),])),
            value(HashMap::from([("q", 0), ("a", 42),])));
        assert_eq!(Atom::gnd(TestInteger(42)), grounded(TestInteger(42)));
        assert_eq!(Atom::gnd(TestInteger(42)).as_gnd::<i32>(), None);
        assert_eq!(Atom::gnd(TestInteger(42)).as_gnd::<TestInteger>(), Some(&TestInteger(42)));
    }

    #[test]
    fn test_display_atom() {
        assert_eq!(format!("{}", Atom::Symbol(SymbolAtom::new(ImmutableString::Literal("test")))), "test");
        assert_eq!(format!("{}", Atom::Symbol(SymbolAtom::new(ImmutableString::Allocated("test".into())))), "test");
        assert_eq!(format!("{}", Atom::var("x")), "$x");
        assert_eq!(format!("{}", Atom::value(42)), "42");
        assert_eq!(format!("{}", Atom::value([1, 2, 3])), "[1, 2, 3]");
        assert_eq!(format!("{}", Atom::value(HashMap::from([("hello", "world")]))),
            "{\"hello\": \"world\"}");
        assert_eq!(format!("{}", Atom::gnd(TestInteger(42))), "42");
        assert_eq!(format!("{}", Atom::gnd(TestMulX(3))), "x3");
        assert_eq!(format!("{}", expr!("=" ("fact" n) ("*" n ("-" n "1")))),
            "(= (fact $n) (* $n (- $n 1)))");
        assert_eq!(format!("{}", expr!()), "()");
    }

    #[ignore = "Interpret plan printing cannot be easily implemented using Display trait"]
    #[test]
    fn test_debug_atom() {
        assert_eq!(format!("{:?}", Atom::sym("test")), "Symbol(SymbolAtom { name: \"test\" })");
        assert_eq!(format!("{:?}", Atom::var("x")), "Variable(VariableAtom { name: \"x\" })");
        assert_eq!(format!("{:?}", Atom::value(42)), "Grounded(AutoGroundedAtom(42))");
        assert_eq!(format!("{:?}", Atom::value([1, 2, 3])), "Grounded(AutoGroundedAtom([1, 2, 3]))");
        assert_eq!(format!("{:?}", Atom::value(HashMap::from([("hello", "world")]))),
            "Grounded(AutoGroundedAtom({\"hello\": \"world\"}))");
        assert_eq!(format!("{:?}", Atom::gnd(TestInteger(42))), "Grounded(CustomGroundedAtom(TestInteger(42)))");
        assert_eq!(format!("{:?}", Atom::gnd(TestMulX(3))), "Grounded(CustomGroundedAtom(TestMulX(3)))");
    }

    #[test]
    fn test_clone_atom() {
        assert_eq!(Atom::sym("test").clone(), symbol("test"));
        assert_eq!(Atom::var("x").clone(), variable("x"));
        assert_eq!(Atom::value(HashMap::from([("hello", "world")])).clone(),
            value(HashMap::from([("hello", "world")])));
        assert_eq!(Atom::gnd(TestMulX(3)).clone(), grounded(TestMulX(3)));
        assert_eq!(Atom::expr([Atom::sym("="), Atom::value(6),
            Atom::expr([Atom::sym("fact"), Atom::var("n")])]).clone(),
            expression(vec![symbol("="), value(6),
                expression(vec![symbol("fact"), variable("n")])]));
    }

    #[test]
    fn test_custom_type() {
        let atom = Atom::value(42);
        if let Atom::Grounded(gnd) = atom {
            assert_eq!(gnd.type_(), Atom::sym("i32"));
        } else {
            assert!(false, "GroundedAtom is expected");
        }

        let atom = Atom::gnd(TestInteger(42));
        if let Atom::Grounded(gnd) = atom {
            assert_eq!(gnd.type_(), Atom::sym("Integer"));
        } else {
            assert!(false, "GroundedAtom is expected");
        }
    }

    #[test]
    fn test_custom_execution() {
        let mul3 = Atom::gnd(TestMulX(3));
        if let Atom::Grounded(gnd) = mul3 {
            let res = gnd.execute(&mut vec![Atom::value(14)]);
            assert_eq!(res, Ok(vec![Atom::value(42)]));
        } else {
            assert!(false, "GroundedAtom is expected");
        }
    }

}
