// Macros to simplify expression writing

#[macro_export]
macro_rules! expr {
    () => { Atom::expr(vec![]) };
    ($x:ident) => { Atom::var(stringify!($x)) };
    ($x:literal) => { sym!($x) };
    ({$x:tt}) => { (&&Wrap($x)).to_atom() };
    (($($x:tt),*)) => { Atom::expr(vec![ $( expr!($x) , )* ]) };
    ($($x:tt),*) => { Atom::expr(vec![ $( expr!($x) , )* ]) };
}

#[macro_export]
macro_rules! sym {
    ($x:literal) => { Atom::sym($x) };
}

pub mod matcher;
pub mod subexpr;

use std::any::Any;
use std::fmt::{Display, Debug};

// Symbol atom

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolAtom {
    name: String,
}

impl SymbolAtom {
    fn new(name: String) -> Self {
        Self{ name }
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

impl Display for SymbolAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

// Expression atom

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionAtom {
    children: Vec<Atom>,
}

impl ExpressionAtom {
    fn new(children: Vec<Atom>) -> Self {
        Self{ children }
    }

    pub fn is_plain(&self) -> bool {
        self.children.iter().all(|atom| ! matches!(atom, Atom::Expression(_)))
    }

    pub fn children(&self) -> &Vec<Atom> {
        &self.children
    }

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VariableAtom {
    name: String,
}

impl VariableAtom {
    pub fn new<T: Into<String>>(name: T) -> Self {
        Self{ name: name.into() }
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Display for VariableAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.name)
    }
}

// Grounded atom

// The main idea is to keep grounded atom behaviour implementation inside
// type rather then in type instance. To allow default behaviour overriding
// two wrappers for grounded values are introduced:
// - DefaultGrounded<T> for intrinsic Rust types;
// - CustomGrounded<T> for customized grounded types.
//
// Both of them implement GroundedAtom trait which serves for type erasure
// and has all necessary methods to implement traits required by Atom:
// PartialEq, Clone, Debug, Display. DefaultGrounded<T> implements
// default behaviour (match via equality, no execution) and doesn't
// expect any specific traits implemented. And CustomGrounded<T> expects
// Grounded trait to be implemented and delegates calls to it.

// Both grounded atom wrappers expect grounded type implements PartialEq,
// Clone, Debug, Sync and Any traits and use them to implement eq_gnd(),
// clone_gnd() and as_any_...() methods. This allows reusing standard
// behaviour as much as possible. CustomGrounded<T> also expects Display is
// implemented. DefaultGrounded<T> implements Display via Debug because not
// all standard Rust types implement Display (HashMap for example).
// as_any_...() method are used to transparently convert grounded atom to
// original Rust type.

// Grounded trait contains three methods to implement customized behaviour
// for grounded values:
// - type_() to return MeTTa type of the atom;
// - execute() to represent functions as atoms;
// - match_() to implement custom matching behaviour.

// match_by_equality() method allows reusing default match_() implementation in
// 3rd party code when it is not needed to be customized. 

// FIXME: try implementing eq_gnd/clone_gnd and as_any_ref on the trait level
// FIXME: replace list of traits by alias trait in all locations
pub trait GroundedAtom : mopa::Any + Debug + Display + Sync {
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool;
    fn clone_gnd(&self) -> Box<dyn GroundedAtom>;
    fn as_any_ref(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn type_(&self) -> Atom;
    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String>;
    fn match_(&self, other: &Atom) -> matcher::MatchResultIter;
}

mopafy!(GroundedAtom);

pub trait Grounded : Display {
    fn type_(&self) -> Atom;
    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String>;
    fn match_(&self, other: &Atom) -> matcher::MatchResultIter;
}

pub fn match_by_equality<T: 'static + PartialEq>(this: &T, other: &Atom) -> matcher::MatchResultIter {
    match other {
        Atom::Grounded(other) => {
            if let Some(other) = other.as_any_ref().downcast_ref::<T>() {
                if *this == *other {
                    Box::new(std::iter::once(matcher::MatchResult::new()))
                } else {
                    Box::new(std::iter::empty())
                }
            } else {
                Box::new(std::iter::empty())
            }
        },
        _ => Box::new(std::iter::empty()),
    }
}

pub fn execute_not_executable<T: Debug>(this: &T) -> Result<Vec<Atom>, String> {
    Err(format!("Grounded type is not executable: {:?}", this))
}

#[derive(PartialEq, Clone, Debug)]
struct DefaultGrounded<T: 'static + PartialEq + Clone + Debug + Sync>(T);

impl<T: 'static + PartialEq + Clone + Debug + Sync> GroundedAtom for DefaultGrounded<T> {
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
        Atom::sym(std::any::type_name::<T>())
    }

    fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        execute_not_executable(self)
    }

    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        match_by_equality(&self.0, other)
    }
}

impl<T: 'static + PartialEq + Clone + Debug + Sync> Display for DefaultGrounded<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

#[derive(PartialEq, Clone, Debug)]
struct CustomGrounded<T: 'static + PartialEq + Clone + Debug + Display + Sync + Grounded>(T);

impl<T: 'static + PartialEq + Clone + Debug + Display + Sync + Grounded> GroundedAtom for CustomGrounded<T> {
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<Self>() {
            Some(other) => self == other,
            _ => false,
        }
    }

    fn clone_gnd(&self) -> Box<dyn GroundedAtom> {
        Box::new(CustomGrounded(self.0.clone()))
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

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        Grounded::execute(&self.0, args)
    }

    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        Grounded::match_(&self.0, other)
    }
}

impl<T: 'static + PartialEq + Clone + Debug + Display + Sync + Grounded> Display for CustomGrounded<T> {
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

pub struct Wrap<T>(pub T);

pub trait DefaultGroundedToAtom { fn to_atom(&self) -> Atom; }
impl<T: 'static + PartialEq + Clone + Debug + Sync> DefaultGroundedToAtom for Wrap<T> {
    fn to_atom(&self) -> Atom {
        Atom::Grounded(Box::new(DefaultGrounded(self.0.clone())))
    }
}

pub trait CustomGroundedToAtom { fn to_atom(&self) -> Atom; }
impl<T: 'static + PartialEq + Clone + Debug + Display + Sync + Grounded> CustomGroundedToAtom for &Wrap<T> {
    fn to_atom(&self) -> Atom {
        Atom::Grounded(Box::new(CustomGrounded(self.0.clone())))
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

pub fn rust_type_atom<T>() -> Atom {
    Atom::sym(std::any::type_name::<T>())
}

// Atom enum

#[derive(Clone, Debug)]
pub enum Atom {
    Symbol(SymbolAtom),
    Expression(ExpressionAtom),
    Variable(VariableAtom),
    Grounded(Box<dyn GroundedAtom>),
}

impl Atom {
    pub fn sym<T: Into<String>>(name: T) -> Self {
        Self::Symbol(SymbolAtom::new(name.into()))
    }

    pub fn expr<T: Into<Vec<Atom>>>(children: T) -> Self {
        Self::Expression(ExpressionAtom::new(children.into()))
    }

    pub fn var<T: Into<String>>(name: T) -> Self {
        Self::Variable(VariableAtom::new(name))
    }

    pub fn gnd<T: 'static + PartialEq + Clone + Debug + Display + Sync + Grounded>(gnd: T) -> Atom {
        Self::Grounded(Box::new(CustomGrounded(gnd)))
    }

    pub fn value<T: 'static + PartialEq + Clone + Debug + Sync>(value: T) -> Atom {
        Self::Grounded(Box::new(DefaultGrounded(value)))
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

#[cfg(test)]
mod test {
    #![allow(non_snake_case)]

    use super::*;
    use std::collections::HashMap;

    // Expected atom constructors to make test checks
    
    #[inline]
    fn symbol(name: &'static str) -> Atom {
        Atom::Symbol(SymbolAtom{ name: name.to_string() })
    }

    #[inline]
    fn expression(children: Vec<Atom>) -> Atom {
        Atom::Expression(ExpressionAtom{ children })
    }

    #[inline]
    fn variable(name: &'static str) -> Atom {
        Atom::Variable(VariableAtom{ name: name.to_string() })
    }

    #[inline]
    fn value<T: 'static + PartialEq + Clone + Debug + Sync>(value: T) -> Atom {
        Atom::Grounded(Box::new(DefaultGrounded(value)))
    }

    #[inline]
    fn gnd<T: 'static + PartialEq + Clone + Debug + Display + Sync + Grounded>(value: T) -> Atom {
        Atom::Grounded(Box::new(CustomGrounded(value)))
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
    fn test_expr_expression() {
        assert_eq!(expr!("=", ("fact", n), ("*", n, ("-", n, "1"))), 
            expression(vec![symbol("="), expression(vec![symbol("fact"), variable("n")]),
            expression(vec![symbol("*"), variable("n"),
            expression(vec![symbol("-"), variable("n"), symbol("1") ]) ]) ]));
        assert_eq!(expr!("=", n, {[1, 2, 3]}),
            expression(vec![symbol("="), variable("n"), value([1, 2, 3])]));
        assert_eq!(expr!("=", {6}, ("fact", n)),
            expression(vec![symbol("="), value(6), expression(vec![symbol("fact"), variable("n")])]));
    }

    // FIXME: write tests for Atom PartialEq, Clone, Display, Debug for all
    // kinds of atoms
    #[derive(PartialEq, Clone, Debug)]
    struct TestGrounded(i32);

    impl Grounded for TestGrounded {
        fn type_(&self) -> Atom {
            Atom::sym("Integer")
        }
        fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
            execute_not_executable(self)
        }
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            match_by_equality(self, other)
        }
    }

    impl Display for TestGrounded {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
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
        assert_eq!(Atom::gnd(TestGrounded(42)), gnd(TestGrounded(42)));
    }

    #[test]
    fn test_grounded_type() {
        let atom = Atom::value(42);
        if let Atom::Grounded(gnd) = atom {
            assert_eq!(gnd.type_(), Atom::sym("i32"));
        } else {
            assert!(false, "GroundedAtom is expected");
        }

        let atom = Atom::gnd(TestGrounded(42));
        if let Atom::Grounded(gnd) = atom {
            assert_eq!(gnd.type_(), Atom::sym("Integer"));
        } else {
            assert!(false, "GroundedAtom is expected");
        }
    }

    #[test]
    fn test_display_symbol() {
        assert_eq!(format!("{}", sym!("test")), "test");
    }

    #[test]
    fn test_display_variable() {
        assert_eq!(format!("{}", Atom::var("x")), "$x");
    }

    #[test]
    fn test_display_expression() {
        assert_eq!(format!("{}", expr!("=", ("fact", n), ("*", n, ("-", n, "1")))),
        "(= (fact $n) (* $n (- $n 1)))");
        assert_eq!(format!("{}", expr!()), "()");
    }

    #[test]
    fn test_display_grounded() {
        assert_eq!(format!("{}", Atom::value(42)), "42");
        assert_eq!(format!("{}", Atom::value([1, 2, 3])), "[1, 2, 3]");
        assert_eq!(format!("{}", Atom::value(HashMap::from([("hello", "world")]))),
            "{\"hello\": \"world\"}");
    }

    #[test]
    fn test_debug_grounded() {
        assert_eq!(format!("{:?}", Atom::value(42)), "Grounded(DefaultGrounded(42))");
        assert_eq!(format!("{:?}", Atom::value([1, 2, 3])), "Grounded(DefaultGrounded([1, 2, 3]))");
        assert_eq!(format!("{:?}", Atom::value(HashMap::from([("hello", "world")]))),
            "Grounded(DefaultGrounded({\"hello\": \"world\"}))");
    }

    #[derive(PartialEq, Clone, Debug)]
    struct TestDict(Vec<(Atom, Atom)>);

    impl TestDict {
        fn new() -> Self {
            TestDict(Vec::new())
        }

        fn get(&self, key: &Atom) -> Option<&Atom> {
            self.0.iter().filter(|(k, _)| { k == key }).nth(0).map(|(_, v)| { v })
        }
        fn remove(&mut self, key: &Atom) -> Option<Atom> {
            let v = self.get(key).map(Atom::clone);
            self.0 = self.0.drain(..).filter(|(k, _)| { k != key }).collect();
            v
        }
        fn put(&mut self, key: Atom, value: Atom) -> Option<Atom> {
            let v = self.remove(&key);
            self.0.push((key, value));
            v
        }
    }

    use crate::*;
    use crate::atom::matcher::*;

    impl Grounded for TestDict {
        fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
            if let Some(other) = other.as_gnd::<TestDict>() {
                other.0.iter().map(|(ko, vo)| {
                    self.0.iter().map(|(k, v)| {
                        Atom::expr(vec![k.clone(), v.clone()]).do_match(&Atom::expr(vec![ko.clone(), vo.clone()]))
                    }).fold(Box::new(std::iter::empty()) as MatchResultIter, |acc, i| {
                        Box::new(acc.chain(i))
                    })
                }).fold(Box::new(std::iter::once(MatchResult::new())), |acc, i| { matcher::product_iter(acc, i) })
            } else {
                Box::new(std::iter::empty())
            }
        }
        fn type_(&self) -> Atom {
            Atom::sym("Dict")
        }

        fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
            execute_not_executable(self)
        }
    }
    
    impl Display for TestDict {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    #[test]
    fn test_custom_matching() {
        let mut dict = TestDict::new();
        dict.put(expr!("x"), expr!({2}, {5}));
        dict.put(expr!("y"), expr!({5}));
        let dict = expr!({dict}); 

        let mut query = TestDict::new();
        query.put(expr!(b), expr!(y));
        query.put(expr!(a), expr!({2}, y));
        let query = expr!({query});

        let result: Vec<MatchResult> = dict.do_match(&query).collect();
        assert_eq!(result, vec![MatchResult::from((bind!{},
                    bind!{y: expr!({5}), b: expr!("y"), a: expr!("x")}))]);
    }

}
