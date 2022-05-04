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

use std::fmt::{Display, Debug};
use std::sync::Arc;

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

// Sync is required to make creating static Atom declarations possible
pub trait GroundedValue : mopa::Any + Sync + Send + Debug {
    fn eq_gnd(&self, other: &dyn GroundedValue) -> bool;
    fn clone_gnd(&self) -> Box<dyn GroundedValue>;
}

mopafy!(GroundedValue);

// GroundedValue implementation for all "regular" types
// to allow using them as GroundedAtoms
// 'static is required because mopa::Any requires it
impl<T: 'static + Clone + PartialEq + Debug + Sync + Send> GroundedValue for T {
    fn eq_gnd(&self, other: &dyn GroundedValue) -> bool {
        match other.downcast_ref::<T>() {
            Some(o) => self.eq(o),
            None => false,
        }
    }

    fn clone_gnd(&self) -> Box<dyn GroundedValue> {
        Box::new(self.clone())
    }
}

// see https://lukaskalbertodt.github.io/2019/12/05/generalized-autoref-based-specialization.html
pub struct Wrap<T>(pub T);

pub trait GroundedValueToGroundedAtom { fn to_atom(&self) -> Atom; }
impl<T: GroundedValue> GroundedValueToGroundedAtom for Wrap<T> {
    fn to_atom(&self) -> Atom {
        Atom::Grounded(GroundedAtom{
            value: self.0.clone_gnd(),
            do_match: Arc::new(default_match),
            do_execute: Box::new(DEFAULT_EXECUTE)
        })
    }
}

pub trait ExplicitToGroundedAtom { fn to_atom(&self) -> Atom; }
impl<T: Clone + Into<Atom>> ExplicitToGroundedAtom for &Wrap<T> {
    fn to_atom(&self) -> Atom {
        self.0.clone().into()
    }
}

// See https://users.rust-lang.org/t/lifetime-error-while-trying-to-simulate-trait-alias-in-a-stable-version-of-compiler/73912/2
//pub trait MatchFn : 'static + Sync + Send + Fn(&dyn GroundedValue, &Atom) -> matcher::MatchResultIter {}
//impl<T: 'static + Sync + Send + Fn(&dyn GroundedValue, &Atom) -> matcher::MatchResultIter> MatchFn for T {}

fn default_match(this: &dyn GroundedValue, other: &Atom) -> matcher::MatchResultIter {
    match other {
        Atom::Grounded(other) if other.value.as_ref().eq_gnd(this) =>
            Box::new(std::iter::once(matcher::MatchResult::new())),
        _ => Box::new(std::iter::empty()),
    }
}

// See https://users.rust-lang.org/t/lifetime-error-while-trying-to-simulate-trait-alias-in-a-stable-version-of-compiler/73912/2
//pub trait ExecuteFn : 'static + Sync + Send + Fn(&mut Vec<Atom>) -> Result<Vec<Atom>, String> {}
//impl<T: 'static + Sync + Send + Fn(&mut Vec<Atom>) -> Result<Vec<Atom>, String>> ExecuteFn for T {}

fn default_execute_impl(_args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
    Err(format!("Execute is not implemented"))
}
static DEFAULT_EXECUTE: ExecuteFn = default_execute_impl;

// Grounded function

pub trait GroundedFunction : mopa::Any + Sync + Send {
    fn eq_trait(&self, other: &dyn GroundedFunction) -> bool;
    fn clone_trait(&self) -> Box<dyn GroundedFunction>;
    fn call(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String>;
}

mopafy!(GroundedFunction);

type ExecuteFn = fn(&mut Vec<Atom>) -> Result<Vec<Atom>, String>;

impl GroundedFunction for ExecuteFn {
    fn eq_trait(&self, other: &dyn GroundedFunction) -> bool {
        match other.downcast_ref::<ExecuteFn>() {
            Some(other) => (*self as usize) == (*other as usize),
            _ => false,
        }
    }

    fn clone_trait(&self) -> Box<dyn GroundedFunction> {
        Box::new(self.clone())
    }

    fn call(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        (*self)(args)
    }
}

pub struct GroundedAtom {
    // We need using Box here because:
    // - we cannot use GroundedValue because trait size is not known at compile time
    // - reference to trait does not allow heap allocated values
    // - other smart pointers like Rc doesn't allow choosing whether value should
    //   be copied or shared between two atoms when clone() is called
    value: Box<dyn GroundedValue>,
    do_match: Arc<dyn Sync + Send + Fn(&dyn GroundedValue, &Atom) -> matcher::MatchResultIter>,
    do_execute: Box<dyn GroundedFunction>,
}

impl GroundedAtom {
    pub fn new_value<T: GroundedValue>(value: T) -> Self {
        Self{ value: Box::new(value), do_match: Arc::new(default_match), do_execute: Box::new(DEFAULT_EXECUTE) }
    }
    pub fn new_matchable<T: GroundedValue, M: 'static + Sync + Send + Fn(&dyn GroundedValue, &Atom) -> matcher::MatchResultIter>(value: T, do_match: M) -> Self {
        Self{ value: Box::new(value), do_match: Arc::new(do_match), do_execute: Box::new(DEFAULT_EXECUTE) }
    }
    pub fn new_function<T: GroundedValue, E: 'static + GroundedFunction>(value: T, do_execute: E) -> Self {
        Self{ value: Box::new(value), do_match: Arc::new(default_match), do_execute: Box::new(do_execute) }
    }

    pub fn do_match(&self, other: &Atom) -> matcher::MatchResultIter {
        (self.do_match)(self.value.as_ref(), other)
    }

    pub fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        self.do_execute.call(args)
    }
    pub fn downcast_ref<T: GroundedValue>(&self) -> Option<&T> {
        self.value.downcast_ref::<T>()
    }

    pub fn downcast_mut<T: GroundedValue>(&mut self) -> Option<&mut T> {
        self.value.downcast_mut::<T>()
    }
}

impl PartialEq for GroundedAtom {
    fn eq(&self, other: &Self) -> bool {
        // TODO: it is temporal implementation before GroundedAtom is splitted
        // on GroundedValue and GroundedFunction
        self.value.eq_gnd(&*other.value)
            && self.do_execute.eq_trait(&*other.do_execute)
    }
}

impl Eq for GroundedAtom {}

impl Clone for GroundedAtom {
    fn clone(&self) -> Self {
        Self{
            value: self.value.clone_gnd(),
            do_match: self.do_match.clone(),
            do_execute: self.do_execute.clone_trait(),
        }
    }
}

impl Display for GroundedAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&*self.value, f)
    }
}

impl Debug for GroundedAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

// Atom enum

#[derive(PartialEq, Eq, Clone)]
pub enum Atom {
    Symbol(SymbolAtom),
    Expression(ExpressionAtom),
    Variable(VariableAtom),
    Grounded(GroundedAtom),
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

    pub fn value<T: GroundedValue>(value: T) -> Atom {
        Self::Grounded(GroundedAtom::new_value(value))
    }

    pub fn as_gnd<T: GroundedValue>(&self) -> Option<&T> {
        match self {
            Atom::Grounded(gnd) => gnd.value.downcast_ref::<T>(),
            _ => None,
        }
    }

    pub fn as_gnd_mut<T: GroundedValue>(&mut self) -> Option<&mut T> {
        match self {
            Atom::Grounded(gnd) => gnd.value.downcast_mut::<T>(),
            _ => None,
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Symbol(sym) => Display::fmt(sym, f),
            Atom::Expression(expr) => Display::fmt(expr, f),
            Atom::Variable(var) => Display::fmt(var, f),
            Atom::Grounded(gnd) => Debug::fmt(gnd, f),
        }
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

#[cfg(test)]
mod test {
    #![allow(non_snake_case)]

    use super::*;
    use std::collections::HashMap;

    // Aliases to have a shorter notation
    fn G<T: GroundedValue>(gnd: T) -> Atom { Atom::value(gnd) }

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
            expression(vec![symbol("="), variable("n"), G([1, 2, 3])]));
        assert_eq!(expr!("=", {6}, ("fact", n)),
            expression(vec![symbol("="), G(6), expression(vec![symbol("fact"), variable("n")])]));
    }

    #[test]
    fn test_grounded() {
        assert_eq!(Atom::value(3), Atom::Grounded(GroundedAtom::new_value(3)));
        assert_eq!(G(42).as_gnd::<i32>().unwrap(), &42);
        assert_eq!(G("Data string"), Atom::Grounded(GroundedAtom::new_value("Data string")));
        assert_eq!(G(vec![1, 2, 3]), Atom::Grounded(GroundedAtom::new_value(vec![1, 2, 3])));
        assert_eq!(G([42, -42]).as_gnd::<[i32; 2]>().unwrap(), &[42, -42]);
        assert_eq!(G((-42, "42")).as_gnd::<(i32, &str)>().unwrap(), &(-42, "42"));
        assert_eq!(G(HashMap::from([("q", 0), ("a", 42),])),
            Atom::Grounded(GroundedAtom::new_value(HashMap::from([("q", 0), ("a", 42),]))));
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
        assert_eq!(
            format!("{}", Atom::value(HashMap::from([("hello", "world")]))),
            "{\"hello\": \"world\"}");
    }

    #[derive(Debug, Clone)]
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

    impl GroundedValue for TestDict {
        fn eq_gnd(&self, other: &dyn GroundedValue) -> bool {
            match other.downcast_ref::<TestDict>() {
                Some(o) => self.0.eq(&o.0),
                None => false,
            }
        }
        fn clone_gnd(&self) -> Box<dyn GroundedValue> {
            Box::new(Self(self.0.clone()))
        }
    }

    fn test_dict_match(this: &dyn GroundedValue, other: &Atom) -> MatchResultIter {
        if let (Some(this), Some(other)) = (this.downcast_ref::<TestDict>(), other.as_gnd::<TestDict>()) {
            other.0.iter().map(|(ko, vo)| {
                this.0.iter().map(|(k, v)| {
                        Atom::expr(vec![k.clone(), v.clone()]).do_match(&Atom::expr(vec![ko.clone(), vo.clone()]))
                }).fold(Box::new(std::iter::empty()) as MatchResultIter, |acc, i| {
                    Box::new(acc.chain(i))
                })
            }).fold(Box::new(std::iter::once(MatchResult::new())), |acc, i| { matcher::product_iter(acc, i) })
        } else {
            Box::new(std::iter::empty())
        }
    }

    impl From<TestDict> for Atom {
        fn from(dict: TestDict) -> Self {
            Atom::Grounded(GroundedAtom::new_matchable(dict, test_dict_match))
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
