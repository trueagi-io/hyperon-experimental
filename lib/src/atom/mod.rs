// Macros to simplify expression writing

#[macro_export]
macro_rules! expr {
    () => { Atom::expr(&[]) };
    ($x:ident) => { Atom::var(stringify!($x)) };
    ($x:literal) => { Atom::sym($x) };
    ({$x:tt}) => { Atom::gnd($x) };
    (($($x:tt),*)) => { Atom::expr(&[ $( expr!($x) , )* ]) };
    ($($x:tt),*) => { Atom::expr(&[ $( expr!($x) , )* ]) };
}

pub mod matcher;
pub mod subexpr;

use std::fmt::{Display, Debug};

// Symbol atom

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolAtom {
    name: String,
}

impl From<String> for SymbolAtom {
    fn from(name: String) -> Self {
        SymbolAtom{ name }
    }
}

impl From<&str> for SymbolAtom {
    fn from(name: &str) -> Self {
        SymbolAtom::from(name.to_string())
    }
}

impl SymbolAtom {
    pub fn name(&self) -> &str {
        self.name.as_str()
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

impl From<Vec<Atom>> for ExpressionAtom {
    fn from(children: Vec<Atom>) -> Self {
        ExpressionAtom{ children }
    }
}

impl From<&[Atom]> for ExpressionAtom {
    fn from(children: &[Atom]) -> Self {
        ExpressionAtom::from(children.to_vec())
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

impl From<String> for VariableAtom {
    fn from(name: String) -> Self {
        VariableAtom{ name }
    }
}

impl From<&str> for VariableAtom {
    fn from(name: &str) -> Self {
        VariableAtom::from(name.to_string())
    }
}

impl VariableAtom {
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
pub trait GroundedAtom : Debug + mopa::Any + Sync {
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool;
    fn clone_gnd(&self) -> Box<dyn GroundedAtom>;

    fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        Err(format!("{:?} is not executable", self))
    }
    fn match_gnd(&self, other: &Atom) -> matcher::MatchResultIter {
        if let Atom::Grounded(other) = other {
            if self.eq_gnd(&**other) {
                return Box::new(std::iter::once(matcher::MatchResult::new()))
            }
        }
        Box::new(std::iter::empty())
    }
}

mopafy!(GroundedAtom);

// FIXME: one cannot implement custom execute() for the type which
// have derived Clone, PartialEq and Debug at the same time because
// it automatically have GroundedAtom implemented from the impl below.
// This is a Rust restriction: there is no method overriding and
// trait can be implemented only once for each type.

// GroundedAtom implementation for all "regular" types
// to allow using them as GroundedAtoms
// 'static is required because mopa::Any requires it
impl<T: 'static + Clone + PartialEq + Debug + Sync> GroundedAtom for T {
    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<T>() {
            Some(o) => self.eq(o),
            None => false,
        }
    }

    fn clone_gnd(&self) -> Box<dyn GroundedAtom> {
        Box::new(self.clone())
    }
}

// Grounded value

pub trait GroundedValue : mopa::Any + Debug + Sync {
    fn eq_gnd(&self, other: &dyn GroundedValue) -> bool;
    fn clone_gnd(&self) -> Box<dyn GroundedValue>;
    fn match_gnd(&self, other: &Atom) -> matcher::MatchResultIter {
        if let Atom::Value(other) = other {
            if self.eq_gnd(&**other) {
                return Box::new(std::iter::once(matcher::MatchResult::new()))
            }
        }
        Box::new(std::iter::empty())
    }
}

mopafy!(GroundedValue);

// GroundedValue implementation for all "regular" types
// to allow using them as Atoms
// 'static is required because mopa::Any requires it
impl<T: 'static + Clone + PartialEq + Debug + Sync> GroundedValue for T {
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

// Grounded function

type GroundedFunction = fn(&mut[Atom]) -> Result<Vec<Atom>, String>;

#[derive(Clone)]
pub struct FunctionAtom {
    func: GroundedFunction,
    name: String,
}

impl FunctionAtom {
    pub fn new(name: &str, func: GroundedFunction) -> Self {
        Self{ func, name: name.into() }
    }
    pub fn execute(&self, args: &mut[Atom]) -> Result<Vec<Atom>, String> {
        (self.func)(args)
    }
}

impl Debug for FunctionAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Display for FunctionAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for FunctionAtom {
    fn eq(&self, other: &Self) -> bool {
        self.func as usize == other.func as usize
            && self.name == other.name
    }
}

impl Eq for FunctionAtom {}

// Atom enum

pub enum Atom {
    Symbol(SymbolAtom),
    Expression(ExpressionAtom),
    Variable(VariableAtom),
    // We need using Box here because:
    // - we cannot use GroundedAtom because trait size is not known at compile time
    // - reference to trait does not allow heap allocated values
    // - other smart pointers like Rc doesn't allow choosing whether value should
    //   be copied or shared between two atoms when clone() is called
    Grounded(Box<dyn GroundedAtom>),

    Value(Box<dyn GroundedValue>),
    Function(FunctionAtom),
}

impl Atom {
    pub fn sym(name: &str) -> Self {
        Self::Symbol(SymbolAtom::from(name))
    }

    pub fn expr(children: &[Atom]) -> Self {
        Self::Expression(ExpressionAtom::from(children))
    }

    pub fn var(name: &str) -> Self {
        Self::Variable(VariableAtom::from(name))
    }

    pub fn gnd<T: GroundedAtom>(gnd: T) -> Atom {
        Self::Grounded(Box::new(gnd))
    }

    pub fn as_gnd<T: GroundedAtom>(&self) -> Option<&T> {
        match self {
            Atom::Grounded(gnd) => gnd.downcast_ref::<T>(),
            _ => None,
        }
    }

    pub fn as_gnd_mut<T: GroundedAtom>(&mut self) -> Option<&mut T> {
        match self {
            Atom::Grounded(gnd) => gnd.downcast_mut::<T>(),
            _ => None,
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Symbol(sym1), Atom::Symbol(sym2)) => sym1 == sym2,
            (Atom::Expression(expr1), Atom::Expression(expr2)) => expr1 == expr2,
            (Atom::Variable(var1), Atom::Variable(var2)) => var1 == var2,
            (Atom::Grounded(gnd1), Atom::Grounded(gnd2)) => gnd1.eq_gnd(&**gnd2),
            (Atom::Value(val1), Atom::Value(val2)) => val1.eq_gnd(&**val2),
            (Atom::Function(func1), Atom::Function(func2)) => func1 == func2,
             _ => false,
        }
    }
}

impl Eq for Atom {}

impl Clone for Atom {
    fn clone(&self) -> Self {
        match self {
            Atom::Symbol(sym) => Atom::Symbol(sym.clone()),
            Atom::Expression(expr) => Atom::Expression(expr.clone()),
            Atom::Variable(var) => Atom::Variable(var.clone()),
            Atom::Grounded(gnd) => Atom::Grounded((*gnd).clone_gnd()),
            Atom::Value(val) => Atom::Value((*val).clone_gnd()),
            Atom::Function(func) => Atom::Function(func.clone()),
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
            Atom::Value(val) => Debug::fmt(val, f),
            Atom::Function(func) => Display::fmt(func, f),
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
    fn S(name: &str) -> Atom { Atom::sym(name) }
    fn E(children: &[Atom]) -> Atom { Atom::expr(children) }
    fn V(name: &str) -> Atom { Atom::var(name) }
    fn G<T: GroundedAtom>(gnd: T) -> Atom { Atom::gnd(gnd) }

    #[test]
    fn test_expr_symbol() {
        assert_eq!(expr!("="), S("="));
        assert_eq!(expr!("1"), S("1"));
        assert_eq!(expr!("*"), S("*"));
        assert_eq!(expr!("foo"), S("foo"));
    }

    #[test]
    fn test_expr_variable() {
        assert_eq!(expr!(n), V("n"));
        assert_eq!(expr!(self), V("self"));
    }

    #[test]
    fn test_expr_expression() {
        assert_eq!(expr!("=", ("fact", n), ("*", n, ("-", n, "1"))), 
            E(&[S("="), E(&[S("fact"), V("n")]),
            E(&[ S("*"), V("n"), E(&[ S("-"), V("n"), S("1") ]) ]) ]));
        assert_eq!(expr!("=", n, {[1, 2, 3]}), E(&[S("="), V("n"), G([1, 2, 3])]));
        assert_eq!(expr!("=", {6}, ("fact", n)), E(&[S("="), G(6), E(&[S("fact"), V("n")])]));
    }

    #[test]
    fn test_grounded() {
        assert_eq!(Atom::gnd(3), Atom::Grounded(Box::new(3)));
        assert_eq!(G(42).as_gnd::<i32>().unwrap(), &42);
        assert_eq!(G("Data string"), Atom::Grounded(Box::new("Data string")));
        assert_eq!(G(vec![1, 2, 3]), Atom::Grounded(Box::new(vec![1, 2, 3])));
        assert_eq!(G([42, -42]).as_gnd::<[i32; 2]>().unwrap(), &[42, -42]);
        assert_eq!(G((-42, "42")).as_gnd::<(i32, &str)>().unwrap(), &(-42, "42"));
        assert_eq!(G(HashMap::from([("q", 0), ("a", 42),])),
        Atom::Grounded(Box::new(HashMap::from([("q", 0), ("a", 42),]))));
    }

    #[test]
    fn test_display_symbol() {
        assert_eq!(format!("{}", Atom::sym("test")), "test");
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
        assert_eq!(format!("{}", Atom::gnd(42)), "42");
        assert_eq!(format!("{}", Atom::gnd([1, 2, 3])), "[1, 2, 3]");
        assert_eq!(
            format!("{}", Atom::gnd(HashMap::from([("hello", "world")]))),
            "{\"hello\": \"world\"}");
    }

    #[derive(Debug)]
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

    impl GroundedAtom for TestDict {
        fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool {
            match other.downcast_ref::<TestDict>() {
                Some(o) => self.0.eq(&o.0),
                None => false,
            }
        }
        fn clone_gnd(&self) -> Box<dyn GroundedAtom> {
            Box::new(Self(self.0.clone()))
        }
        fn match_gnd(&self, other: &Atom) -> MatchResultIter {
            if let Some(other) = other.as_gnd::<TestDict>() {
                other.0.iter().map(|(ko, vo)| {
                    self.0.iter().map(|(k, v)| {
                            Atom::expr(&[k.clone(), v.clone()]).do_match(&Atom::expr(&[ko.clone(), vo.clone()]))
                    }).fold(Box::new(std::iter::empty()) as MatchResultIter, |acc, i| {
                        Box::new(acc.chain(i))
                    })
                }).fold(Box::new(std::iter::once(MatchResult::new())), |acc, i| { matcher::product_iter(acc, i) })
            } else {
                Box::new(std::iter::empty())
            }
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

    #[test]
    fn function_atom_equality() {
        fn foo(_args: &mut[Atom]) -> Result<Vec<Atom>, String> {
            Result::Err("Don't call me".into())
        }
        fn bar(_args: &mut[Atom]) -> Result<Vec<Atom>, String> {
            Result::Err("Don't call me".into())
        }
        let foo1_atom = Atom::Function(FunctionAtom::new("foo", foo));
        let foo2_atom = Atom::Function(FunctionAtom::new("foo", foo));
        let bar_atom = Atom::Function(FunctionAtom::new("bar", bar));

        assert!(foo1_atom == foo2_atom);
        assert!(foo1_atom != bar_atom);
    }

}
