use std::collections::HashMap;

macro_rules! expr {
    () => {};
    ($x:ident) => { Atom::var(stringify!($x)) };
    ($x:literal) => { Atom::sym($x) };
    (($($x:tt),*)) => { Atom::expr(&[ $( expr!($x) , )* ]) };
    ($($x:tt),*) => { Atom::expr(&[ $( expr!($x) , )* ]) };
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Hash)]
#[derive(PartialEq)]
#[derive(Eq)]
struct VariableAtom {
    name: String,
}

impl VariableAtom {
    fn from(name: &str) -> VariableAtom {
        VariableAtom{ name: name.to_string() }
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
enum Atom {
    Symbol{ symbol: String },
    Expression{ children: Vec<Atom> },
    Variable(VariableAtom),
    Grounded,
}

impl Atom {
    fn sym(name: &str) -> Self {
        Self::Symbol{ symbol: name.to_string() }
    }

    fn expr(children: &[Atom]) -> Self {
        Self::Expression{ children: children.to_vec() }
    }

    fn var(name: &str) -> Self {
        Self::Variable(VariableAtom::from(name))
    }
}

type Bindings = HashMap<VariableAtom, Atom>;

fn check_and_insert_binding(bindings: &mut Bindings, var: &VariableAtom,
        value: &Atom) -> bool{
    match bindings.get(var) {
        Some(prev) => prev == value,
        None => {
            bindings.insert(var.clone(), value.clone());
            true
        }
    }
}

fn match_atoms_recursively(a: &Atom, b: &Atom,
        a_bindings: &mut Bindings, b_bindings: &mut Bindings) -> bool {
    match (a, b) {
        (Atom::Symbol{ symbol: a }, Atom::Symbol{ symbol: b }) => a == b,
        (a, Atom::Variable(v)) => check_and_insert_binding(b_bindings, v, a),
        (Atom::Variable(v), b) => check_and_insert_binding(a_bindings, v, b),
        (Atom::Expression{ children: a }, Atom::Expression{ children: b }) => {
            if a.len() != b.len() {
                false
            } else {
                a.iter().zip(b.iter()).fold(true,
                    |succ, pair| succ && match_atoms_recursively(pair.0, pair.1,
                        a_bindings, b_bindings))
            }
        },
        _ => false,
    }
}

fn match_atoms(a: &Atom, b: &Atom) -> Option<(Bindings, Bindings)> {
    let mut a_bindings = Bindings::new();
    let mut b_bindings = Bindings::new();
    if match_atoms_recursively(a, b, &mut a_bindings, &mut b_bindings) {
        Some((a_bindings, b_bindings))
    } else {
        None
    }
}


struct GroundingSpace {
    content: Vec<Atom>,
}

impl GroundingSpace {

    fn new() -> GroundingSpace {
        GroundingSpace{ content: Vec::new() }
    }
    
    fn add(&mut self, atom: Atom) {
        self.content.push(atom)
    }

    fn query(&self, pattern: &Atom) -> Vec<Bindings> {
        let mut result = Vec::new();
        for next in &self.content {
            match match_atoms(next, pattern) {
                Some((a_bindings, b_bindings)) => result.push(b_bindings),
                None => continue,
            }
        }
        result
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    // Aliases to have a shorter notation
    fn S(name: &str) -> Atom { Atom::sym(name) }
    fn E(children: &[Atom]) -> Atom { Atom::expr(children) }
    fn V(name: &str) -> Atom { Atom::var(name) }

    // Make Bindings map from list of (k, v) pairs
    macro_rules! bind {
        ($($k:ident: $v:expr),*) => { vec![$( (VariableAtom::from(stringify!($k)), $v), )*]
            .iter().cloned().collect() };
    }

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
    fn test_expr_self_expression() {
        assert_eq!(expr!("=", ("fact", n), ("*", n, ("-", n, "1"))), 
                   E(&[S("="), E(&[S("fact"), V("n")]),
                   E(&[ S("*"), V("n"), E(&[ S("-"), V("n"), S("1") ]) ]) ]));
    }

    #[test]
    fn test_match_symbol() {
        let mut space = GroundingSpace::new();
        space.add(expr!("foo"));
        assert_eq!(space.query(&expr!("foo")), vec![bind!{}])
    }

    #[test]
    fn test_match_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("foo"));
        assert_eq!(space.query(&expr!(x)), vec![bind!{x: expr!("foo")}])
    }

    #[test]
    fn test_match_expression() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+", "a", ("*", "b", "c")));
        assert_eq!(space.query(&expr!("+", "a", ("*", "b", "c"))), vec![bind!{}])
    }

    #[test]
    fn test_match_expression_with_variables() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+", "A", ("*", "B", "C")));
        assert_eq!(space.query(&expr!("+", a, ("*", b, c))),
            vec![bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }])
    }

    #[test]
    fn test_match_different_value_for_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+", "A", ("*", "B", "C")));
        assert_eq!(space.query(&expr!("+", a, ("*", a, c))), vec![])
    }

    #[test]
    fn test_match_variables_in_data() {
        assert_eq!(
            match_atoms(&expr!("+", a, ("*", b, c)), &expr!("+", "A", ("*", "B", "C"))),
            Some((bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }, bind!{})))
    }

    #[test]
    fn test_match_different_value_for_variable_in_data() {
        assert_eq!(
            match_atoms(&expr!("+", a, ("*", a, c)), &expr!("+", "A", ("*", "B", "C"))),
            None)
    }
}
