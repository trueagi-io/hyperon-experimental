macro_rules! expr {
    () => {};
    ($x:ident) => { Atom::var(stringify!($x)) };
    ($x:literal) => { Atom::sym($x) };
    (($($x:tt),*)) => { Atom::expr(&[ $( expr!($x) , )* ]) };
    ($($x:tt),*) => { Atom::expr(&[ $( expr!($x) , )* ]) };
}

#[derive(Debug)]
enum Atom {
    Symbol{ symbol: String },
    Expression{ children: Vec<Atom> },
    Variable{ name: String },
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
        Self::Variable{ name: name.to_string() }
    }
}

impl Clone for Atom {
    fn clone(&self) -> Self {
        match self {
            Self::Symbol {symbol: s} => Self::Symbol{ symbol: s.clone() },
            Self::Expression {children: c} => Self::Expression{ children: c.clone() },
            Self::Variable {name: n} => Self::Variable{ name: n.clone() },
            Self::Grounded => Self::Grounded,
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Symbol {symbol: s}, Self::Symbol {symbol: o}) => s == o,
            (Self::Expression {children: c},  Self::Expression{ children: o }) => c == o,
            (Self::Variable {name: n}, Self::Variable{ name: o }) => n == o,
            (Self::Grounded, Self::Grounded) => true,
            _ => false,
        }
    }
}

struct GroundingSpace {
    content: Vec<Atom>,
}

impl GroundingSpace {

    fn empty() -> GroundingSpace {
        GroundingSpace{ content: Vec::new() }
    }
    
    fn match_to(&self, pattern: Atom) -> Atom {
        self.content[0].clone()
    }

    fn add_atom(&mut self, atom: Atom) {
        self.content.push(atom)
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr_macros() {
        // Aliases to have a shorter notation
        fn S(name: &str) -> Atom { Atom::sym(name) }
        fn E(children: &[Atom]) -> Atom { Atom::expr(children) }
        fn V(name: &str) -> Atom { Atom::var(name) }

        assert_eq!(expr!("=", ("fact", n), ("*", n, ("-", n, "1"))), 
                   E(&[S("="), E(&[S("fact"), V("n")]),
                   E(&[ S("*"), V("n"), E(&[ S("-"), V("n"), S("1") ]) ]) ]));
    }

}
