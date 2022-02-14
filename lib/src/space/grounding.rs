use crate::*;
use crate::atom::*;
use crate::atom::matcher::Bindings;
use crate::atom::subexpr::split_expr;

use std::fmt::{Display, Debug};

// Grounding space

pub type Unifications = Vec<matcher::UnificationPair>;

use std::rc::Rc;

// TODO: Clone is required by C API
#[derive(Clone)]
pub struct GroundingSpace {
    content: Rc<Vec<Atom>>,
}

impl GroundingSpace {

    pub fn new() -> Self {
        Self{ content: Rc::new(Vec::new()) }
    }
    
    pub fn add(&mut self, atom: Atom) {
        Rc::get_mut(&mut self.content).expect("Cannot mutate shared atomspace").push(atom)
    }

    pub fn query(&self, pattern: &Atom) -> Vec<Bindings> {
        match split_expr(pattern) {
            Some((Atom::Symbol(sym), args)) if *sym == SymbolAtom::from(",") => {
                args.fold(vec![bind!{}],
                    |acc, pattern| {
                        if acc.is_empty() {
                            acc
                        } else {
                            let res = self.query(pattern);
                            Self::merge_results(acc, res)
                        }
                    })
            },
            _ => self.single_query(pattern),
        }
    }

    fn merge_results(prev: Vec<Bindings>, next: Vec<Bindings>) -> Vec<Bindings> {
        prev.iter().flat_map(|p| -> Vec<Option<Bindings>> {
            next.iter().map(|n| Bindings::merge_bindings(p, n)).collect()
        }).filter(Option::is_some).map(Option::unwrap).collect()
    }
    
    fn single_query(&self, pattern: &Atom) -> Vec<Bindings> {
        log::debug!("single_query: pattern: {}", pattern);
        let mut result = Vec::new();
        for next in &(*self.content) {
            match matcher::match_atoms(next, pattern) {
                Some(res) => {
                    let bindings = matcher::apply_bindings_to_bindings(&res.candidate_bindings, &res.pattern_bindings);
                    if let Ok(bindings) = bindings {
                        // TODO: implement Display for bindings
                        log::debug!("single_query: push result: {}, bindings: {:?}", next, bindings);
                        result.push(bindings);
                    }
                },
                None => continue,
            }
        }
        result
    }

    pub fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        self.query(pattern).drain(0..)
            .map(| bindings | matcher::apply_bindings_to_atom(template, &bindings))
            .collect()
    }

    // TODO: for now we have separate methods query() and unify() but
    // they probably can be merged. One way of doing it is designating
    // in the query which part of query should be unified and which matched.
    // For example for the typical query in a form (= (+ a b) $X) the
    // (= (...) $X) level should not be unified otherwise we will recursively
    // infer that we need calculating (+ a b) again which is equal to original
    // query. Another option is designating this in the data itself.
    // After combining match and unification we could leave only single
    // universal method.
    pub fn unify(&self, pattern: &Atom) -> Vec<(Bindings, Unifications)> {
        log::debug!("unify: pattern: {}", pattern);
        let mut result = Vec::new();
        for next in &(*self.content) {
            match matcher::unify_atoms(next, pattern) {
                Some(res) => {
                    let bindings = matcher::apply_bindings_to_bindings(&res.candidate_bindings, &res.pattern_bindings);
                    if let Ok(bindings) = bindings {
                        // TODO: implement Display for bindings
                        log::debug!("unify: push result: {}, bindings: {:?}", next, bindings);
                        result.push((bindings, res.unifications));
                    }
                },
                None => continue,
            }
        }
        result
    }

    pub fn as_vec(&self) -> &Vec<Atom> {
        &self.content
    }

    pub fn atom_iter(&self) -> std::slice::Iter<Atom>{
        self.content.iter()
    }
}

impl PartialEq for GroundingSpace {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl Debug for GroundingSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundingSpace")
    }
}

impl Display for GroundingSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GroundingSpace")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_match_symbol() {
        let mut space = GroundingSpace::new();
        space.add(expr!("foo"));
        assert_eq!(space.query(&expr!("foo")), vec![bind!{}]);
    }

    #[test]
    fn test_match_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("foo"));
        assert_eq!(space.query(&expr!(x)), vec![bind!{x: expr!("foo")}]);
    }

    #[test]
    fn test_match_expression() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+", "a", ("*", "b", "c")));
        assert_eq!(space.query(&expr!("+", "a", ("*", "b", "c"))), vec![bind!{}]);
    }

    #[test]
    fn test_match_expression_with_variables() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+", "A", ("*", "B", "C")));
        assert_eq!(space.query(&expr!("+", a, ("*", b, c))),
        vec![bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }]);
    }

    #[test]
    fn test_match_different_value_for_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+", "A", ("*", "B", "C")));
        assert_eq!(space.query(&expr!("+", a, ("*", a, c))), vec![]);
    }

    #[test]
    fn test_match_query_variable_has_priority() {
        let mut space = GroundingSpace::new();
        space.add(expr!("equals", x, x));
        assert_eq!(space.query(&expr!("equals", y, z)), vec![bind!{y: expr!(x), z: expr!(x)}]);
    }

    #[test]
    fn test_match_query_variable_via_data_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!(x, x));
        assert_eq!(space.query(&expr!(y, (z))), vec![bind!{y: expr!((z))}]);
    }

    #[test]
    fn test_match_if_then_with_x() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=", ("if", "True", then), then));
        assert_eq!(space.query(&expr!("=", ("if", "True", "42"), X)),
        vec![bind!{X: expr!("42")}]);
    }

    #[test]
    fn test_match_combined_query() {
        let mut space = GroundingSpace::new();
        space.add(expr!("posesses", "Sam", "baloon"));
        space.add(expr!("likes", "Sam", ("blue", "stuff")));
        space.add(expr!("has-color", "baloon", "blue"));

        let result = space.query(&expr!(",", ("posesses", "Sam", object),
        ("likes", "Sam", (color, "stuff")),
        ("has-color", object, color)));
        assert_eq!(result, vec![bind!{object: expr!("baloon"), color: expr!("blue")}]);
    }

    #[test]
    fn test_type_check_in_query() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":", "Human", "Type"));
        space.add(expr!(":", "Socrates", "Human"));
        space.add(expr!("Cons", "Socrates", "Nil"));

        let result = space.query(&expr!(",", (":", h, "Human"), ("Cons", h, t)));
        assert_eq!(result, vec![bind!{h: expr!("Socrates"), t: expr!("Nil")}]);
    }
}
