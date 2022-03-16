use crate::*;
use crate::atom::*;
use crate::atom::matcher::{Bindings, Unifications, WithMatch};
use crate::atom::subexpr::split_expr;

use std::fmt::{Display, Debug};
use std::rc::Rc;
use std::cell::{RefCell, Ref};

// Grounding space

// TODO: Clone is required by C API
#[derive(Clone)]
pub struct GroundingSpace {
    content: Rc<RefCell<Vec<Atom>>>,
}

impl GroundingSpace {

    pub fn new() -> Self {
        Self{ content: Rc::new(RefCell::new(Vec::new())) }
    }
    
    pub fn add(&mut self, atom: Atom) {
        self.content.borrow_mut().push(atom)
    }

    pub fn remove(&mut self, atom: &Atom) -> bool {
        let position = self.borrow_vec().iter().position(|other| other == atom);
        match position {
            Some(position) => {
                self.content.borrow_mut().remove(position);
                true
            },
            None => false, 
        }
    }

    pub fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        let position = self.borrow_vec().iter().position(|other| other == from);
        match position {
            Some(position) => {
                self.content.borrow_mut().as_mut_slice()[position] = to;
                true
            },
            None => false, 
        }
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
                            Bindings::product(acc, res)
                        }
                    })
            },
            _ => self.single_query(pattern),
        }
    }

    fn single_query(&self, pattern: &Atom) -> Vec<Bindings> {
        log::debug!("single_query: pattern: {}", pattern);
        let mut result = Vec::new();
        for next in &(*self.borrow_vec()) {
            for res in next.do_match(pattern) {
                let bindings = matcher::apply_bindings_to_bindings(&res.candidate_bindings, &res.pattern_bindings);
                if let Ok(bindings) = bindings {
                    // FIXME: why compiler cannot see Display is implemented for Bindings
                    log::debug!("single_query: push result: {}, bindings: {:?}", next, bindings);
                    result.push(bindings);
                }
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
        for next in &(*self.borrow_vec()) {
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

    pub fn borrow_vec(&self) -> Ref<Vec<Atom>> {
        self.content.borrow()
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
    fn add_atom() {
        let mut space = GroundingSpace::new();
        space.add(expr!("a"));
        space.add(expr!("b"));
        space.add(expr!("c"));
        assert_eq!(*space.borrow_vec(), vec![expr!("a"), expr!("b"), expr!("c")]);
    }

    #[test]
    fn remove_atom() {
        let mut space = GroundingSpace::new();
        space.add(expr!("a"));
        space.add(expr!("b"));
        space.add(expr!("c"));
        assert_eq!(space.remove(&expr!("b")), true);
        assert_eq!(*space.borrow_vec(), vec![expr!("a"), expr!("c")]);
    }

    #[test]
    fn remove_atom_not_found() {
        let mut space = GroundingSpace::new();
        space.add(expr!("a"));
        assert_eq!(space.remove(&expr!("b")), false);
        assert_eq!(*space.borrow_vec(), vec![expr!("a")]);
    }

    #[test]
    fn replace_atom() {
        let mut space = GroundingSpace::new();
        space.add(expr!("a"));
        space.add(expr!("b"));
        space.add(expr!("c"));
        assert_eq!(space.replace(&expr!("b"), expr!("d")), true);
        assert_eq!(*space.borrow_vec(), vec![expr!("a"), expr!("d"), expr!("c")]);
    }

    #[test]
    fn replace_atom_not_found() {
        let mut space = GroundingSpace::new();
        space.add(expr!("a"));
        assert_eq!(space.replace(&expr!("b"), expr!("d")), false);
        assert_eq!(*space.borrow_vec(), vec![expr!("a")]);
    }

    #[test]
    fn mut_shared_atomspace() {
        let mut first = GroundingSpace::new();
        let mut second = first.clone(); 

        first.add(expr!("b"));
        second.replace(&expr!("b"), expr!("d"));

        assert_eq!(*first.borrow_vec(), vec![expr!("d")]);
        assert_eq!(*second.borrow_vec(), vec![expr!("d")]);
    }

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
