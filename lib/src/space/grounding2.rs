use crate::atom::*;
use crate::serial::NullSerializer;

use bimap::BiMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::hash::DefaultHasher;

#[derive(Default)]
struct AtomStorage {
    next_id: usize,
    atoms: BiMap<HashAtom, usize>,
}

impl AtomStorage {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, atom: &Atom) -> Option<usize> {
        match atom {
            Atom::Symbol(_) => self.insert_internal(atom),
            Atom::Variable(_) => self.insert_internal(atom),
            Atom::Grounded(g) if g.as_grounded().as_match().is_none()
                    && Self::is_serializable(&**g)
                    => self.insert_internal(atom),
            _ => None,
        }
    }

    fn insert_internal(&mut self, atom: &Atom) -> Option<usize> {
        let id = match self.atoms.get_by_left(&HashAtom::Query(atom)) {
            Some(id) => *id,
            None => {
                let id = self.next_id();
                self.atoms.insert(HashAtom::Store(atom.clone()), id);
                id
            },
        };
        Some(id)
    }

    #[inline]
    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id = self.next_id + 1;
        id
    }

    pub fn get(&self, id: usize) -> Option<&Atom> {
        self.atoms.get_by_right(&id).map(|h| h.as_atom())
    }

    // TODO: calling Grounded::serialize() is a workaround. We don't know
    // in advance whether atom is serializable and we understand it only
    // after calling Grounded::serialize.
    fn is_serializable(gnd: &dyn GroundedAtom) -> bool {
        match gnd.serialize(&mut NullSerializer::default()) {
            Ok(_) => true,
            Err(_) => false,
        }
    }
}

#[derive(Eq)]
enum HashAtom {
    // Used pointer to eliminate lifetime which leaks through the usages.
    // At the same time HashAtom::Query is used only for querying before adding
    // where one can guarantee the reference to the added atom is correct.
    Query(*const Atom),
    Store(Atom),
}

impl HashAtom {
    pub fn as_atom(&self) -> &Atom {
        match self {
            HashAtom::Query(a) => unsafe{ &**a },
            HashAtom::Store(a) => a,
        }
    }
}

impl Hash for HashAtom {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self.as_atom() {
            Atom::Symbol(s) => s.hash(state),
            Atom::Variable(v) => v.hash(state),
            Atom::Grounded(g) => {
                let mut hasher = DefaultHasher::new();
                let _ = g.serialize(&mut hasher).expect("Expected to be hashable");
                state.write_u64(hasher.finish());
            },
            _ => panic!("Not expected"),
        }
    }
}

impl PartialEq for HashAtom {
    fn eq(&self, other: &Self) -> bool {
        self.as_atom() == other.as_atom()
    }
}

#[derive(PartialEq, Debug)]
enum AtomToken<'a> {
    Atom(&'a Atom),
    StartExpr,
    EndExpr,
}

#[derive(Default)]
enum AtomTokenIterState<'a> {
    Single(&'a Atom),
    Expression(&'a ExpressionAtom),
    Iterate(&'a ExpressionAtom, usize, Box<AtomTokenIterState<'a>>),
    #[default]
    End,
}

struct AtomTokenIter<'a> {
    atom: &'a Atom,
    state: AtomTokenIterState<'a>,
}

impl<'a> AtomTokenIter<'a> {
    fn new(atom: &'a Atom) -> Self {
        let state = match atom {
            Atom::Symbol(_) | Atom::Variable(_) | Atom::Grounded(_) =>
                AtomTokenIterState::Single(atom),
            Atom::Expression(expr) => 
                AtomTokenIterState::Expression(expr),
        };
        Self{ atom, state }
    }
}

impl<'a> Iterator for AtomTokenIter<'a> {
    type Item = AtomToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::take(&mut self.state) {
            AtomTokenIterState::Single(atom) => {
                self.state = AtomTokenIterState::End;
                Some(AtomToken::Atom(atom))
            },
            AtomTokenIterState::Expression(expr) => {
                self.state = AtomTokenIterState::Iterate(expr, 0, Box::new(AtomTokenIterState::End));
                Some(AtomToken::StartExpr)
            },
            AtomTokenIterState::Iterate(expr, i, prev) => {
                let children = expr.children();
                if i < children.len() {
                    let atom = unsafe{ children.get_unchecked(i) };
                    let next_state = AtomTokenIterState::Iterate(expr, i + 1, prev);
                    match atom {
                        Atom::Symbol(_) | Atom::Variable(_) | Atom::Grounded(_) => {
                            self.state = next_state;
                            Some(AtomToken::Atom(atom))
                        },
                        Atom::Expression(e) => {
                            self.state = AtomTokenIterState::Iterate(e, 0, Box::new(next_state));
                            Some(AtomToken::StartExpr)
                        },
                    }
                } else {
                    self.state = *prev;
                    Some(AtomToken::EndExpr)
                }
            },
            AtomTokenIterState::End => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr;
    use crate::metta::runner::number::Number;

    #[test]
    fn atom_storage_insert_symbol() {
        let atom = Atom::sym("Symbol");
        let mut storage = AtomStorage::new();
        let id = storage.insert(&atom);
        assert!(id.is_some());
        assert_eq!(Some(&atom), storage.get(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_variable() {
        let atom = Atom::var("Variable");
        let mut storage = AtomStorage::new();
        let id = storage.insert(&atom);
        assert!(id.is_some());
        assert_eq!(Some(&atom), storage.get(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_grounded_value() {
        let atom = Atom::gnd(Number::Integer(1234));
        let mut storage = AtomStorage::new();
        let id = storage.insert(&atom);
        assert!(id.is_some());
        assert_eq!(Some(&atom), storage.get(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_grounded_atom() {
        let mut storage = AtomStorage::new();
        assert!(storage.insert(&Atom::value(1)).is_none());
    }

    #[test]
    fn atom_storage_insert_expression() {
        let mut storage = AtomStorage::new();
        assert!(storage.insert(&expr!("A" b {Number::Integer(1)})).is_none());
    }

    #[test]
    fn atom_token_iter_symbol() {
        let atom = Atom::sym("sym");
        let it = AtomTokenIter::new(&atom);
        let actual: Vec<AtomToken> = it.collect();
        assert_eq!(vec![AtomToken::Atom(&atom)], actual);
    }

    #[test]
    fn atom_token_iter_expr() {
        let atom = expr!(("sym" var {Number::Integer(42)} ("sym1" var1 {Number::Integer(43)})));
        let it = AtomTokenIter::new(&atom);
        let actual: Vec<AtomToken> = it.collect();
        assert_eq!(vec![AtomToken::StartExpr, AtomToken::Atom(&expr!("sym")),
            AtomToken::Atom(&expr!(var)), AtomToken::Atom(&expr!({Number::Integer(42)})),
            AtomToken::StartExpr, AtomToken::Atom(&expr!("sym1")),
            AtomToken::Atom(&expr!(var1)), AtomToken::Atom(&expr!({Number::Integer(43)})),
            AtomToken::EndExpr, AtomToken::EndExpr], actual);
    }
}
