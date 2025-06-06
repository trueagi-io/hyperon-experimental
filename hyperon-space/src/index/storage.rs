use hyperon_atom::*;
use hyperon_atom::serial::NullSerializer;
use hyperon_common::collections::write_mapping;

use bimap::BiMap;
use std::hash::{Hash, Hasher};
use std::hash::DefaultHasher;
use std::fmt::{Debug, Display, Formatter};

/// Storage for a hashable atoms.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AtomStorage {
    next_id: usize,
    atoms: BiMap<HashableAtom, usize>,
}

impl AtomStorage {
    /// New storage instance.
    #[cfg(test)]
    pub fn new() -> Self {
        Default::default()
    }

    /// Insert atom into a storage. Returns `Ok(<id>)` if atom is hashable or
    /// `Err(<atom>)` otherwise. Returns `Ok(<previous id>)` if atoms is already
    /// in a storage.
    pub fn insert(&mut self, atom: Atom) -> Result<usize, Atom> {
        if Self::is_hashable(&atom) {
            Ok(self.insert_internal(atom))
        } else {
            Err(atom)
        }
    }

    fn is_hashable(atom: &Atom) -> bool {
        match atom {
            Atom::Symbol(_) => true,
            Atom::Variable(_) => true,
            Atom::Grounded(g) if g.as_grounded().as_match().is_none()
                    && Self::is_serializable(&**g) => true,
            _ => false,
        }
    }

    fn insert_internal(&mut self, atom: Atom) -> usize {
        match self.atoms.get_by_left(&HashableAtom::Query(&atom)) {
            Some(id) => *id,
            None => {
                let id = self.next_id();
                self.atoms.insert(HashableAtom::Store(atom), id);
                id
            },
        }
    }

    #[inline]
    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id = self.next_id + 1;
        id
    }

    /// Gets atom from the storage if any.
    pub fn get_atom(&self, id: usize) -> Option<&Atom> {
        self.atoms.get_by_right(&id).map(|h| h.as_atom())
    }

    /// Gets id of the atom in the storage if any.
    pub fn get_id(&self, atom: &Atom) -> Option<usize> {
        if Self::is_hashable(atom) {
            self.atoms.get_by_left(&HashableAtom::Query(atom)).map(|id| *id)
        } else {
            None
        }
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

    /// Returns number of atoms in the storage.
    pub fn count(&self) -> usize {
        self.atoms.left_values().count()
    }
}

impl Display for AtomStorage {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut sorted: Vec<_> = self.atoms.iter()
            .map(|(atom, id)| (id, atom)).collect();
        sorted.sort_unstable_by_key(|(&id, _)| id);
        write_mapping(f, sorted.into_iter())
    }
}

#[derive(Eq, Debug, Clone)]
enum HashableAtom {
    // Used pointer to eliminate lifetime which leaks through the usages.
    // At the same time HashableAtom::Query is used only for querying before adding
    // where one can guarantee the reference to the added atom is correct.
    Query(*const Atom),
    Store(Atom),
}

impl HashableAtom {
    pub fn as_atom(&self) -> &Atom {
        match self {
            HashableAtom::Query(a) => unsafe{ &**a },
            HashableAtom::Store(a) => a,
        }
    }
}

impl Hash for HashableAtom {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self.as_atom() {
            Atom::Symbol(s) => s.hash(state),
            Atom::Variable(v) => v.hash(state),
            Atom::Grounded(g) => {
                let mut hasher = DefaultHasher::new();
                let _ = g.serialize(&mut hasher).expect("Expected to be serializable");
                state.write_u64(hasher.finish());
            },
            _ => panic!("Not expected"),
        }
    }
}

impl PartialEq for HashableAtom {
    fn eq(&self, other: &Self) -> bool {
        self.as_atom() == other.as_atom()
    }
}

impl Display for HashableAtom {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Store(atom) => Display::fmt(atom, f),
            Self::Query(atom) => write!(f, "{}?", unsafe{ &**atom }),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use hyperon_atom::expr;

    #[test]
    fn atom_storage_insert_symbol() {
        let atom = Atom::sym("Symbol");
        let mut storage = AtomStorage::new();
        let id = storage.insert(atom.clone());
        assert!(id.is_ok());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_variable() {
        let atom = Atom::var("Variable");
        let mut storage = AtomStorage::new();
        let id = storage.insert(atom.clone());
        assert!(id.is_ok());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[derive(PartialEq, Clone, Debug)]
    struct Num(i64);

    impl std::fmt::Display for Num {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            std::fmt::Display::fmt(&self.0, f)
        }
    }

    impl Grounded for Num {
        fn type_(&self) -> Atom {
            Atom::sym("Num")
        }

        fn serialize(&self, serializer: &mut dyn serial::Serializer) -> serial::Result {
            serializer.serialize_i64(self.0)
        }
    }

    #[test]
    fn atom_storage_insert_grounded_value() {
        let atom = Atom::gnd(Num(1234));
        let mut storage = AtomStorage::new();
        let id = storage.insert(atom.clone());
        assert!(id.is_ok());
        assert_eq!(Some(&atom), storage.get_atom(id.unwrap()));
    }

    #[test]
    fn atom_storage_insert_grounded_atom() {
        let mut storage = AtomStorage::new();
        assert_eq!(storage.insert(Atom::value(1)), Err(Atom::value(1)));
    }

    #[test]
    fn atom_storage_insert_expression() {
        let mut storage = AtomStorage::new();
        assert_eq!(storage.insert(expr!("A" b {Num(1)})),
            Err(expr!("A" b {Num(1)})));
    }

    #[test]
    fn atom_storage_display() {
        let mut storage = AtomStorage::new();
        assert!(storage.insert(Atom::sym("S")).is_ok());
        assert!(storage.insert(Atom::var("V")).is_ok());
        assert!(storage.insert(Atom::gnd(Num(42))).is_ok());
        assert_eq!(format!("{}", storage), "{ 0: S, 1: $V, 2: 42 }");
    }
}
