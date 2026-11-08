use hyperon_atom::*;
use hyperon_atom::serial::NullSerializer;
use hyperon_common::collections::write_mapping;

use bimap::BiMap;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::hash::DefaultHasher;
use std::fmt::{Debug, Display, Formatter};

/// Storage for a hashable atoms.
///
/// Equal atoms can be written differently: `1` and `1.0` are equal numbers.
/// Each spelling gets its own id, so an atom is returned as it was inserted,
/// and [AtomStorage::get_equal_ids] returns ids of all spellings of an atom.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AtomStorage {
    next_id: usize,
    /// First inserted spelling of each set of equal atoms.
    atoms: BiMap<HashableAtom, usize>,
    /// Other spellings by id.
    spellings: HashMap<usize, Atom>,
    /// Ids of other spellings by the id of the first one.
    spelling_ids: HashMap<usize, Vec<usize>>,
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
            // Non-reflexive values (notably IEEE NaN) cannot be hash-map Eq keys.
            Atom::Grounded(g) if g.as_grounded().as_match().is_none()
                    && Self::is_serializable(&**g) && atom == atom => true,
            _ => false,
        }
    }

    fn insert_internal(&mut self, atom: Atom) -> usize {
        match self.atoms.get_by_left(&HashableAtom::Query(&atom)).copied() {
            Some(first) => match self.find_spelling(first, &atom) {
                Some(id) => id,
                None => {
                    let id = self.next_id();
                    self.spellings.insert(id, atom);
                    self.spelling_ids.entry(first).or_default().push(id);
                    id
                },
            },
            None => {
                let id = self.next_id();
                self.atoms.insert(HashableAtom::Store(atom), id);
                id
            },
        }
    }

    /// Ids of all spellings of the atom which has `first` as an id of the
    /// first inserted spelling.
    fn spelling_ids(&self, first: usize) -> impl Iterator<Item=usize> + '_ {
        std::iter::once(first)
            .chain(self.spelling_ids.get(&first).into_iter().flatten().copied())
    }

    fn find_spelling(&self, first: usize, atom: &Atom) -> Option<usize> {
        self.spelling_ids(first)
            .find(|id| is_same_spelling(self.get_atom(*id).unwrap(), atom))
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
            .or_else(|| self.spellings.get(&id))
    }

    fn get_first_id(&self, atom: &Atom) -> Option<usize> {
        if Self::is_hashable(atom) {
            self.atoms.get_by_left(&HashableAtom::Query(atom)).copied()
        } else {
            None
        }
    }

    /// Gets id of the atom in the storage if any. When the storage has no
    /// atom with the same spelling the id of an equal atom is returned.
    pub fn get_id(&self, atom: &Atom) -> Option<usize> {
        self.get_first_id(atom)
            .map(|first| self.find_spelling(first, atom).unwrap_or(first))
    }

    /// Gets ids of all atoms in the storage which are equal to the `atom`.
    pub fn get_equal_ids(&self, atom: &Atom) -> impl Iterator<Item=usize> + '_ {
        self.get_first_id(atom).into_iter()
            .flat_map(|first| self.spelling_ids(first))
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
        self.atoms.len() + self.spellings.len()
    }
}

impl Display for AtomStorage {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut sorted: Vec<_> = self.atoms.iter()
            .map(|(atom, id)| (id, atom.as_atom()))
            .chain(self.spellings.iter().map(|(id, atom)| (id, atom)))
            .collect();
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
                let mut hasher = EqualityHasher(DefaultHasher::new());
                let _ = g.serialize(&mut hasher).expect("Expected to be serializable");
                state.write_u64(hasher.0.finish());
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

/// Hashes serialized values in such a way that equal values have equal
/// hashes. Float which has an integer value is equal to this integer thus
/// it is hashed as an integer.
struct EqualityHasher(DefaultHasher);

impl serial::Serializer for EqualityHasher {
    fn serialize_bool(&mut self, v: bool) -> serial::Result {
        self.0.serialize_bool(v)
    }
    fn serialize_i64(&mut self, v: i64) -> serial::Result {
        self.0.serialize_i64(v)
    }
    fn serialize_f64(&mut self, v: f64) -> serial::Result {
        let integer = v as i64;
        if integer as f64 == v {
            self.0.serialize_i64(integer)
        } else {
            self.0.serialize_f64(v)
        }
    }
    fn serialize_str(&mut self, v: &str) -> serial::Result {
        self.0.serialize_str(v)
    }
}

/// Serialized value which keeps the kind of each serialized part: `0` and
/// `0.0` are different.
#[derive(Default, PartialEq)]
struct Spelling(Vec<u8>);

impl serial::Serializer for Spelling {
    fn serialize_bool(&mut self, v: bool) -> serial::Result {
        Ok(self.0.extend([0, v as u8]))
    }
    fn serialize_i64(&mut self, v: i64) -> serial::Result {
        self.0.push(1);
        Ok(self.0.extend(v.to_le_bytes()))
    }
    fn serialize_f64(&mut self, v: f64) -> serial::Result {
        self.0.push(2);
        Ok(self.0.extend(v.to_le_bytes()))
    }
    fn serialize_str(&mut self, v: &str) -> serial::Result {
        self.0.push(3);
        self.0.extend(v.len().to_le_bytes());
        Ok(self.0.extend(v.bytes()))
    }
}

/// Checks that two equal hashable atoms are written in the same way.
fn is_same_spelling(a: &Atom, b: &Atom) -> bool {
    match (a, b) {
        (Atom::Grounded(a), Atom::Grounded(b)) => {
            let (mut a_spelling, mut b_spelling) = (Spelling::default(), Spelling::default());
            let _ = a.serialize(&mut a_spelling);
            let _ = b.serialize(&mut b_spelling);
            a_spelling == b_spelling
        },
        _ => true,
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
    use hyperon_atom::gnd::number::Number;

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
    fn atom_storage_equal_atoms_keep_their_spelling() {
        let mut storage = AtomStorage::new();
        let float = storage.insert(Atom::gnd(Number::Float(1.0))).unwrap();
        let negative_zero = storage.insert(Atom::gnd(Number::Float(-0.0))).unwrap();
        let zero = storage.insert(Atom::gnd(Number::Float(0.0))).unwrap();
        let int = storage.insert(Atom::gnd(Number::Integer(1))).unwrap();

        assert_eq!(storage.count(), 4);
        assert_eq!(storage.insert(Atom::gnd(Number::Float(1.0))), Ok(float));
        assert_eq!(storage.insert(Atom::gnd(Number::Integer(1))), Ok(int));
        assert_eq!(storage.insert(Atom::gnd(Number::Float(0.0))), Ok(zero));
        assert_eq!(storage.count(), 4);
        assert_eq!(format!("{}", storage), "{ 0: 1.0, 1: -0.0, 2: 0.0, 3: 1 }");

        assert_eq!(storage.get_id(&Atom::gnd(Number::Integer(1))), Some(int));
        assert_eq!(storage.get_id(&Atom::gnd(Number::Float(1.0))), Some(float));
        assert_eq!(storage.get_id(&Atom::gnd(Number::Integer(0))), Some(negative_zero));
        assert_eq!(storage.get_id(&Atom::gnd(Number::Integer(2))), None);
        assert_eq!(storage.get_equal_ids(&Atom::gnd(Number::Integer(1))).collect::<Vec<_>>(), vec![float, int]);
        assert_eq!(storage.get_equal_ids(&Atom::gnd(Number::Float(1.0))).collect::<Vec<_>>(), vec![float, int]);
        assert_eq!(storage.get_equal_ids(&Atom::gnd(Number::Float(0.0))).collect::<Vec<_>>(), vec![negative_zero, zero]);
        assert_eq!(storage.get_equal_ids(&Atom::gnd(Number::Float(0.5))).count(), 0);
    }

    #[test]
    fn equal_numeric_keys_hash_equally_at_representation_boundaries() {
        let mut values: Vec<Atom> = [i64::MIN, -9007199254740993, -1, 0, 1,
            9007199254740992, 9007199254740993, i64::MAX].into_iter()
            .map(|n| Atom::gnd(Number::Integer(n))).collect();
        values.extend([-9223372036854775808.0, -9007199254740992.0, -1.0, -0.0,
            0.0, f64::from_bits(1), 1.0, 1.5, 9007199254740992.0,
            9223372036854775808.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN]
            .into_iter().map(|n| Atom::gnd(Number::Float(n))));
        for a in &values {
            for b in &values {
                if a == b {
                    let mut ha = DefaultHasher::new();
                    let mut hb = DefaultHasher::new();
                    HashableAtom::Store(a.clone()).hash(&mut ha);
                    HashableAtom::Store(b.clone()).hash(&mut hb);
                    assert_eq!(ha.finish(), hb.finish(), "equal keys: {a}, {b}");
                }
            }
        }
    }

    #[test]
    fn nan_uses_the_non_hashable_index_path() {
        let mut storage = AtomStorage::new();
        assert!(matches!(storage.insert(Atom::gnd(Number::Float(f64::NAN))), Err(_)));
        assert_eq!(storage.count(), 0);
    }

    #[derive(Clone, Debug)]
    struct NonSerializable;

    impl PartialEq for NonSerializable {
        fn eq(&self, _other: &Self) -> bool {
            panic!("non-serializable values need no equality test to reject hashing")
        }
    }

    #[test]
    fn non_serializable_values_do_not_need_a_reflexivity_check() {
        let mut storage = AtomStorage::new();
        assert!(storage.insert(Atom::value(NonSerializable)).is_err());
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
