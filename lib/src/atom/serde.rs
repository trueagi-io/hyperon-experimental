// Here, in C and in Python interfaces native types are used as primitives
// for a serialization. There are two options: (1) using MeTTa types (Bool, Number)
// or (2) using native types (bool/i64/f64 in Rust, bool/longlong/double in C,
// bool/int/float in Python). Using MeTTa types forces API user to convert native
// data into this type and back (during deserialization). On the first glance
// it should allow saving value in Python and loading it in Rust using the same
// serializer implementation on both ends. But it is expected that exchanging
// data between Python and Rust anyway requires additional code on the Rust side.
// Thus this code can include Rust specific serialization. On the other hand
// using MeTTa types makes API user doing additional conversion from native type
// to the MeTTa type even if same runtime is used for saving and loading. Thus
// using native types allows eliminate the double conversion.
pub trait Serializer {
    fn serialize_bool(&mut self, _v: bool) -> Result { Err(Error::NotSupported) }
    fn serialize_i64(&mut self, _v: i64) -> Result { Err(Error::NotSupported) }
    fn serialize_f64(&mut self, _v: f64) -> Result { Err(Error::NotSupported) }
}

pub enum Error {
    NotSupported,
}

pub type Result = std::result::Result<(), Error>;

#[derive(Default)]
pub(crate) struct BoolSerializer {
    value: Option<bool>,
}

impl Serializer for BoolSerializer {
    fn serialize_bool(&mut self, v: bool) -> Result {
        self.value = Some(v);
        Ok(())
    }
}

use std::convert::TryInto;

pub struct AsPrimitive<'a> {
    atom: &'a super::Atom
}

impl<'a> AsPrimitive<'a> {
    pub fn from_atom(atom: &'a super::Atom) -> Self {
        Self{ atom }
    }

    pub fn as_bool(self) -> Option<bool> {
        TryInto::<&dyn super::GroundedAtom>::try_into(self.atom).ok()
            .map(|gnd| {
                let mut serializer = BoolSerializer::default();
                let _ = gnd.serialize(&mut serializer);
                serializer.value
            }).flatten()
    }
}
