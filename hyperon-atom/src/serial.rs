use std::hash::{DefaultHasher, Hasher};

/// Serial module defines an API to implement serialization/deserialization of the
/// grounded atoms. The serialization API can be used for saving grounded atoms to
/// disk, sending them over network or implement value conversion between
/// different runtimes (Rust and Python for instance).
///
/// One should keep in mind that different runtimes has different set of the
/// native types for serialization. At the same time the serialization protocol
/// which is implemented by serializer/deserializer defines how these basic
/// types are converted into a binary blob. In particular core
/// library implements a conversion protocol which defines how Python values
/// are converted into the Rust ones and vice versa. Using native types
/// instead of using an universal set of types (for example MeTTa stdlib types)
/// eliminates additional conversion from a native type to a MeTTa one.

/// Trait to implement Rust grounded value serializer. It is not necessary to
/// implement all methods. By default methods return [Error::NotSupported].
/// This means that this implementation doesn't support serializing values of
/// such type.
pub trait Serializer {
    /// Serialize bool value.
    fn serialize_bool(&mut self, _v: bool) -> Result { Err(Error::NotSupported) }
    /// Serialize i64 value.
    fn serialize_i64(&mut self, _v: i64) -> Result { Err(Error::NotSupported) }
    /// Serialize f64 value.
    fn serialize_f64(&mut self, _v: f64) -> Result { Err(Error::NotSupported) }
    /// Serialize string value.
    fn serialize_str(&mut self, _v: &str) -> Result { Err(Error::NotSupported) }
}

/// Serialization error code
#[derive(PartialEq, Debug)]
pub enum Error {
    /// Serialization of the type is not supported by serializer.
    NotSupported,
}

/// Converts serializable grounded atom into native Rust type `T`.
/// Returning error from this serializer is not expected as usually it should
/// just write data into a memory buffer for conversion. [Default] is required
/// to instantiate it in [ConvertingSerializer::convert] method.
pub trait ConvertingSerializer<T>: Serializer + Default {
    fn into_type(self) -> Option<T>;

    /// Converts atom into Rust value using `Self::default()` instance.
    /// First it checks whether the grounded atom is already an instance of `T`
    /// and clones value if it the case. Otherwise it uses `Self` to convert
    /// into `T` via serialization.
    fn convert(atom: &super::Atom) -> Option<T>
        where T: 'static + Clone
    {
        std::convert::TryInto::<&dyn super::GroundedAtom>::try_into(atom)
            .ok()
            .and_then(|gnd| {
                gnd.as_any_ref()
                    // TODO: it is not clear whether this step really improves performance.
                    // On the other hand it requires `T` to be static and implement `Clone`.
                    // It is better to do a performance test and check if first step should be
                    // removed.
                    .downcast_ref::<T>()
                    .cloned()
                    .or_else(|| {
                        let mut serializer = Self::default();
                        gnd.serialize(&mut serializer).ok()
                            .and_then(|()| serializer.into_type())
                    })
            })
    }
}

/// Serialization result type
pub type Result = std::result::Result<(), Error>;

trait PrivHasher : Hasher {}
impl PrivHasher for DefaultHasher {}

// there are much speedier hashers, but not sure if it's worth the extra dependency given the other options
impl<H: PrivHasher> Serializer for H {
    fn serialize_bool(&mut self, v: bool) -> Result { Ok(self.write_u8(v as u8)) }
    fn serialize_i64(&mut self, v: i64) -> Result { Ok(self.write_i64(v)) }
    fn serialize_f64(&mut self, v: f64) -> Result { Ok(self.write_i64(unsafe{ std::mem::transmute::<f64, i64>(v) })) }
    fn serialize_str(&mut self, v: &str) -> Result { Ok(v.bytes().for_each(|b| self.write_u8(b))) }
}

// for debugging
impl Serializer for String {
    fn serialize_bool(&mut self, v: bool) -> Result { Ok(self.push_str(&*v.to_string())) }
    fn serialize_i64(&mut self, v: i64) -> Result { Ok(self.push_str(&*v.to_string())) }
    fn serialize_f64(&mut self, v: f64) -> Result { Ok(self.push_str(&*v.to_string())) }
    fn serialize_str(&mut self, v: &str) -> Result { Ok(self.push_str(v)) }
}

// for speed, but is technically unsafe at usage site because not a valid utf-8 string
impl Serializer for Vec<u8> {
    fn serialize_bool(&mut self, v: bool) -> Result { Ok(self.push(v as u8)) }
    fn serialize_i64(&mut self, v: i64) -> Result { Ok(self.extend(v.to_le_bytes())) }
    fn serialize_f64(&mut self, v: f64) -> Result { Ok(self.extend(v.to_le_bytes())) }
    fn serialize_str(&mut self, v: &str) -> Result { Ok(self.extend(v.bytes())) }
}

#[derive(Default)]
pub struct NullSerializer();

impl Serializer for NullSerializer {
    fn serialize_bool(&mut self, _v: bool) -> Result { Ok(()) }
    fn serialize_i64(&mut self, _v: i64) -> Result { Ok(()) }
    fn serialize_f64(&mut self, _v: f64) -> Result { Ok(()) }
    fn serialize_str(&mut self, _v: &str) -> Result { Ok(()) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;
    use std::hash::DefaultHasher;

    use std::fmt::Display;

    #[derive(Default)]
    struct I64Serializer {
        value: Option<i64>,
    }

    impl Serializer for I64Serializer {
        fn serialize_i64(&mut self, v: i64) -> super::Result {
            self.value = Some(v);
            Ok(())
        }
    }

    impl ConvertingSerializer<i64> for I64Serializer {
        fn into_type(self) -> Option<i64> {
            self.value
        }
    }

    #[derive(PartialEq, Debug, Clone)]
    struct I64Gnd(i64);

    impl Display for I64Gnd {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl Grounded for I64Gnd {
        fn type_(&self) -> Atom {
            rust_type_atom::<Self>()
        }

        fn serialize(&self, serializer: &mut dyn serial::Serializer) -> serial::Result {
            serializer.serialize_i64(self.0)
        }
    }

    #[test]
    fn convert_return_none_on_incorrect_grounded_atom() {
        assert_eq!(I64Serializer::convert(&Atom::gnd(I64Gnd(42))), Some(42));
        assert_eq!(I64Serializer::convert(&Atom::value("42")), None);
    }

    #[derive(PartialEq, Debug, Clone)]
    struct F64Gnd(f64);

    impl Display for F64Gnd {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl Grounded for F64Gnd {
        fn type_(&self) -> Atom {
            rust_type_atom::<Self>()
        }

        fn serialize(&self, serializer: &mut dyn serial::Serializer) -> serial::Result {
            serializer.serialize_f64(self.0)
        }
    }


    #[test]
    fn default_hasher_serialization_f64_i64() {
        let mut hasher = DefaultHasher::new();
        assert_eq!(I64Gnd(1i64).serialize(&mut hasher), Ok(()));
        let i64hash = hasher.finish();
        let mut hasher = DefaultHasher::new();
        assert_eq!(F64Gnd(1.0f64).serialize(&mut hasher), Ok(()));
        let f64hash = hasher.finish();
        assert_ne!(i64hash, f64hash);
    }
}
