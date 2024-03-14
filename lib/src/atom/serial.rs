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
}

/// Serialization error code
pub enum Error {
    /// Serialization of the type is not supported by serializer.
    NotSupported,
}

/// Serialization result type
pub type Result = std::result::Result<(), Error>;
