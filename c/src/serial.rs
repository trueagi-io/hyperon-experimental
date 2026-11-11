use hyperon_atom::serial;

use std::os::raw::*;
use crate::util::*;

type serial_serialize_func_t<T> = extern "C" fn(context: *mut c_void, value: T) -> serial_result_t;

/// @struct serializer_api_t
/// @brief A table of functions to receive values encoded as specific primitive types
/// @ingroup serializer_group
///
// TODO: Serializer needs comprehensive documentation, but perhaps we should wait until architecture
// is fully fleshed out to avoid needing to redo work
#[repr(C)]
pub struct serializer_api_t {
    /// @brief Serialize C `bool` value
    /// @param[in]  context A caller-defined object to pass to functions in the `api`, to receive the encoded value(s)
    /// @param[in]  v A value to serialize
    /// @return  A `serial_result_t` indicating whether the `serialize` operation was successful
    ///
    serialize_bool: Option<extern "C" fn(context: *mut c_void, v: bool) -> serial_result_t>,
    /// @brief Serialize C `long long` value
    /// @param[in]  context A caller-defined object to pass to functions in the `api`, to receive the encoded value(s)
    /// @param[in]  v A value to serialize
    /// @return  A `serial_result_t` indicating whether the `serialize` operation was successful
    ///
    serialize_longlong: Option<extern "C" fn(context: *mut c_void, v: c_longlong) -> serial_result_t>,
    /// @brief Serialize C `double` value
    /// @param[in]  context A caller-defined object to pass to functions in the `api`, to receive the encoded value(s)
    /// @param[in]  v A value to serialize
    /// @return  A `serial_result_t` indicating whether the `serialize` operation was successful
    ///
    serialize_double: Option<extern "C" fn(context: *mut c_void, v: c_double) -> serial_result_t>,
    /// @brief Serialize C `char[]` value
    /// @param[in]  context A caller-defined object to pass to functions in the `api`, to receive the encoded value(s)
    /// @param[in]  v A value to serialize
    /// @return  A `serial_result_t` indicating whether the `serialize` operation was successful
    ///
    serialize_str: Option<extern "C" fn(context: *mut c_void, v: *const c_char) -> serial_result_t>,
}

/// @struct c_to_rust_serializer_t
/// @brief Adapt a serializer implemented in Rust to C API
/// @ingroup serializer_group
/// @see gnd_api_t
///
#[repr(C)]
pub(crate) struct c_to_rust_serializer_t<'a>(&'a mut dyn serial::Serializer);

impl c_to_rust_serializer_t<'_> {
    fn borrow_mut(&mut self) -> &mut dyn serial::Serializer {
        self.0
    }
}

impl<'a> From<&'a mut dyn serial::Serializer> for c_to_rust_serializer_t<'a> {
    fn from(src: &'a mut dyn serial::Serializer) -> Self {
        Self(src)
    }
}

/// @struct serial_result_t
/// @brief The result of a `serialize` operation reported by serializer
/// @ingroup serializer_group
/// @see gnd_api_t
///
#[repr(C)]
pub enum serial_result_t {
    /// @brief Successful serialization
    ///
    OK,
    /// @brief Serialization of the value is not supported by serializer
    ///
    NOT_SUPPORTED,
}

impl From<serial::Result> for serial_result_t {
    fn from(result: serial::Result) -> Self {
        match result {
            Ok(()) => serial_result_t::OK,
            Err(serial::Error::NotSupported) => serial_result_t::NOT_SUPPORTED,
        }
    }
}

impl From<serial_result_t> for serial::Result {
    fn from(result: serial_result_t) -> Self {
        match result {
            serial_result_t::OK => Ok(()),
            serial_result_t::NOT_SUPPORTED => Err(serial::Error::NotSupported),
        }
    }
}

pub(crate) const C_TO_RUST_SERIALIZER_API: serializer_api_t = serializer_api_t {
    serialize_bool: Some(serialize_bool_rust_adapter),
    serialize_longlong: Some(serialize_longlong_rust_adapter),
    serialize_double: Some(serialize_double_rust_adapter),
    serialize_str: Some(serialize_str_rust_adapter),
};

#[no_mangle]
extern "C" fn serialize_bool_rust_adapter(context: *mut c_void, v: bool) -> serial_result_t {
    let target = unsafe{ &mut*(context as *mut c_to_rust_serializer_t) }.borrow_mut();
    target.serialize_bool(v).into()
}

#[no_mangle]
extern "C" fn serialize_longlong_rust_adapter(context: *mut c_void, v: c_longlong) -> serial_result_t {
    let target = unsafe{ &mut*(context as *mut c_to_rust_serializer_t)}.borrow_mut();
    target.serialize_i64(v).into()
}

#[no_mangle]
extern "C" fn serialize_double_rust_adapter(context: *mut c_void, v: c_double) -> serial_result_t {
    let target = unsafe{ &mut*(context as *mut c_to_rust_serializer_t)}.borrow_mut();
    target.serialize_f64(v).into()
}

#[no_mangle]
extern "C" fn serialize_str_rust_adapter(context: *mut c_void, v: *const c_char) -> serial_result_t {
    let target = unsafe{ &mut*(context as *mut c_to_rust_serializer_t)}.borrow_mut();
    target.serialize_str(cstr_as_str(v)).into()
}

/// @struct RustToCSerializer
/// @brief Adapt a serializer implemented in C to Rust API
/// @ingroup serializer_group
/// @see gnd_api_t
///
pub(crate) struct RustToCSerializer {
    api: *const serializer_api_t,
    context: *mut c_void,
}

impl serial::Serializer for RustToCSerializer {
    fn serialize_bool(&mut self, v: bool) -> serial::Result {
        self.call_serialize(self.api().serialize_bool, v)
    }
    fn serialize_i64(&mut self, v: i64) -> serial::Result {
        self.call_serialize(self.api().serialize_longlong, v)
    }
    fn serialize_f64(&mut self, v: f64) -> serial::Result {
        self.call_serialize(self.api().serialize_double, v)
    }
    fn serialize_str(&mut self, v: &str) -> serial::Result {
        // This callback's protocol is NUL-terminated, not a byte-span protocol.
        let text = std::ffi::CString::new(v).map_err(|_| serial::Error::NotSupported)?;
        self.call_serialize(self.api().serialize_str, text.as_ptr())
    }
}

impl RustToCSerializer {
    pub fn new(api: *const serializer_api_t, context: *mut c_void) -> Self {
        Self{ api, context }
    }
    fn api(&self) -> &serializer_api_t {
        unsafe{ &*self.api }
    }
    fn call_serialize<T>(&self, serialize: Option<serial_serialize_func_t<T>>, v: T) -> serial::Result {
        serialize.map_or(Err(serial::Error::NotSupported), |serialize| {
            serialize(self.context, v).into()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hyperon_atom::{Atom, gnd::str::Str, serial::Serializer};
    use std::ffi::CStr;

    #[derive(Default)]
    struct Received {
        strings: Vec<Vec<u8>>,
        stable: bool,
        reject: bool,
        primitives: Vec<String>,
    }

    extern "C" fn receive_str(context: *mut c_void, value: *const c_char) -> serial_result_t {
        let received = unsafe { &mut *(context as *mut Received) };
        let value = unsafe { CStr::from_ptr(value) };
        let copy = value.to_bytes().to_vec();
        // Read again after allocating/copying, while the synchronous callback is active.
        received.stable = value.to_bytes() == copy.as_slice();
        received.strings.push(copy);
        if received.reject { serial_result_t::NOT_SUPPORTED } else { serial_result_t::OK }
    }

    extern "C" fn receive_bool(context: *mut c_void, value: bool) -> serial_result_t {
        unsafe { &mut *(context as *mut Received) }.primitives.push(value.to_string());
        serial_result_t::OK
    }

    extern "C" fn receive_i64(context: *mut c_void, value: c_longlong) -> serial_result_t {
        unsafe { &mut *(context as *mut Received) }.primitives.push(value.to_string());
        serial_result_t::NOT_SUPPORTED
    }

    extern "C" fn receive_f64(context: *mut c_void, value: c_double) -> serial_result_t {
        unsafe { &mut *(context as *mut Received) }.primitives.push(value.to_string());
        serial_result_t::OK
    }

    fn api() -> serializer_api_t {
        serializer_api_t {
            serialize_bool: Some(receive_bool),
            serialize_longlong: Some(receive_i64),
            serialize_double: Some(receive_f64),
            serialize_str: Some(receive_str),
        }
    }

    fn context(received: &mut Received) -> *mut c_void {
        received as *mut Received as *mut c_void
    }

    #[test]
    fn strings_preserve_bytes_and_callback_result() {
        let api = api();
        let mut received = Received::default();
        let mut serializer = RustToCSerializer::new(&api, context(&mut received));
        for value in ["", "ASCII", "žluťoučký 🦀", &"long λ".repeat(16384)] {
            let before = received.strings.len();
            assert_eq!(serializer.serialize_str(value), Ok(()));
            assert_eq!(received.strings.len(), before + 1);
            assert_eq!(received.strings.last().unwrap(), value.as_bytes());
            assert!(received.stable);
        }
        received.reject = true;
        assert_eq!(serializer.serialize_str("unsupported"), Err(serial::Error::NotSupported));
        assert_eq!(received.strings.len(), 5);
        received.reject = false;
        assert_eq!(serializer.serialize_str("after callback rejection"), Ok(()));
        assert_eq!(received.strings.len(), 6);
    }

    #[test]
    fn nul_is_rejected_before_callback_and_serializer_is_reusable() {
        let api = api();
        let mut received = Received::default();
        let mut serializer = RustToCSerializer::new(&api, context(&mut received));
        let mut failures = Vec::new();
        for value in ["\0start", "mid\0dle", "end\0", "\0\0"] {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                serializer.serialize_str(value)
            }));
            if !matches!(result, Ok(Err(serial::Error::NotSupported))) {
                failures.push(value);
            }
            assert!(received.strings.is_empty());
        }
        assert!(failures.is_empty(), "did not return NotSupported: {failures:?}");
        assert_eq!(serializer.serialize_str("after NUL rejection"), Ok(()));
        assert_eq!(received.strings, vec![b"after NUL rejection".to_vec()]);
    }

    #[test]
    fn missing_callbacks_return_not_supported() {
        let api = serializer_api_t {
            serialize_bool: None, serialize_longlong: None,
            serialize_double: None, serialize_str: None,
        };
        let mut serializer = RustToCSerializer::new(&api, std::ptr::null_mut());
        for value in ["", "ordinary", "\0", "mid\0dle"] {
            assert_eq!(serializer.serialize_str(value), Err(serial::Error::NotSupported));
        }
        assert_eq!(serializer.serialize_bool(true), Err(serial::Error::NotSupported));
        assert_eq!(serializer.serialize_i64(42), Err(serial::Error::NotSupported));
        assert_eq!(serializer.serialize_f64(1.5), Err(serial::Error::NotSupported));
    }

    #[test]
    fn primitive_callbacks_are_unchanged() {
        let api = api();
        let mut received = Received::default();
        let mut serializer = RustToCSerializer::new(&api, context(&mut received));
        assert_eq!(serializer.serialize_bool(true), Ok(()));
        assert_eq!(serializer.serialize_i64(i64::MIN), Err(serial::Error::NotSupported));
        assert_eq!(serializer.serialize_f64(1.5), Ok(()));
        assert_eq!(received.primitives, ["true", "-9223372036854775808", "1.5"]);
        assert!(received.strings.is_empty());
    }

    fn exported_rejection(value: &str) {
        let api = api();
        let mut received = Received::default();
        let atom = Atom::gnd(Str::from_string(value.into()));
        let atom_ref = crate::atom::atom_ref_t::from(&atom);
        let result = crate::atom::atom_gnd_serialize(&atom_ref, &api, context(&mut received));
        assert!(matches!(result, serial_result_t::NOT_SUPPORTED));
        assert!(received.strings.is_empty());

        let ordinary = Atom::gnd(Str::from_str("after exported rejection"));
        let atom_ref = crate::atom::atom_ref_t::from(&ordinary);
        let result = crate::atom::atom_gnd_serialize(&atom_ref, &api, context(&mut received));
        assert!(matches!(result, serial_result_t::OK));
        assert_eq!(received.strings, vec![b"after exported rejection".to_vec()]);
        assert!(received.stable);
    }

    #[test]
    fn exported_nul_beginning() { exported_rejection("\0start"); }
    #[test]
    fn exported_nul_middle() { exported_rejection("mid\0dle"); }
    #[test]
    fn exported_nul_end() { exported_rejection("end\0"); }
    #[test]
    fn exported_nul_repeated() { exported_rejection("\0\0"); }
}
