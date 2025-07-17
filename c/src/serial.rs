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
        self.call_serialize(self.api().serialize_str, str_as_cstr(v).as_ptr())
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
