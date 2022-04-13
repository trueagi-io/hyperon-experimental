use std::ffi::*;
use std::os::raw::*;

#[allow(non_camel_case_types)]
pub type c_str_callback_t = extern "C" fn(str: *const c_char, context: *mut c_void) -> ();

pub unsafe fn cstr_as_str<'a>(s: *const c_char) -> &'a str {
    CStr::from_ptr(s).to_str().expect("Incorrect UTF-8 sequence")
}

pub unsafe fn cstr_into_string(s: *const c_char) -> String {
    String::from(cstr_as_str(s))
}

pub fn str_as_cstr<'a>(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}
