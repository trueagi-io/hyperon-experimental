use std::ffi::c_void;
use std::ffi::CString;
use std::os::raw::c_char;
use std::ffi::CStr;

#[repr(C)]
pub struct array_t<T> {
    pub items: *const T,
    pub size: usize,
}

impl<T> From<&Vec<T>> for array_t<T> {
    fn from(vec: &Vec<T>) -> Self {
        Self{ items: vec.as_ptr(), size: vec.len() }
    }
}

pub type lambda_t<T> = extern "C" fn(data: T, context: *mut c_void);

pub type c_str_callback_t = lambda_t<*const c_char>;

pub fn cstr_as_str<'a>(s: *const c_char) -> &'a str {
    unsafe{ CStr::from_ptr(s) }.to_str().expect("Incorrect UTF-8 sequence")
}

pub fn cstr_into_string(s: *const c_char) -> String {
    String::from(cstr_as_str(s))
}

pub fn str_as_cstr(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}

pub fn string_as_cstr(s: String) -> CString {
    CString::new(s).expect("CString::new failed")
}
