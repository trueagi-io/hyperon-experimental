use std::ffi::*;
use std::os::raw::*;

#[repr(C)]
pub struct callback_t<T> {
    pub func: extern "C" fn(data: T, context: *mut c_void),
    pub context: *mut c_void,
}

impl<T> callback_t<T> {
    pub fn call(&self, arg: T) {
        (self.func)(arg, self.context)
    }
}

pub type c_str_callback_t = callback_t<*const c_char>;

pub unsafe fn cstr_as_str<'a>(s: *const c_char) -> &'a str {
    CStr::from_ptr(s).to_str().expect("Incorrect UTF-8 sequence")
}

pub unsafe fn cstr_into_string(s: *const c_char) -> String {
    String::from(cstr_as_str(s))
}

pub fn str_as_cstr<'a>(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}
