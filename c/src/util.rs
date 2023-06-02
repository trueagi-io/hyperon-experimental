use core::slice;
use std::io::{Cursor, Write};
use std::ffi::c_void;
use std::ffi::CString;
use std::os::raw::c_char;
use std::ffi::CStr;
use hyperon::common::shared::Shared;
use std::ops::{Deref, DerefMut};

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

pub(crate) fn write_into_buf<T: std::fmt::Display>(obj: T, buf: *mut c_char, buf_len: usize) -> usize {
    let slice = unsafe{ slice::from_raw_parts_mut(buf as *mut u8, buf_len) };
    let mut cursor = Cursor::new(slice);
    let len = if let Err(_err) = write!(cursor, "{obj}") {
        0 //The buffer was probably too short
    } else {
        let len = cursor.position() as usize;

        //If we wrote right up to the end of the buffer, then fail, because we need room for the terminator
        if len < buf_len-1 {
            len
        } else {
            0
        }
    };

    //Write the terminator
    let slice = cursor.into_inner();
    slice[len] = 0;
    len
}

pub(crate) fn write_debug_into_buf<T: std::fmt::Debug>(obj: T, buf: *mut c_char, buf_len: usize) -> usize {
    struct DisplayDebug<DebugT>(DebugT);
    impl<DebugT: std::fmt::Debug> std::fmt::Display for DisplayDebug<DebugT> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.0)
        }
    }
    write_into_buf(DisplayDebug(obj), buf, buf_len)
}

// We cannot use imported Shared in C API because it is not correctly
// converted int C header and header cannot be compiled. This wrapper just
// solves the issue by shadowing original type.
pub struct SharedApi<T>(pub(crate) Shared<T>);

impl<T> SharedApi<T> {
    pub fn new(value: T) -> *mut Self {
        Box::into_raw(Box::new(Self(Shared::new(value))))
    }

    pub fn from_shared(shared: Shared<T>) -> *mut Self {
        Box::into_raw(Box::new(Self(shared)))
    }

    pub fn drop(ptr: *mut Self) {
        unsafe { drop(Box::from_raw(ptr)) }
    }

    pub fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        self.0.borrow()
    }

    pub fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_> {
        self.0.borrow_mut()
    }

    pub fn shared(&self) -> Shared<T> {
        self.0.clone()
    }
}

impl<T> PartialEq for SharedApi<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
