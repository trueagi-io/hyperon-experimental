use core::slice;
use std::io::{Cursor, Write};
use std::ffi::CString;
use std::os::raw::c_char;
use std::ffi::CStr;

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

    //If buf_len == 0, the caller is just interested in the size of buffer they will need
    if buf_len == 0 {
        struct LengthTracker(usize);
        impl Write for LengthTracker {
            fn write(&mut self, slice: &[u8]) -> Result<usize, std::io::Error> {
                self.0 += slice.len();
                Ok(slice.len())
            }
            fn flush(&mut self) -> Result<(), std::io::Error> { Ok (())}
        }

        let mut length_tracker = LengthTracker(0);
        write!(length_tracker, "{obj}").unwrap();
        length_tracker.0
    } else {
        //We are goint to try and actually render the object into the buffer, saving room for the terminator
        let slice = unsafe{ slice::from_raw_parts_mut(buf as *mut u8, buf_len) };
        let mut cursor = Cursor::new(slice);
        let len = if let Err(_err) = write!(cursor, "{obj}") {
            //The buffer was probably too short, so figure out the buffer size we need
            cursor.into_inner()[0] = 0;
            return write_into_buf(obj, buf, 0);
        } else {
            let len = cursor.position() as usize;
            //We still need room for the terminator
            if len == buf_len {
                cursor.into_inner()[0] = 0;
                return write_into_buf(obj, buf, 0)
            } else {
                len
            }
        };

        //Write the terminator
        cursor.into_inner()[len] = 0;
        len
    }
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
