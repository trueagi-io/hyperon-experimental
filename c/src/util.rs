use core::slice;
use std::io::{Cursor, Write};
use std::ffi::CString;
use std::os::raw::*;
use std::ffi::CStr;
use log::{error, warn, info};

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

/// @brief Logs an error through the MeTTa library's logger.  Does not halt execution.
/// @ingroup misc_group
/// @param[in]  msg  A C-style string containing the message to log
///
#[no_mangle]
pub extern "C" fn log_error(msg: *const c_char) {
    let msg = cstr_as_str(msg);
    error!("{msg}")
}

/// @brief Logs a warning through the MeTTa library's logger
/// @ingroup misc_group
/// @param[in]  msg  A C-style string containing the message to log
///
#[no_mangle]
pub extern "C" fn log_warn(msg: *const c_char) {
    let msg = cstr_as_str(msg);
    warn!("{msg}")
}

/// @brief Logs an informative message through the MeTTa library's logger
/// @ingroup misc_group
/// @param[in]  msg  A C-style string containing the message to log
///
#[no_mangle]
pub extern "C" fn log_info(msg: *const c_char) {
    let msg = cstr_as_str(msg);
    info!("{msg}")
}

/// @struct write_t
/// @brief A handle to a Rust std::fmt::Writer to be used from C code
/// @ingroup misc_group
///
#[repr(C)]
pub struct write_t(*const c_void);

/// @brief Write C string into a Rust writer
/// @ingroup misc_group
/// @param[in]  cwrite  A handle to a Rust writer
/// @param[in]  text  C string in UTF-8 format to be written
/// @return 0 if string is written successfully, non-zero otherwise
///
#[no_mangle]
pub extern "C" fn write_str(cwrite: write_t, text: *const c_char) -> isize {
    let cwrite = unsafe{ &mut *(cwrite.0 as *mut CWrite) };
    cwrite.res = cwrite.res.and_then(|()| cwrite.write.write_str(cstr_as_str(text)));
    match cwrite.res {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Rust wrapper of reference of the [std::fmt::Write] implementer which is used
/// to writer to the C code and get the result back. Typical use-case is implementing
/// [std::fmt::Display] or [std::fmt::Debug] for the C object.
/// See [crate::metta::module_loader_t].
pub struct CWrite<'a> {
    /// The reference to the instance of the [std::fmt::Write] trait
    write: &'a mut dyn std::fmt::Write,
    /// The result of the last write operation invoked by C code
    res: std::fmt::Result,
}

impl<'a> CWrite<'a> {
    /// Create new writer from the reference to the [std::fmt::Write] implementation
    pub fn new<T: std::fmt::Write>(write: &'a mut T) -> Self {
        Self{ write, res: Ok(()) }
    }
    /// Get the result of the last operation performed on writer
    pub fn res(&self) -> std::fmt::Result {
        self.res
    }
    /// Call passed operator in a context of the writer passing instance of the
    /// [CWrite] as an argument and returning the result back
    pub fn with<F: 'a + FnOnce(&mut Self)>(&mut self, f: F) -> std::fmt::Result {
        f(self);
        self.res()
    }
}

impl<'a>  From<&mut CWrite<'a>> for write_t {
    fn from(write: &mut CWrite<'a>) -> Self {
        Self((write as *mut CWrite).cast())
    }
}
