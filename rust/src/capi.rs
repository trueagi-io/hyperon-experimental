use crate::*;
use std::ffi::*;
use std::os::raw::*;

#[allow(non_camel_case_types)]
pub type atom_t = Atom;

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct gnd_api_t {
    execute: fn(*const gnd_t, *mut vec_atom_t, *mut vec_atom_t) -> *const c_char,
    eq: fn(*const gnd_t, *const gnd_t) -> bool,
    clone: fn(*const gnd_t) -> *mut gnd_t,
    display: fn(*const gnd_t, *mut c_char, usize) -> usize,
    free: fn(*mut gnd_t),
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct gnd_t {
    api: *const gnd_api_t,
}

#[no_mangle]
pub extern "C" fn atom_sym(name: *const c_char) -> *mut atom_t {
    atom_to_ptr(Atom::sym(&cstr_to_string(name)))
}

// TODO: Think about changing the API to make resulting expression taking ownership of passed
// values
#[no_mangle]
pub extern "C" fn atom_expr(children: *const *mut atom_t, size: usize) -> *mut atom_t {
    unsafe {
        let children: Vec<Atom> = std::slice::from_raw_parts(children, size).iter().map(|p| (**p).clone()).collect();
        atom_to_ptr(Atom::Expression(ExpressionAtom{ children }))
    }
}

#[no_mangle]
pub extern "C" fn atom_var(name: *const c_char) -> *mut atom_t {
    atom_to_ptr(Atom::var(&cstr_to_string(name)))
}

#[no_mangle]
pub extern "C" fn atom_gnd(gnd: *mut gnd_t) -> *mut atom_t {
    atom_to_ptr(Atom::gnd(CGroundedAtom(gnd)))
}

#[no_mangle]
pub extern "C" fn free_atom(atom: *mut atom_t) {
    unsafe {
        Box::from_raw(atom);
    }
}

// TODO: change API to pass char buffer and buffer's size in C style
#[no_mangle]
pub extern "C" fn atom_to_str(atom: *const atom_t) -> *const c_char {
    unsafe {
        string_to_cstr(format!("{}", *atom))
    }
}

#[allow(non_camel_case_types)]
pub struct vec_atom_t<'a>(&'a mut Vec<Atom>);

// TODO: think about const and mut for atoms accurately
#[no_mangle]
pub extern "C" fn vec_pop(vec: *mut vec_atom_t) -> *const atom_t {
    unsafe {
        Box::into_raw(Box::new((*vec).0.pop().expect("Vector is empty"))) as *const atom_t
    }
}

////////////////////////////////////////////////////////////////
// Code below is a boilerplate code to implement C API correctly

fn atom_to_ptr(atom: Atom) -> *mut atom_t {
    Box::into_raw(Box::new(atom))
}

// C grounded atom wrapper

struct CGroundedAtom(*mut gnd_t);

impl CGroundedAtom {

    fn as_ptr(&self) -> *mut gnd_t {
        self.0
    }

    fn api(&self) -> &gnd_api_t {
        unsafe {
            &*(*self.as_ptr()).api
        }
    }

    fn execute(&self, ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
        let execute = self.api().execute;
        if execute as usize == 0 {
            Err(format!("{} is not executable", self))
        } else {
            let res = execute(self.as_ptr(), &mut vec_atom_t(ops), &mut vec_atom_t(data));
            if res as usize == 0 {
                Ok(())
            } else {
                Err(cstr_to_string(res).to_owned())
            }
        }
    }

    fn eq(&self, other: &Self) -> bool {
        (self.api().eq)(self.as_ptr(), other.as_ptr())
    }

    fn clone(&self) -> Self {
        CGroundedAtom((self.api().clone)(self.as_ptr()))
    }

    fn display(&self) -> &str {
        let buffer = [0; 4096];
        (self.api().display)(self.as_ptr(), buffer.as_ptr() as *mut c_char, 4096);
        cstr_to_string(buffer.as_ptr() as *mut c_char)
    }

    fn free(&self) {
        (self.api().free)(self.as_ptr());
    }

}

impl GroundedAtom for CGroundedAtom {

    fn execute(&self, ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
        self.execute(ops, data)
    }

    fn eq_gnd(&self, other: &dyn GroundedAtom) -> bool {
        match other.downcast_ref::<CGroundedAtom>() {
            Some(o) => self.eq(o),
            None => false,
        }
    }

    fn clone_gnd(&self) -> Box<dyn GroundedAtom> {
        Box::new(self.clone())
    }

}

impl Display for CGroundedAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display())
    }
}

impl Drop for CGroundedAtom {
    fn drop(&mut self) {
        self.free();
    }
}

// String conversion utilities

const BUFFER_SIZE: usize = 4096;
static mut BUFFER: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];

fn string_to_cstr(s: String) -> *const c_char {
    unsafe {
        let bytes = s.as_bytes();
        let len = std::cmp::min(bytes.len(), BUFFER_SIZE - 4);
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), BUFFER.as_mut_ptr(), len);
        if bytes.len() > BUFFER_SIZE - 4 {
            BUFFER[len] = '.' as u8;
            BUFFER[len + 1] = '.' as u8;
            BUFFER[len + 2] = '.' as u8;
            BUFFER[len + 3] = 0;
        } else {
            BUFFER[len] = 0;
        }
        BUFFER.as_ptr() as *const c_char
    }
}

fn cstr_to_string<'a>(s: *const c_char) -> &'a str {
    unsafe {
        CStr::from_ptr(s).to_str().expect("Incorrect UTF-8 sequence")
    }
}
