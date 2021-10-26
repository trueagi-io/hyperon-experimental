use hyperon::*;
use std::ffi::*;
use std::os::raw::*;
use std::convert::TryInto;
use std::fmt::Display;

#[allow(non_camel_case_types)]
pub struct atom_t {
    atom: Atom,
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct gnd_api_t {
    // One can assign NULL to this field, it means the atom is not executable
    execute: Option<extern "C" fn(*const gnd_t, *mut vec_atom_t, *mut vec_atom_t) -> *const c_char>,
    eq: extern "C" fn(*const gnd_t, *const gnd_t) -> bool,
    clone: extern "C" fn(*const gnd_t) -> *mut gnd_t,
    display: extern "C" fn(*const gnd_t, *mut c_char, usize) -> usize,
    free: extern "C" fn(*mut gnd_t),
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct gnd_t {
    api: *const gnd_api_t,
}

#[no_mangle]
pub unsafe extern "C" fn atom_sym(name: *const c_char) -> *mut atom_t {
    // cstr_as_str() keeps pointer ownership, but Atom::sym() copies resulting
    // String into Atom::Symbol::symbol field. atom_to_ptr() moves value to the
    // heap and gives ownership to the caller.
    atom_to_ptr(Atom::Symbol(cstr_as_str(name).into()))
}

#[no_mangle]
pub unsafe extern "C" fn atom_expr(children: *const *mut atom_t, size: usize) -> *mut atom_t {
    let c_arr = std::slice::from_raw_parts(children, size);
    let children: Vec<Atom> = c_arr.into_iter().map(|atom| {
        let c_atom = Box::from_raw(*atom);
        c_atom.atom
    }).collect();
    atom_to_ptr(Atom::Expression(children.into()))
}

#[no_mangle]
pub unsafe extern "C" fn atom_var(name: *const c_char) -> *mut atom_t {
    atom_to_ptr(Atom::Variable(cstr_as_str(name).into()))
}

#[no_mangle]
pub extern "C" fn atom_gnd(gnd: *mut gnd_t) -> *mut atom_t {
    atom_to_ptr(Atom::gnd(CGroundedAtom(gnd)))
}

#[no_mangle]
pub unsafe extern "C" fn atom_free(atom: *mut atom_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(Box::from_raw(atom));
}

#[no_mangle]
pub unsafe extern "C" fn atom_to_str(atom: *const atom_t, buffer: *mut c_char, max_size: usize) -> usize {
    string_to_cstr(format!("{}", (*atom).atom), buffer, max_size)
}

#[no_mangle]
pub unsafe extern "C" fn atom_copy(atom: *const atom_t) -> *mut atom_t {
    atom_to_ptr((*atom).atom.clone())
}

#[allow(non_camel_case_types)]
pub struct grounding_space_t {
    space: GroundingSpace,
}

#[no_mangle]
pub extern "C" fn grounding_space_new() -> *mut grounding_space_t {
    Box::into_raw(Box::new(grounding_space_t{ space: GroundingSpace::new() })) 
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_free(space: *mut grounding_space_t) {
    drop(Box::from_raw(space))
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_add(space: *mut grounding_space_t, atom: *mut atom_t) {
    let c_atom = Box::from_raw(atom);
    (*space).space.add(c_atom.atom);
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct binding_t {
    var: *const c_char,
    atom: *const atom_t,
}

#[allow(non_camel_case_types)]
pub type bindings_callback_t = extern "C" fn(*const binding_t, size: usize, data: *mut c_void);

#[no_mangle]
pub unsafe extern "C" fn grounding_space_query(space: *mut grounding_space_t,
        pattern: *const atom_t, callback: bindings_callback_t, data: *mut c_void) {
    let results = (*space).space.query(&((*pattern).atom));
    for result in results {
        let vec = result.iter().map(|(k, v)| binding_t{
                var: k.name().as_ptr().cast::<c_char>(),
                atom: (v as *const Atom).cast::<atom_t>()
            }).collect::<Vec<binding_t>>();
        callback(vec.as_ptr(), vec.len(), data);
    }
}

// TODO: make a macros to generate Vec<T> definitions for C API
#[allow(non_camel_case_types)]
pub struct vec_atom_t(Vec<Atom>);

#[no_mangle]
pub extern "C" fn vec_atom_new() -> *mut vec_atom_t {
    Box::into_raw(Box::new(vec_atom_t(Vec::new()))) 
}

#[no_mangle]
pub unsafe extern "C" fn vec_pop(vec: *mut vec_atom_t) -> *const atom_t {
    atom_to_ptr((*vec).0.pop().expect("Vector is empty"))
}

#[no_mangle]
pub unsafe extern "C" fn vec_push(vec: *mut vec_atom_t, atom: *mut atom_t) {
    let c_atom = Box::from_raw(atom);
    (*vec).0.push(c_atom.atom);
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_interpret(space: *mut grounding_space_t,
        ops: *mut vec_atom_t, data: *mut vec_atom_t) -> bool {
    // TODO: think how to return the result string in case of error
    Ok(()) == (*space).space.interpret(ops.cast::<Vec<Atom>>().as_mut().unwrap(),
        data.cast::<Vec<Atom>>().as_mut().unwrap())
}

////////////////////////////////////////////////////////////////
// Code below is a boilerplate code to implement C API correctly

fn atom_to_ptr(atom: Atom) -> *mut atom_t {
    Box::into_raw(Box::new(atom_t{ atom }))
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

    unsafe fn execute(&self, ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
        let execute = self.api().execute;
        match execute {
            Some(execute) => {
                let res = execute(self.as_ptr(),
                    (ops as *mut Vec<Atom>).cast::<vec_atom_t>(),
                    (data as *mut Vec<Atom>).cast::<vec_atom_t>());
                if res.is_null() {
                    Err(cstr_as_str(res).to_string())
                } else {
                    Ok(())
                }
            },
            None => Err(format!("{} is not executable", self)),
        }
    }

    fn eq(&self, other: &Self) -> bool {
        (self.api().eq)(self.as_ptr(), other.as_ptr())
    }

    fn clone(&self) -> Self {
        CGroundedAtom((self.api().clone)(self.as_ptr()))
    }

    unsafe fn display(&self) -> &str {
        let mut buffer = [0; 4096];
        (self.api().display)(self.as_ptr(), buffer.as_mut_ptr().cast::<c_char>(), 4096);
        cstr_as_str(buffer.as_ptr().cast::<c_char>())
    }

    fn free(&self) {
        (self.api().free)(self.as_ptr());
    }

}

impl GroundedAtom for CGroundedAtom {

    fn execute(&self, ops: &mut Vec<Atom>, data: &mut Vec<Atom>) -> Result<(), String> {
        unsafe {
            self.execute(ops, data)
        }
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
        unsafe {
            write!(f, "{}", self.display())
        }
    }
}

impl Drop for CGroundedAtom {
    fn drop(&mut self) {
        self.free();
    }
}

// String conversion utilities

unsafe fn string_to_cstr(s: String, buffer: *mut c_char, max_size: usize) -> usize {
    let bytes = s.as_bytes();
    if !buffer.is_null() {
        let len = std::cmp::min(bytes.len(), max_size - 4);
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), buffer.cast::<u8>(), len);
        //let buffer = std::slice::from_raw_parts_mut::<c_char>(buffer, max_size);
        let len : isize = len.try_into().unwrap();
        if bytes.len() > max_size - 4 {
            buffer.offset(len).write('.' as c_char);
            buffer.offset(len + 1).write('.' as c_char);
            buffer.offset(len + 2).write('.' as c_char);
            buffer.offset(len + 3).write(0);
        } else {
            buffer.offset(len).write(0);
        }
    }
    bytes.len()
}

unsafe fn cstr_as_str<'a>(s: *const c_char) -> &'a str {
    CStr::from_ptr(s).to_str().expect("Incorrect UTF-8 sequence")
}
