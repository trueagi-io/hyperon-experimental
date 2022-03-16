use hyperon::*;

use crate::util::*;

use std::os::raw::*;
use std::fmt::Debug;
use std::sync::atomic::{AtomicPtr, Ordering};

// Atom

#[allow(non_camel_case_types)]
#[repr(C)]
pub enum atom_type_t {
    SYMBOL,
    VARIABLE,
    EXPR,
    GROUNDED,
}

#[allow(non_camel_case_types)]
pub struct atom_t {
    pub atom: Atom,
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct gnd_api_t {
    // TODO: replace args by C array and ret by callback
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
    atom_to_ptr(Atom::gnd(CGroundedAtom(AtomicPtr::new(gnd))))
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_type(atom: *const atom_t) -> atom_type_t {
    match (*atom).atom {
        Atom::Symbol(_) => atom_type_t::SYMBOL,
        Atom::Variable(_) => atom_type_t::VARIABLE,
        Atom::Expression(_) => atom_type_t::EXPR,
        Atom::Grounded(_) => atom_type_t::GROUNDED,
    }
}

#[allow(non_camel_case_types)]
type c_str_callback_t = extern "C" fn(str: *const c_char, context: *mut c_void) -> ();

#[no_mangle]
pub unsafe extern "C" fn atom_to_str(atom: *const atom_t, callback: c_str_callback_t, context: *mut c_void) {
    callback(str_as_cstr(format!("{}", (*atom).atom).as_str()).as_ptr(), context);
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_name(atom: *const atom_t, callback: c_str_callback_t, context: *mut c_void) {
    match &((*atom).atom) {
        Atom::Symbol(s) => callback(str_as_cstr(s.name()).as_ptr(), context),
        Atom::Variable(v) => callback(str_as_cstr(v.name()).as_ptr(), context),
        _ => panic!("Only Symbol and Variable has name attribute!"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_object(atom: *const atom_t) -> *mut gnd_t {
    if let Atom::Grounded(ref g) = (*atom).atom {
        match (*g).downcast_ref::<CGroundedAtom>() {
            Some(g) => g.get_mut_ptr(),
            None => panic!("Returning non C grounded objects is not implemented yet!"),
        }
    } else {
        panic!("Only Grounded has object attribute!");
    }
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_children(atom: *const atom_t,
        callback: atoms_callback_t, data: *mut c_void) {
    if let Atom::Expression(ref e) = (*atom).atom {
        return_atoms(e.children(), callback, data);
    } else {
        panic!("Only Expression has children!");
    }
}

#[no_mangle]
pub unsafe extern "C" fn atom_free(atom: *mut atom_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(Box::from_raw(atom));
}

#[no_mangle]
pub unsafe extern "C" fn atom_copy(atom: *const atom_t) -> *mut atom_t {
    atom_to_ptr((*atom).atom.clone())
}

#[no_mangle]
pub unsafe extern "C" fn atom_eq(atoma: *const atom_t, atomb: *const atom_t) -> bool {
    (*atoma).atom == (*atomb).atom
}

// TODO: make a macros to generate Vec<T> definitions for C API
#[allow(non_camel_case_types)]
pub struct vec_atom_t(Vec<Atom>);

#[no_mangle]
pub extern "C" fn vec_atom_new() -> *mut vec_atom_t {
    vec_atom_to_ptr(Vec::new()) 
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_free(vec: *mut vec_atom_t) {
    drop(Box::from_raw(vec));
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_size(vec: *mut vec_atom_t) -> usize {
    (*vec).0.len()
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_pop(vec: *mut vec_atom_t) -> *mut atom_t {
    atom_to_ptr((*vec).0.pop().expect("Vector is empty"))
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_push(vec: *mut vec_atom_t, atom: *mut atom_t) {
    let c_atom = Box::from_raw(atom);
    (*vec).0.push(c_atom.atom);
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_len(vec: *const vec_atom_t) -> usize {
    (*vec).0.len()
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_get(vec: *mut vec_atom_t, idx: usize) -> *mut atom_t {
    atom_to_ptr((*vec).0[idx].clone())
}

#[allow(non_camel_case_types)]
pub type atoms_callback_t = extern "C" fn(*const *const atom_t, size: usize, data: *mut c_void);

/////////////////////////////////////////////////////////////////
// Code below is a boilerplate code to implement C API correctly.
// It is not a part of C API.

pub fn atom_to_ptr(atom: Atom) -> *mut atom_t {
    Box::into_raw(Box::new(atom_t{ atom }))
}

fn vec_atom_to_ptr(vec: Vec<Atom>) -> *mut vec_atom_t {
    Box::into_raw(Box::new(vec_atom_t(vec)))
}

pub fn return_atoms(atoms: &Vec<Atom>, callback: atoms_callback_t, data: *mut c_void) {
    let results: Vec<*const atom_t> = atoms.iter()
        .map(|atom| (atom as *const Atom).cast::<atom_t>()).collect();
    callback(results.as_ptr(), results.len(), data);
}

// C grounded atom wrapper

struct CGroundedAtom(AtomicPtr<gnd_t>);

impl CGroundedAtom {

    fn get_mut_ptr(&self) -> *mut gnd_t {
        self.0.load(Ordering::Acquire)
    }

    fn get_ptr(&self) -> *const gnd_t {
        self.0.load(Ordering::Acquire)
    }

    fn api(&self) -> &gnd_api_t {
        unsafe {
            &*(*self.get_ptr()).api
        }
    }

    unsafe fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        let execute = self.api().execute;
        match execute {
            Some(execute) => {
                let mut ret = Vec::new();
                let res = execute(self.get_ptr(),
                    (args as *mut Vec<Atom>).cast::<vec_atom_t>(),
                    (&mut ret as *mut Vec<Atom>).cast::<vec_atom_t>());
                if res.is_null() {
                    Ok(ret)
                } else {
                    Err(cstr_as_str(res).to_string())
                }
            },
            None => Err(format!("{:?} is not executable", self)),
        }
    }

    fn eq(&self, other: &Self) -> bool {
        (self.api().eq)(self.get_ptr(), other.get_ptr())
    }

    fn clone(&self) -> Self {
        CGroundedAtom(AtomicPtr::new((self.api().clone)(self.get_ptr())))
    }

    unsafe fn display(&self) -> String {
        let mut buffer = [0u8; 4096];
        (self.api().display)(self.get_ptr(), buffer.as_mut_ptr().cast::<c_char>(), 4096);
        cstr_into_string(buffer.as_ptr().cast::<c_char>())
    }

    fn free(&mut self) {
        (self.api().free)(self.get_mut_ptr());
    }

}

impl GroundedAtom for CGroundedAtom {

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, String> {
        unsafe {
            self.execute(args)
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

impl Debug for CGroundedAtom {
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

