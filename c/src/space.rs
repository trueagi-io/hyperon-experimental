use hyperon::*;
use hyperon::space::grounding::*;

use crate::atom::*;
use crate::util::str_as_cstr;

use std::os::raw::*;
use std::ffi::CString;

// GroundingSpace

#[allow(non_camel_case_types)]
pub struct grounding_space_t {
    pub space: GroundingSpace,
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
pub unsafe extern "C" fn grounding_space_eq(a: *const grounding_space_t, b: *const grounding_space_t) -> bool {
    (*a).space == (*b).space
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_add(space: *mut grounding_space_t, atom: *mut atom_t) {
    let c_atom = Box::from_raw(atom);
    (*space).space.add(c_atom.atom);
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_remove(space: *mut grounding_space_t, atom: *const atom_t) -> bool {
    (*space).space.remove(&(*atom).atom)
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_replace(space: *mut grounding_space_t, from: *const atom_t, to: *mut atom_t) -> bool {
    let c_to = Box::from_raw(to);
    (*space).space.replace(&(*from).atom, c_to.atom)
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_len(space: *const grounding_space_t) -> usize {
    (*space).space.borrow_vec().len()
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_get(space: *const grounding_space_t, idx: usize) -> *mut atom_t {
    atom_to_ptr((*space).space.borrow_vec()[idx].clone())
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub struct binding_t {
    var: *const c_char,
    atom: *const atom_t,
}

// TODO: use this idiom in other API calls which work with C strings
#[allow(non_camel_case_types)]
pub type bindings_callback_t = extern "C" fn(*const binding_t, size: usize, data: *mut c_void);

#[no_mangle]
pub extern "C" fn grounding_space_query(space: *const grounding_space_t,
        pattern: *const atom_t, callback: bindings_callback_t, data: *mut c_void) {
    let results;
    unsafe { results = (*space).space.query(&((*pattern).atom)); }
    for result in results {
        let mut vars : Vec<CString> = Vec::new();
        let vec = result.iter().map(|(k, v)| {
                // prevent C string from deallocation before callback is called
                vars.push(str_as_cstr(k.name()));
                binding_t{
                    var: vars.last().unwrap().as_ptr(),
                    atom: (v as *const Atom).cast::<atom_t>()
                }
            }).collect::<Vec<binding_t>>();
        callback(vec.as_ptr(), vec.len(), data);
    }
}

#[no_mangle]
pub extern "C" fn grounding_space_subst(space: *const grounding_space_t,
        pattern: *const atom_t, templ: *const atom_t,
        callback: atoms_callback_t, data: *mut c_void) {
    let results = unsafe { (*space).space.subst(&((*pattern).atom), &((*templ).atom)) };
    return_atoms(&results, callback, data);
}

#[no_mangle]
pub extern "C" fn interpret(space: *mut grounding_space_t, expr: *const atom_t,
        callback: atoms_callback_t, data: *mut c_void) {
    let res = unsafe { hyperon::metta::interpreter::interpret((*space).space.clone(), &(*expr).atom) };
    match res {
        Ok(vec) => return_atoms(&vec, callback, data),
        Err(_) => return_atoms(&vec![], callback, data),
    }
}


