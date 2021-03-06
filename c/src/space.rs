use hyperon::*;
use hyperon::space::grounding::*;

use crate::atom::*;
use crate::util::*;

use std::os::raw::*;
use std::ffi::CString;

// GroundingSpace

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

#[repr(C)]
pub struct binding_t {
    var: *const c_char,
    atom: *const atom_t,
}

pub type binding_array_t = array_t<binding_t>;

#[no_mangle]
pub extern "C" fn grounding_space_query(space: *const grounding_space_t,
        pattern: *const atom_t, callback: lambda_t<binding_array_t>, context: *mut c_void) {
    let results = unsafe { (*space).space.query(&((*pattern).atom)) };
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
        callback((&vec).into(), context);
    }
}

#[no_mangle]
pub extern "C" fn grounding_space_subst(space: *const grounding_space_t,
        pattern: *const atom_t, templ: *const atom_t,
        callback: c_atoms_callback_t, context: *mut c_void) {
    let results = unsafe { (*space).space.subst(&((*pattern).atom), &((*templ).atom)) };
    return_atoms(&results, callback, context);
}


