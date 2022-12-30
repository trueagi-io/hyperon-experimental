use hyperon::space::grounding::*;

use crate::atom::*;
use crate::util::*;

use std::os::raw::*;
use std::ffi::CString;

// GroundingSpace

pub type grounding_space_t = SharedApi<GroundingSpace>;

#[no_mangle]
pub extern "C" fn grounding_space_new() -> *mut grounding_space_t {
    grounding_space_t::new(GroundingSpace::new())
}

#[no_mangle]
pub extern "C" fn grounding_space_free(space: *mut grounding_space_t) {
    grounding_space_t::drop(space)
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_eq(a: *const grounding_space_t, b: *const grounding_space_t) -> bool {
    *a == *b
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_add(space: *mut grounding_space_t, atom: *mut atom_t) {
    (*space).borrow_mut().add(ptr_to_atom(atom));
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_remove(space: *mut grounding_space_t, atom: *const atom_t) -> bool {
    (*space).borrow_mut().remove(&(*atom).atom)
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_replace(space: *mut grounding_space_t, from: *const atom_t, to: *mut atom_t) -> bool {
    (*space).borrow_mut().replace(&(*from).atom, ptr_to_atom(to))
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_len(space: *const grounding_space_t) -> usize {
    (*space).borrow().iter().count()
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_get(space: *const grounding_space_t, idx: usize) -> *mut atom_t {
    // TODO: highly ineffective implementation, should be reworked after replacing
    // the GroundingSpace struct by Space trait in code.
    atom_to_ptr((*space).borrow().iter().skip(idx).next()
        .expect(format!("Index is out of bounds: {}", idx).as_str()).clone())
}

#[no_mangle]
pub extern "C" fn grounding_space_query(space: *const grounding_space_t,
        pattern: *const atom_t, callback: lambda_t<binding_array_t>, context: *mut c_void) {
    let results = unsafe { (*space).borrow().query(&((*pattern).atom)) };
    for result in results.into_iter() {
        let mut vars : Vec<CString> = Vec::new();
        let vec: Vec<binding_t> = result.into_iter().map(|(k, v)| {
                // put C string into collection which is external to closure
                // to prevent its deallocation before callback is called
                vars.push(string_as_cstr(k.name()));
                binding_t{
                    var: vars.last().unwrap().as_ptr(),
                    atom: atom_to_ptr(v),
                }
            }).collect();
        callback((&vec).into(), context);
    }
}

#[no_mangle]
pub extern "C" fn grounding_space_subst(space: *const grounding_space_t,
        pattern: *const atom_t, templ: *const atom_t,
        callback: c_atoms_callback_t, context: *mut c_void) {
    let results = unsafe { (*space).borrow().subst(&((*pattern).atom), &((*templ).atom)) };
    return_atoms(&results, callback, context);
}


