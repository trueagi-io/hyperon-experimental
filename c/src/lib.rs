use hyperon::*;
use hyperon::text::*;

use std::ffi::*;
use std::os::raw::*;
use std::fmt::Display;
use regex::Regex;

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
        match (**g).downcast_ref::<CGroundedAtom>() {
            Some(g) => g.as_ptr(),
            None => panic!("Returning non C grounded objects is not implemented yet!"),
        }
    } else {
        panic!("Only Grounded has object attribute!");
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

// GroundingSpace

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
pub unsafe extern "C" fn grounding_space_eq(a: *const grounding_space_t, b: *const grounding_space_t) -> bool {
    (*a).space == (*b).space
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_add(space: *mut grounding_space_t, atom: *mut atom_t) {
    let c_atom = Box::from_raw(atom);
    (*space).space.add(c_atom.atom);
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_len(space: *const grounding_space_t) -> usize {
    (*space).space.as_vec().len()
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_get(space: *const grounding_space_t, idx: usize) -> *const atom_t {
    (&(*space).space.as_vec()[idx] as *const Atom).cast::<atom_t>()
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
pub unsafe extern "C" fn vec_atom_free(vec: *mut vec_atom_t) {
    drop(Box::from_raw(vec));
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
pub unsafe extern "C" fn grounding_space_interpret(space: *mut grounding_space_t,
        ops: *mut vec_atom_t, data: *mut vec_atom_t) -> bool {
    // TODO: think how to return the result string in case of error
    Ok(()) == (*space).space.interpret(ops.cast::<Vec<Atom>>().as_mut().unwrap(),
        data.cast::<Vec<Atom>>().as_mut().unwrap())
}

// SExprSpace

#[allow(non_camel_case_types)]
pub struct sexpr_space_t {
    space: SExprSpace, 
}

#[no_mangle]
pub extern "C" fn sexpr_space_new() -> *mut sexpr_space_t {
    Box::into_raw(Box::new(sexpr_space_t{ space: SExprSpace::new() })) 
}

#[no_mangle]
pub unsafe extern "C" fn sexpr_space_free(space: *mut sexpr_space_t) {
    drop(Box::from_raw(space)) 
}

// TODO: think how to return the result string in case of error
#[no_mangle]
pub unsafe extern "C" fn sexpr_space_add_str(space: *mut sexpr_space_t, text: *const c_char) -> bool {
    Ok(()) == (*space).space.add_str(cstr_as_str(text))
}

#[allow(non_camel_case_types)]
type atom_constr_t = extern "C" fn(*const c_char) -> *mut atom_t;

#[no_mangle]
pub unsafe extern "C" fn sexpr_space_register_token(space: *mut sexpr_space_t,
    regex: *const c_char, constr: atom_constr_t) {
    let regex = Regex::new(cstr_as_str(regex)).unwrap();
    (*space).space.register_token(regex, move |token| {
        let catom = Box::from_raw(constr(token.as_ptr().cast::<c_char>()));
        catom.atom
    });
}

#[no_mangle]
pub unsafe extern "C" fn sexpr_space_into_grounding_space(sexpr: *const sexpr_space_t,
        gnd: *mut grounding_space_t) {
    (*sexpr).space.into_grounding_space(&mut (*gnd).space);
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

unsafe fn cstr_as_str<'a>(s: *const c_char) -> &'a str {
    CStr::from_ptr(s).to_str().expect("Incorrect UTF-8 sequence")
}

fn str_as_cstr<'a>(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}
