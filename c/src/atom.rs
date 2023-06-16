use hyperon::*;
use hyperon::space::DynSpace;

use crate::util::*;
use crate::space::*;

use std::os::raw::*;
use std::fmt::Display;
use std::convert::TryInto;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::sync::atomic::{AtomicPtr, Ordering};

use hyperon::matcher::{Bindings, BindingsSet};
use std::ptr;

// Atom

#[repr(C)]
pub enum atom_type_t {
    SYMBOL,
    VARIABLE,
    EXPR,
    GROUNDED,
}

pub struct atom_t {
    pub atom: Atom,
}

pub struct exec_error_t {
    pub error: ExecError,
}

#[repr(C)]
pub struct var_atom_t {
    pub var: *const c_char,
    pub atom: *mut atom_t,
}

pub struct bindings_t {
    pub bindings: Bindings,
}

pub struct bindings_set_t {
    pub(crate) set: BindingsSet,
}

pub type bindings_callback_t = lambda_t<*const bindings_t>;
pub type bindings_mut_callback_t = lambda_t<*mut bindings_t>;

#[repr(C)]
pub struct gnd_api_t {
    // TODO: replace args by C array and ret by callback
    // One can assign NULL to this field, it means the atom is not executable
    execute: Option<extern "C" fn(*const gnd_t, *mut vec_atom_t, *mut vec_atom_t) -> *mut exec_error_t>,
    match_: Option<extern "C" fn(*const gnd_t, *const atom_t, bindings_mut_callback_t, *mut c_void)>,
    eq: extern "C" fn(*const gnd_t, *const gnd_t) -> bool,
    clone: extern "C" fn(*const gnd_t) -> *mut gnd_t,
    display: extern "C" fn(*const gnd_t, *mut c_char, usize) -> usize,
    free: extern "C" fn(*mut gnd_t),
}

#[repr(C)]
pub struct gnd_t {
    api: *const gnd_api_t,
    typ: *mut atom_t,
}

#[no_mangle]
pub extern "C" fn exec_error_runtime(message: *const c_char) -> *mut exec_error_t {
    Box::into_raw(Box::new(exec_error_t{ error: ExecError::Runtime(cstr_into_string(message)) }))
}

#[no_mangle]
pub extern "C" fn exec_error_no_reduce() -> *mut exec_error_t {
    Box::into_raw(Box::new(exec_error_t{ error: ExecError::NoReduce }))
}

#[no_mangle]
pub extern "C" fn exec_error_free(error: *mut exec_error_t) {
    unsafe{ drop(Box::from_raw(error)); }
}

// bindings
#[no_mangle]
pub extern "C" fn bindings_new() -> *mut bindings_t {
    bindings_into_ptr(Bindings::new())
}

#[no_mangle]
pub extern "C" fn bindings_free(bindings: *mut bindings_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(unsafe{Box::from_raw(bindings)});
}

#[no_mangle]
pub extern "C" fn bindings_clone(bindings: *const bindings_t) -> *mut bindings_t {
    bindings_into_ptr(unsafe{ &(*bindings) }.bindings.clone())
}

/// Writes a text description of the bindings_t into the provided buffer and returns the number of bytes
/// written, or that would have been written had the buf_len been large enough, excluding the
/// string terminator.
#[no_mangle]
pub extern "C" fn bindings_to_str(bindings: *const bindings_t, buf: *mut c_char, buf_len: usize) -> usize {
    let bindings = unsafe{ &(*bindings).bindings };
    write_into_buf(bindings, buf, buf_len)
}

#[no_mangle]
pub extern "C" fn bindings_eq(bindingsa: *const bindings_t, bindingsb: *const bindings_t) -> bool {
    let left = unsafe{&(*bindingsa).bindings};
    let right = unsafe{&(*bindingsb).bindings};
    left == right
}

#[no_mangle]
pub extern "C" fn bindings_traverse(cbindings: *const bindings_t, callback: lambda_t<* const var_atom_t>, context: *mut c_void) {
    let bindings = unsafe{&(*cbindings).bindings};
    bindings.iter().for_each(|(var, atom)|  {
            let name = string_as_cstr(var.name());
            let var_atom = var_atom_t {
                var: name.as_ptr(),
                atom: atom_into_ptr(atom)
            };
            callback(&var_atom, context);
        }
    )
}

#[no_mangle]
pub extern "C" fn bindings_add_var_binding(bindings: * mut bindings_t, var_atom: *const var_atom_t) -> bool {
    let bindings = unsafe{ &mut(*bindings).bindings };
    let var = VariableAtom::new(cstr_into_string (unsafe{(*var_atom).var}));
    let atom = ptr_into_atom(unsafe{(*var_atom).atom});
    match bindings.clone().add_var_binding_v2(var, atom) {
        Ok(new_bindings) => {
            *bindings = new_bindings;
            true
        },
        Err(_) => false
    }
}

#[no_mangle]
pub extern "C" fn bindings_is_empty(bindings: *const bindings_t) -> bool{
    let bindings = unsafe{ &(*bindings).bindings };
    bindings.is_empty()
}

// TODO: discuss if var_atom_t is more convenient
//       using Option cause `extern` fn uses type `Option<atom::atom_t>`, which is not FFI-safe warning
#[no_mangle]
pub extern "C" fn bindings_resolve(bindings: *const bindings_t, var_name: *const c_char) -> * mut atom_t
{
    let bindings = unsafe{&(*bindings).bindings};
    let var = VariableAtom::new(cstr_into_string(var_name));

    match bindings.resolve(&var) {
        None => { ptr::null_mut() }
        Some(atom) => { atom_into_ptr(atom) }
    }
}

#[no_mangle]
pub extern "C" fn bindings_merge(bindings_left: *const bindings_t, bindings_right: *const bindings_t) -> *mut bindings_t
{
    let bindings_l = unsafe{ &(*bindings_left).bindings };
    let bindings_r = unsafe{ &(*bindings_right).bindings };

    match Bindings::merge(bindings_l, bindings_r){
        None => { ptr::null_mut() }
        Some(merged) => { bindings_into_ptr(merged)}
    }
}

/// WARNING: This function takes ownership of the _self argument.
/// After calling this function, the bindings_t passed as _self must not be accessed or freed
///
/// The object returned from this function must be freed with bindings_set_free()
#[no_mangle]
pub extern "C" fn bindings_merge_v2(_self: *mut bindings_t, other: *const bindings_t) -> *mut bindings_set_t
{
    let other = unsafe{ &(*other).bindings };
    let owned_self = ptr_into_bindings(_self);

    let new_set = owned_self.merge_v2(other);
    bindings_set_into_ptr(new_set)
}

#[no_mangle]
pub extern "C" fn bindings_resolve_and_remove(bindings: *mut bindings_t, var_name: *const c_char) -> *mut atom_t {
    let bindings = unsafe{&mut(*bindings).bindings};
    let var = VariableAtom::new(cstr_into_string(var_name));

    match bindings.resolve_and_remove(&var) {
        None => { ptr::null_mut() }
        Some(removed) =>{ atom_into_ptr(removed) }
    }
}

#[no_mangle]
pub extern "C" fn bindings_narrow_vars(bindings: *mut bindings_t, vars: *const vec_atom_t) {
    let bindings = unsafe{&mut (*bindings).bindings};
    let vars = unsafe{&(*vars).0};
    let vars_iter = vars.into_iter().map(|atom| {
        TryInto::<&VariableAtom>::try_into(atom)
            .expect("Only variable atoms allowed for bindings_narrow_vars")
    });
    let vars_set = HashSet::from_iter(vars_iter);

    let mut new_bindings = bindings.narrow_vars(&vars_set);
    core::mem::swap(&mut new_bindings, bindings);
}

// end of bindings

// bindings_set

#[no_mangle]
pub extern "C" fn bindings_set_empty() -> *mut bindings_set_t {
    bindings_set_into_ptr(BindingsSet::empty())
}

#[no_mangle]
pub extern "C" fn bindings_set_single() -> *mut bindings_set_t {
    bindings_set_into_ptr(BindingsSet::single())
}

/// WARNING: This function takes ownership of the bindings argument.
/// After calling this function, the bindings_t passed must not be accessed or freed
///
/// The object returned from this function must be freed with bindings_set_free()
#[no_mangle]
pub extern "C" fn bindings_set_from_bindings(bindings: *mut bindings_t) -> *mut bindings_set_t {
    let owned_bindings = ptr_into_bindings(bindings);
    bindings_set_into_ptr(BindingsSet::from(owned_bindings))
}

/// WARNING: This function takes ownership of the bindings argument.
/// After calling this function, the bindings_t passed must not be accessed or freed
#[no_mangle]
pub extern "C" fn bindings_set_push(set: *mut bindings_set_t, bindings: *mut bindings_t) {
    let set = unsafe{&mut (*set).set};
    let owned_bindings = ptr_into_bindings(bindings);
    set.push(owned_bindings);
}

#[no_mangle]
pub extern "C" fn bindings_set_free(set: *mut bindings_set_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(unsafe{Box::from_raw(set)});
}

#[no_mangle]
pub extern "C" fn bindings_set_clone(set: *const bindings_set_t) -> *mut bindings_set_t {
    let set = unsafe{&(*set).set};
    bindings_set_into_ptr(set.clone())
}

#[no_mangle]
pub extern "C" fn bindings_set_eq(set: *const bindings_set_t, other: *const bindings_set_t) -> bool {
    let set = unsafe{&(*set).set};
    let other = unsafe{&(*other).set};
    set == other
}

/// Writes a text description of the bindings_set_t into the provided buffer and returns the number of bytes
/// written, or that would have been written had the buf_len been large enough, excluding the
/// string terminator.
#[no_mangle]
pub extern "C" fn bindings_set_to_str(set: *const bindings_set_t, buf: *mut c_char, buf_len: usize) -> usize {
    let set = unsafe{ &(*set).set };
    write_into_buf(set, buf, buf_len)
}

#[no_mangle]
pub extern "C" fn bindings_set_is_empty(set: *const bindings_set_t) -> bool {
    let set = unsafe{ &(*set).set };
    set.is_empty()
}

#[no_mangle]
pub extern "C" fn bindings_set_is_single(set: *const bindings_set_t) -> bool {
    let set = unsafe{ &(*set).set };
    set.is_single()
}

#[no_mangle]
pub extern "C" fn bindings_set_iterate(set: *mut bindings_set_t, callback: bindings_mut_callback_t, context: *mut c_void) {
    let set = unsafe{ &mut (*set).set };
    for bindings in set.iter_mut() {
        let bindings_ptr = (bindings as *mut Bindings).cast::<bindings_t>();
        callback(bindings_ptr, context);
    }
}

#[no_mangle]
pub extern "C" fn bindings_set_add_var_equality(set: *mut bindings_set_t, a: *const atom_t, b: *const atom_t) {
    let set = unsafe{ &mut(*set).set };
    let a = unsafe{ &(*a).atom };
    let b = unsafe{ &(*b).atom };

    let mut owned_set = BindingsSet::empty();
    core::mem::swap(&mut owned_set, set);
    let mut result_set = owned_set.add_var_equality(a.try_into().unwrap(), b.try_into().unwrap());
    core::mem::swap(&mut result_set, set);
}

#[no_mangle]
pub extern "C" fn bindings_set_add_var_binding(set: *mut bindings_set_t, var: *const atom_t, value: *const atom_t) {
    let set = unsafe{ &mut(*set).set };
    let var = unsafe{ &(*var).atom };
    let value = unsafe{ &(*value).atom };

    let mut owned_set = BindingsSet::empty();
    core::mem::swap(&mut owned_set, set);
    let mut result_set = owned_set.add_var_binding(TryInto::<&VariableAtom>::try_into(var).unwrap(), value);
    core::mem::swap(&mut result_set, set);
}

#[no_mangle]
pub extern "C" fn bindings_set_merge_into(_self: *mut bindings_set_t, other: *const bindings_set_t) {
    let _self = unsafe{ &mut (*_self).set };
    let other = unsafe{ &(*other).set };
    let mut owned_self = BindingsSet::empty();
    core::mem::swap(_self, &mut owned_self);

    let mut result_set = owned_self.merge(other);
    core::mem::swap(_self, &mut result_set);
}

// end of bindings_set functions

#[no_mangle]
pub unsafe extern "C" fn atom_sym(name: *const c_char) -> *mut atom_t {
    // cstr_as_str() keeps pointer ownership, but Atom::sym() copies resulting
    // String into Atom::Symbol::symbol field. atom_into_ptr() moves value to the
    // heap and gives ownership to the caller.
    atom_into_ptr(Atom::sym(cstr_as_str(name)))
}

#[no_mangle]
pub unsafe extern "C" fn atom_expr(children: *const *mut atom_t, size: usize) -> *mut atom_t {
    let c_arr = std::slice::from_raw_parts(children, size);
    let children: Vec<Atom> = c_arr.into_iter().map(|atom| {
        ptr_into_atom(*atom)
    }).collect();
    atom_into_ptr(Atom::expr(children))
}

#[no_mangle]
pub unsafe extern "C" fn atom_var(name: *const c_char) -> *mut atom_t {
    atom_into_ptr(Atom::var(cstr_as_str(name)))
}

#[no_mangle]
pub extern "C" fn atom_gnd(gnd: *mut gnd_t) -> *mut atom_t {
    atom_into_ptr(Atom::gnd(CGrounded(AtomicPtr::new(gnd))))
}

/// Returns a new grounded `atom_t` referencing the space
/// 
/// This function does not consume the space and the space still must be freed with `space_free`
#[no_mangle]
pub extern "C" fn atom_gnd_for_space(space: *mut space_t) -> *mut atom_t {
    let space = unsafe { &(*space).0 };
    atom_into_ptr(Atom::gnd(space.clone()))
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

/// Writes a text description of the atom into the provided buffer and returns the number of bytes
/// written, or that would have been written had the buf_len been large enough, excluding the
/// string terminator.
#[no_mangle]
pub extern "C" fn atom_to_str(atom: *const atom_t, buf: *mut c_char, buf_len: usize) -> usize {
    let atom = unsafe{ &(*atom).atom };
    write_into_buf(atom, buf, buf_len)
}

/// Writes the name of the atom into the provided buffer and returns the number of bytes
/// written, or that would have been written had the buf_len been large enough, excluding the
/// string terminator.
#[no_mangle]
pub extern "C" fn atom_get_name(atom: *const atom_t, buf: *mut c_char, buf_len: usize) -> usize {
    let atom = unsafe{ &(*atom).atom };
    match atom {
        Atom::Symbol(s) => write_into_buf(s.name(), buf, buf_len),
        Atom::Variable(v) => write_into_buf(v.name(), buf, buf_len),
        _ => panic!("Only Symbol and Variable has name attribute!"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_object(atom: *const atom_t) -> *mut gnd_t {
    if let Atom::Grounded(ref g) = (*atom).atom {
        match (*g).as_any_ref().downcast_ref::<CGrounded>() {
            Some(g) => g.get_mut_ptr(),
            None => panic!("Returning non C grounded objects is not implemented yet!"),
        }
    } else {
        panic!("Only Grounded has object attribute!");
    }
}

/// Returns a space_t from a grounded atom referencing the space
/// 
/// The returned space is borrowed from the atom, and should not be freed nor accessed after the atom
/// has been freed.
#[no_mangle]
pub unsafe extern "C" fn atom_get_space(atom: *const atom_t) -> *const space_t {
    let atom = &(*atom).atom;
    if let Some(space) = Atom::as_gnd::<DynSpace>(atom) {
        (space as *const DynSpace).cast()
    } else {
        panic!("Atom does not reference a space");
    }
}

#[no_mangle]
pub extern "C" fn atom_get_grounded_type(atom: *const atom_t) -> *mut atom_t {
    if let Atom::Grounded(ref g) = unsafe{ &(*atom) }.atom {
        atom_into_ptr(g.type_())
    } else {
        panic!("Only Grounded atoms has grounded type attribute!");
    }
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_children(atom: *const atom_t,
        callback: c_atoms_callback_t, context: *mut c_void) {
    if let Atom::Expression(ref e) = (*atom).atom {
        return_atoms(e.children(), callback, context);
    } else {
        panic!("Only Expression has children!");
    }
}

/// Performs a depth-first exhaustive iteration of an atom and all its children recursively.
/// The first result returned will be the atom itself
#[no_mangle]
pub unsafe extern "C" fn atom_iterate(atom: *const atom_t,
        callback: c_atom_callback_t, context: *mut c_void) {
    let atom = &(*atom).atom;
    for inner_atom in AtomIter::new(atom) {
        callback((inner_atom as *const Atom).cast(), context);
    }
}

/// The object returned from this function must be freed with bindings_set_free()
#[no_mangle]
pub extern "C" fn atom_match_atom(a: *const atom_t, b: *const atom_t) -> *mut bindings_set_t {
    let a = unsafe{ &(*a).atom };
    let b = unsafe{ &(*b).atom };
    let result_set: BindingsSet = crate::atom::matcher::match_atoms(a, b).collect();
    bindings_set_into_ptr(result_set)
}

#[no_mangle]
pub unsafe extern "C" fn atom_free(atom: *mut atom_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(Box::from_raw(atom));
}

#[no_mangle]
pub extern "C" fn atom_clone(atom: *const atom_t) -> *mut atom_t {
    atom_into_ptr(unsafe{ &(*atom) }.atom.clone())
}

#[no_mangle]
pub unsafe extern "C" fn atom_eq(atoma: *const atom_t, atomb: *const atom_t) -> bool {
    (*atoma).atom == (*atomb).atom
}

// TODO: make a macros to generate Vec<T> definitions for C API
pub struct vec_atom_t(pub(crate) Vec<Atom>);

#[no_mangle]
pub extern "C" fn vec_atom_new() -> *mut vec_atom_t {
    vec_atom_into_ptr(Vec::new())
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
    atom_into_ptr((*vec).0.pop().expect("Vector is empty"))
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_push(vec: *mut vec_atom_t, atom: *mut atom_t) {
    (*vec).0.push(ptr_into_atom(atom));
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_len(vec: *const vec_atom_t) -> usize {
    (*vec).0.len()
}

/// WARNING: The atom returned from this function remains owned by the vec_atom_t.  It should NOT be freed.
#[no_mangle]
pub unsafe extern "C" fn vec_atom_get(vec: *const vec_atom_t, idx: usize) -> *const atom_t {
    (&(*vec).0[idx] as *const Atom).cast()
}

pub type atom_array_t = array_t<*const atom_t>;
pub type c_atoms_callback_t = lambda_t<atom_array_t>;

pub type c_atom_callback_t = lambda_t<*const atom_t>;

#[no_mangle]
pub extern "C" fn atoms_are_equivalent(first: *const atom_t, second: *const atom_t) -> bool {
    crate::atom::matcher::atoms_are_equivalent(&unsafe{ &*first }.atom, &unsafe{ &*second }.atom)
}

/////////////////////////////////////////////////////////////////
// Code below is a boilerplate code to implement C API correctly.
// It is not a part of C API.

pub fn atom_into_ptr(atom: Atom) -> *mut atom_t {
    Box::into_raw(Box::new(atom_t{ atom }))
}

pub fn bindings_into_ptr(bindings: Bindings) -> *mut bindings_t {
    Box::into_raw(Box::new(bindings_t{bindings}))
}

pub fn ptr_into_atom(atom: *mut atom_t) -> Atom {
    unsafe{ Box::from_raw(atom) }.atom
}

pub fn ptr_into_bindings(bindings: *mut bindings_t) -> Bindings {
    unsafe {Box::from_raw(bindings)}.bindings
}

pub fn bindings_set_into_ptr(set: BindingsSet) -> *mut bindings_set_t {
    Box::into_raw(Box::new(bindings_set_t{set}))
}

fn vec_atom_into_ptr(vec: Vec<Atom>) -> *mut vec_atom_t {
    Box::into_raw(Box::new(vec_atom_t(vec)))
}

pub fn return_atoms(atoms: &Vec<Atom>, callback: c_atoms_callback_t, context: *mut c_void) {
    let results: Vec<*const atom_t> = atoms.iter()
        .map(|atom| (atom as *const Atom).cast::<atom_t>()).collect();
    callback((&results).into(), context);
}

// C grounded atom wrapper

#[derive(Debug)]
struct CGrounded(AtomicPtr<gnd_t>);

impl CGrounded {
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

    fn free(&mut self) {
        (self.api().free)(self.get_mut_ptr());
    }

    extern "C" fn match_callback(cbindings: *mut bindings_t, context: *mut c_void) {
        let bindings = ptr_into_bindings(cbindings);
        let vec_bnd = unsafe{ &mut *context.cast::<Vec<Bindings>>() };
        vec_bnd.push(bindings);
    }

}


impl Grounded for CGrounded {
    fn type_(&self) -> Atom {
        unsafe{ &*(*self.get_ptr()).typ }.atom.clone()
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let mut ret = Vec::new();
        match self.api().execute {
            Some(func) => {
                let error = func(self.get_ptr(), (args as *mut Vec<Atom>).cast::<vec_atom_t>(),
                (&mut ret as *mut Vec<Atom>).cast::<vec_atom_t>());
                let ret = if error.is_null() {
                    Ok(ret)
                } else {
                    let error = unsafe{ Box::from_raw(error) };
                    Err(error.error)
                };
                log::trace!("CGrounded::execute: atom: {:?}, args: {:?}, ret: {:?}", self, args, ret);
                ret
            },
            None => execute_not_executable(self)
        }
    }

    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        match self.api().match_ {
            Some(func) => {
                let mut results: Vec<Bindings> = Vec::new();
                let context = (&mut results as *mut Vec<Bindings>).cast::<c_void>();
                func(self.get_ptr(), (other as *const Atom).cast::<atom_t>(),
                    CGrounded::match_callback, context);
                Box::new(results.into_iter())
            },
            None => match_by_equality(self, other)
        }
    }
}

impl PartialEq for CGrounded {
    fn eq(&self, other: &CGrounded) -> bool {
        (self.api().eq)(self.get_ptr(), other.get_ptr())
    }
}

impl Clone for CGrounded {
    fn clone(&self) -> Self {
        CGrounded(AtomicPtr::new((self.api().clone)(self.get_ptr())))
    }
}

impl Display for CGrounded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = [0u8; 4096];
        (self.api().display)(self.get_ptr(), buffer.as_mut_ptr().cast::<c_char>(), 4096);
        let text = cstr_into_string(buffer.as_ptr().cast::<c_char>());
        write!(f, "{}", text)
    }
}

impl Drop for CGrounded {
    fn drop(&mut self) {
        self.free();
    }
}

#[cfg(test)]
mod tests {
use super::*;
use std::ptr;

    #[test]
    pub fn test_match_callback() {

        let cbindings = bindings_into_ptr(bind!{var: expr!("atom_test")} );

        let mut results: Vec<Bindings> = Vec::new();
        let context = ptr::addr_of_mut!(results).cast::<c_void>();

        CGrounded::match_callback( cbindings, context);

        assert_eq!(results, vec![Bindings::from(vec![
                (VariableAtom::new("var"), Atom::sym("atom_test"))])]);
    }

}
