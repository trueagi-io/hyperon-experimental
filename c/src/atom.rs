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

//Implementation Notes: both `atom_t` and `atom_ref_t` are transparent wrappers around a RustAtom,
// which internally knows whether it owns or borrows the native `Atom` struct.  The reason for this
// design choice is because at allows a pointer to `atom_ref` to be used interchangeably with a
// pointer to `atom_t`
//
//TODO for Alpha: Explain this in user-facing API docs, along with ownership conventions
#[repr(C)]
enum RustAtom {
    None,
    Owned(Box<RustOpaqueAtom>),
    Borrowed(*const RustOpaqueAtom)
}

struct RustOpaqueAtom(Atom);

impl RustAtom {
    pub(crate) fn is_null(&self) -> bool {
        match self {
            Self::None => true,
            Self::Owned(_) => false,
            Self::Borrowed(atom_ptr) => atom_ptr.is_null()
        }
    }
    pub(crate) fn borrow(&self) -> &Atom {
        match self {
            Self::None => panic!("Attempt to access NULL atom"),
            Self::Owned(atom) => &atom.0,
            Self::Borrowed(atom_ptr) => unsafe{ &(**atom_ptr).0 }
        }
    }
    pub(crate) fn into_ref(self) -> &'static Atom {
        match self {
            Self::None => panic!("Attempt to access NULL atom"),
            Self::Owned(_) => panic!("atom_ref must reference an atom stored elsewhere"),
            Self::Borrowed(atom_ptr) => unsafe{ &(*atom_ptr).0 }
        }
    }
    pub(crate) fn into_inner(self) -> Atom {
        match self {
            Self::None => panic!("Attempt to access NULL atom"),
            Self::Owned(atom) => atom.0,
            Self::Borrowed(_) => panic!("Can't extract borrowed atom"),
        }
    }
}

/// Contains an Atom of any type
///
/// `atom_t` must be freed with `atom_free`, or passed by value to a function that takes ownership
/// of the atom.
//NOTE: In the future, we will want this struct to actually mirror a Rust atom's internals, or
// possibly just reexport them.
#[repr(transparent)]
pub struct atom_t {
    atom: RustAtom,
}

impl From<Atom> for atom_t {
    fn from(atom: Atom) -> Self {
        Self{ atom: RustAtom::Owned(Box::new(RustOpaqueAtom(atom))) }
    }
}

impl From<Option<Atom>> for atom_t {
    fn from(atom: Option<Atom>) -> Self {
        match atom {
            Some(atom) => atom.into(),
            None => Self::null()
        }
    }
}

impl atom_t {
    pub(crate) fn null() -> Self {
        Self{ atom: RustAtom::None }
    }
    pub(crate) fn borrow(&self) -> &Atom {
        self.atom.borrow()
    }
    pub(crate) fn into_inner(self) -> Atom {
        self.atom.into_inner()
    }
}

/// Refers to an Atom owned by another object.  It is not necessary to free `atom_ref_t`.
///
/// NOTE: A pointer to `atom_t` can be passed to any function requesting with a pointer to `atom_ref_t`.
///
/// WARNING: `atom_ref_t` must not be accessed beyond the lifetime of the object which owns the atom
/// it references.
///
//NOTE: In the future, atom_ref_t will probably just be a `*const Atom` internally, and may even be
// removed, once `Atom` and `atom_t` can share a memory layout
#[repr(transparent)]
pub struct atom_ref_t {
    atom: RustAtom,
}

impl From<&Atom> for atom_ref_t {
    fn from(atom: &Atom) -> Self {
        Self{ atom: RustAtom::Borrowed((atom as *const Atom).cast()) }
    }
}

impl atom_ref_t {
    pub(crate) fn null() -> Self {
        Self{ atom: RustAtom::None }
    }
    pub(crate) fn is_null(&self) -> bool {
        self.atom.is_null()
    }
    pub(crate) fn borrow(&self) -> &Atom {
        self.atom.borrow()
    }
    pub(crate) fn into_ref(self) -> &'static Atom {
        self.atom.into_ref()
    }
}

pub struct exec_error_t {
    pub error: ExecError,
}

#[repr(C)]
pub struct var_atom_t {
    pub var: *const c_char,
    pub atom: atom_t,
}

pub struct bindings_t {
    pub bindings: Bindings,
}

#[repr(C)]
pub struct bindings_set_t {
    set: *mut RustBindingsSet,
}

// Internal wrapper type so CBindgen doesn't try and export it
struct RustBindingsSet(BindingsSet);

impl From<BindingsSet> for bindings_set_t {
    fn from(set: BindingsSet) -> Self {
        bindings_set_t {
            set: Box::into_raw(Box::new(RustBindingsSet(set)))
        }
    }
}

impl bindings_set_t {
    pub(crate) fn borrow(&self) -> &BindingsSet {
        unsafe{ &(*self.set).0 }
    }
    pub(crate) fn borrow_mut(&mut self) -> &mut BindingsSet {
        unsafe{ &mut (*self.set).0 }
    }
    pub(crate) fn into_inner(self) -> BindingsSet {
        unsafe{*Box::from_raw(self.set)}.0
    }
}

pub type bindings_callback_t = lambda_t<*const bindings_t>;
pub type bindings_mut_callback_t = lambda_t<*mut bindings_t>;

#[repr(C)]
pub struct gnd_api_t {
    // TODO: replace args by C array and ret by callback
    // One can assign NULL to this field, it means the atom is not executable
    execute: Option<extern "C" fn(*const gnd_t, *const vec_atom_t, *mut vec_atom_t) -> *mut exec_error_t>,
    match_: Option<extern "C" fn(*const gnd_t, *const atom_ref_t, bindings_mut_callback_t, *mut c_void)>,
    eq: extern "C" fn(*const gnd_t, *const gnd_t) -> bool,
    clone: extern "C" fn(*const gnd_t) -> *mut gnd_t,
    display: extern "C" fn(*const gnd_t, *mut c_char, usize) -> usize,
    free: extern "C" fn(*mut gnd_t),
}

/// Use this struct as a header the the buffer used to implement a grounded atom
//FUTURE TODO: Asking the user to maintain this header on their allocation is error-prone.  For example
//the user may forget to free the typ field.  I'd like to revisit this API after alpha, and make it
//more opaque - and potentially also offer some small amount of allocation-free storage (like 16 bytes)
//for grounded atom types that are fundamentally small.
#[repr(C)]
pub struct gnd_t {
    api: *const gnd_api_t,
    typ: atom_t,
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
pub extern "C" fn bindings_traverse(cbindings: *const bindings_t, callback: lambda_t<var_atom_t>, context: *mut c_void) {
    let bindings = unsafe{&(*cbindings).bindings};
    bindings.iter().for_each(|(var, atom)|  {
            let name = string_as_cstr(var.name());
            let var_atom = var_atom_t {
                var: name.as_ptr(),
                atom: atom.into()
            };
            callback(var_atom, context);
        }
    )
}

#[no_mangle]
pub extern "C" fn bindings_add_var_binding(bindings: * mut bindings_t, var_atom: var_atom_t) -> bool {
    let bindings = unsafe{ &mut(*bindings).bindings };
    let var = VariableAtom::new(cstr_into_string ( var_atom.var ));
    let atom = var_atom.atom.into_inner();
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

/// Returns the atom bound to the supplied variable in the bindings.  Returns a NULL atom_ref if the
/// variable is not present.
#[no_mangle]
pub extern "C" fn bindings_resolve(bindings: *const bindings_t, var_name: *const c_char) -> atom_t
{
    let bindings = unsafe{&(*bindings).bindings};
    let var = VariableAtom::new(cstr_into_string(var_name));

    bindings.resolve(&var).into()
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
pub extern "C" fn bindings_merge_v2(_self: *mut bindings_t, other: *const bindings_t) -> bindings_set_t
{
    let other = unsafe{ &(*other).bindings };
    let owned_self = ptr_into_bindings(_self);

    let new_set = owned_self.merge_v2(other);
    new_set.into()
}

/// Returns the atom bound to the supplied variable, and then removes the variable-atom pair from the
/// bindings.  Returns a NULL atom_ref if the variable is not present.
#[no_mangle]
pub extern "C" fn bindings_resolve_and_remove(bindings: *mut bindings_t, var_name: *const c_char) -> atom_t {
    let bindings = unsafe{&mut(*bindings).bindings};
    let var = VariableAtom::new(cstr_into_string(var_name));

    bindings.resolve_and_remove(&var).into()
}

#[no_mangle]
pub extern "C" fn bindings_narrow_vars(bindings: *mut bindings_t, vars: *const vec_atom_t) {
    let bindings = unsafe{&mut (*bindings).bindings};
    let vars = unsafe{&*vars}.as_slice();
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
pub extern "C" fn bindings_set_empty() -> bindings_set_t {
    BindingsSet::empty().into()
}

#[no_mangle]
pub extern "C" fn bindings_set_single() -> bindings_set_t {
    BindingsSet::single().into()
}

/// WARNING: This function takes ownership of the bindings argument.
/// After calling this function, the bindings_t passed must not be accessed or freed
///
/// The object returned from this function must be freed with bindings_set_free()
#[no_mangle]
pub extern "C" fn bindings_set_from_bindings(bindings: *mut bindings_t) -> bindings_set_t {
    let owned_bindings = ptr_into_bindings(bindings);
    BindingsSet::from(owned_bindings).into()
}

/// WARNING: This function takes ownership of the bindings argument.
/// After calling this function, the bindings_t passed must not be accessed or freed
#[no_mangle]
pub extern "C" fn bindings_set_push(set: *mut bindings_set_t, bindings: *mut bindings_t) {
    let set = unsafe{ (&mut *set).borrow_mut() };
    let owned_bindings = ptr_into_bindings(bindings);
    set.push(owned_bindings);
}

#[no_mangle]
pub extern "C" fn bindings_set_free(set: bindings_set_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(set.into_inner());
}

#[no_mangle]
pub extern "C" fn bindings_set_clone(set: *const bindings_set_t) -> bindings_set_t {
    let set = unsafe{ (&*set).borrow() };
    set.clone().into()
}

#[no_mangle]
pub extern "C" fn bindings_set_eq(set: *const bindings_set_t, other: *const bindings_set_t) -> bool {
    let set = unsafe{ (&*set).borrow() };
    let other = unsafe{ (&*other).borrow() };
    set == other
}

/// Writes a text description of the bindings_set_t into the provided buffer and returns the number of bytes
/// written, or that would have been written had the buf_len been large enough, excluding the
/// string terminator.
#[no_mangle]
pub extern "C" fn bindings_set_to_str(set: *const bindings_set_t, buf: *mut c_char, buf_len: usize) -> usize {
    let set = unsafe{ (&*set).borrow() };
    write_into_buf(set, buf, buf_len)
}

#[no_mangle]
pub extern "C" fn bindings_set_is_empty(set: *const bindings_set_t) -> bool {
    let set = unsafe{ (&*set).borrow() };
    set.is_empty()
}

#[no_mangle]
pub extern "C" fn bindings_set_is_single(set: *const bindings_set_t) -> bool {
    let set = unsafe{ (&*set).borrow() };
    set.is_single()
}

#[no_mangle]
pub extern "C" fn bindings_set_iterate(set: *mut bindings_set_t, callback: bindings_mut_callback_t, context: *mut c_void) {
    let set = unsafe{ (&mut *set).borrow_mut() };
    for bindings in set.iter_mut() {
        let bindings_ptr = (bindings as *mut Bindings).cast::<bindings_t>();
        callback(bindings_ptr, context);
    }
}

#[no_mangle]
pub extern "C" fn bindings_set_add_var_equality(set: *mut bindings_set_t, a: *const atom_ref_t, b: *const atom_ref_t) {
    let set = unsafe{ (&mut *set).borrow_mut() };
    let a = unsafe{ (&*a).borrow() };
    let b = unsafe{ (&*b).borrow() };

    let mut owned_set = BindingsSet::empty();
    core::mem::swap(&mut owned_set, set);
    let mut result_set = owned_set.add_var_equality(a.try_into().unwrap(), b.try_into().unwrap());
    core::mem::swap(&mut result_set, set);
}

#[no_mangle]
pub extern "C" fn bindings_set_add_var_binding(set: *mut bindings_set_t, var: *const atom_ref_t, value: *const atom_ref_t) {
    let set = unsafe{ (&mut *set).borrow_mut() };
    let var = unsafe{ (&*var).borrow() };
    let value = unsafe{ (&*value).borrow() };

    let mut owned_set = BindingsSet::empty();
    core::mem::swap(&mut owned_set, set);
    let mut result_set = owned_set.add_var_binding(TryInto::<&VariableAtom>::try_into(var).unwrap(), value);
    core::mem::swap(&mut result_set, set);
}

#[no_mangle]
pub extern "C" fn bindings_set_merge_into(_self: *mut bindings_set_t, other: *const bindings_set_t) {
    let _self = unsafe{ (&mut *_self).borrow_mut() };
    let other = unsafe{ (&*other).borrow() };
    let mut owned_self = BindingsSet::empty();
    core::mem::swap(_self, &mut owned_self);

    let mut result_set = owned_self.merge(other);
    core::mem::swap(_self, &mut result_set);
}

// end of bindings_set functions

/// Returns an atom_ref_t that points to the supplied atom.
///
/// WARNING: The returned `atom_ref_t` must not be accessed after the atom it refers to has been freed,
/// or after ownership of the original atom has been transferred to another function
#[no_mangle]
pub unsafe extern "C" fn atom_ref(atom: *const atom_t) -> atom_ref_t {
    let atom = unsafe { (*atom).borrow() };
    atom.into()
}

/// Returns an atom_ref_t that does not point to any atom
#[no_mangle]
pub unsafe extern "C" fn atom_ref_null() -> atom_ref_t {
    atom_ref_t::null()
}

#[no_mangle]
pub unsafe extern "C" fn atom_sym(name: *const c_char) -> atom_t {
    // cstr_as_str() keeps pointer ownership, but Atom::sym() copies resulting
    // String into Atom::Symbol::symbol field.
    Atom::sym(cstr_as_str(name)).into()
}

//TODO BEFORE MERGE for Alpha: Make an interface that can construct an expression directly from an vec_atom_t
#[no_mangle]
pub unsafe extern "C" fn atom_expr(children: *mut atom_t, size: usize) -> atom_t {
    let c_arr = std::slice::from_raw_parts_mut(children, size);
    let children: Vec<Atom> = c_arr.into_iter().map(|atom| {
        core::mem::replace(atom, atom_t::null()).into_inner()
    }).collect();
    Atom::expr(children).into()
}

#[no_mangle]
pub unsafe extern "C" fn atom_var(name: *const c_char) -> atom_t {
    Atom::var(cstr_as_str(name)).into()
}

#[no_mangle]
pub extern "C" fn atom_gnd(gnd: *mut gnd_t) -> atom_t {
    Atom::gnd(CGrounded(AtomicPtr::new(gnd))).into()
}

/// Returns a new grounded `atom_t` referencing the space
/// 
/// This function does not consume the space and the space still must be freed with `space_free`
#[no_mangle]
pub extern "C" fn atom_gnd_for_space(space: *const space_t) -> atom_t {
    let space = unsafe { &(*space).0 };
    Atom::gnd(space.clone()).into()
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_type(atom: *const atom_ref_t) -> atom_type_t {
    match (*atom).borrow() {
        Atom::Symbol(_) => atom_type_t::SYMBOL,
        Atom::Variable(_) => atom_type_t::VARIABLE,
        Atom::Expression(_) => atom_type_t::EXPR,
        Atom::Grounded(_) => atom_type_t::GROUNDED,
    }
}

/// Returns `true` if the referenced atom is invalid, otherwise returns `false`
#[no_mangle]
pub unsafe extern "C" fn atom_is_null(atom: *const atom_ref_t) -> bool {
    (*atom).is_null()
}

/// Writes a text description of the atom into the provided buffer and returns the number of bytes
/// written, or that would have been written had the buf_len been large enough, excluding the
/// string terminator.
#[no_mangle]
pub extern "C" fn atom_to_str(atom: *const atom_ref_t, buf: *mut c_char, buf_len: usize) -> usize {
    let atom = unsafe{ (&*atom).borrow() };
    write_into_buf(atom, buf, buf_len)
}

/// Writes the name of the atom into the provided buffer and returns the number of bytes
/// written, or that would have been written had the buf_len been large enough, excluding the
/// string terminator.
#[no_mangle]
pub extern "C" fn atom_get_name(atom: *const atom_ref_t, buf: *mut c_char, buf_len: usize) -> usize {
    let atom = unsafe{ (&*atom).borrow() };
    match atom {
        Atom::Symbol(s) => write_into_buf(s.name(), buf, buf_len),
        Atom::Variable(v) => write_into_buf(v.name(), buf, buf_len),
        _ => panic!("Only Symbol and Variable has name attribute!"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_object(atom: *const atom_ref_t) -> *mut gnd_t {
    if let Atom::Grounded(ref g) = (&*atom).borrow() {
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
pub unsafe extern "C" fn atom_get_space(atom: *const atom_ref_t) -> *const space_t {
    let atom = (&*atom).borrow();
    if let Some(space) = Atom::as_gnd::<DynSpace>(atom) {
        (space as *const DynSpace).cast()
    } else {
        panic!("Atom does not reference a space");
    }
}

#[no_mangle]
pub extern "C" fn atom_get_grounded_type(atom: *const atom_ref_t) -> atom_t {
    if let Atom::Grounded(ref g) = unsafe{ (&*atom).borrow() } {
        g.type_().into()
    } else {
        panic!("Only Grounded atoms has grounded type attribute!");
    }
}

#[no_mangle]
pub unsafe extern "C" fn atom_get_children(atom: *const atom_ref_t,
        callback: c_atoms_callback_t, context: *mut c_void) {
    if let Atom::Expression(ref e) = (&*atom).borrow() {
        return_atoms(e.children(), callback, context);
    } else {
        panic!("Only Expression atoms have children!");
    }
}

/// Performs a depth-first exhaustive iteration of an atom and all its children recursively.
/// The first result returned will be the atom itself
#[no_mangle]
pub unsafe extern "C" fn atom_iterate(atom: *const atom_ref_t,
        callback: c_atom_callback_t, context: *mut c_void) {
    let atom = (&*atom).borrow();
    for inner_atom in AtomIter::new(atom) {
        callback(inner_atom.into(), context);
    }
}

/// The object returned from this function must be freed with bindings_set_free()
#[no_mangle]
pub extern "C" fn atom_match_atom(a: *const atom_ref_t, b: *const atom_ref_t) -> bindings_set_t {
    let a = unsafe{ (&*a).borrow() };
    let b = unsafe{ (&*b).borrow() };
    let result_set: BindingsSet = crate::atom::matcher::match_atoms(a, b).collect();
    result_set.into()
}

#[no_mangle]
pub unsafe extern "C" fn atom_free(atom: atom_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(atom.into_inner());
}

#[no_mangle]
pub extern "C" fn atom_clone(atom: *const atom_ref_t) -> atom_t {
    unsafe{ &*atom }.borrow().clone().into()
}

#[no_mangle]
pub unsafe extern "C" fn atom_eq(atoma: *const atom_ref_t, atomb: *const atom_ref_t) -> bool {
    (&*atoma).borrow() == (&*atomb).borrow()
}

//TODO BEFORE MERGE, rename vec_atom_t to atom_vec_t

//TODO BEFORE MERGE, add "from C array" method
//TODO BEFORE MERGE, "from va_args" method

/// Represents a list (aka Vec) of Atoms.  It is unsafe to directly access the fields of this struct, so
/// accessors should be used instead
#[repr(C)]
pub struct vec_atom_t {
    ptr: *mut RustOpaqueAtom,
    len: usize,
    capacity: usize,
}

impl vec_atom_t {
    fn as_slice(&self) -> &[Atom] {
        core::borrow::Borrow::borrow(self)
    }
}

impl From<Vec<Atom>> for vec_atom_t {
    fn from(vec: Vec<Atom>) -> Self {
        //When Vec::into_raw_parts is stabilized then use it.  https://github.com/rust-lang/rust/issues/65816
        let mut vec = core::mem::ManuallyDrop::new(vec);
        Self {
            ptr: vec.as_mut_ptr().cast(),
            len: vec.len(),
            capacity: vec.capacity()
        }
    }
}

impl From<vec_atom_t> for Vec<Atom> {
    fn from(vec: vec_atom_t) -> Self {
        unsafe{ Vec::from_raw_parts(vec.ptr.cast(), vec.len, vec.capacity) }
    }
}

impl Drop for vec_atom_t {
    fn drop(&mut self) {
        let vec: Vec<Atom> = unsafe{ Vec::from_raw_parts(self.ptr.cast(), self.len, self.capacity) };
        drop(vec);
    }
}

impl core::borrow::Borrow<[Atom]> for vec_atom_t {
    fn borrow(&self) -> &[Atom] {
        unsafe{ core::slice::from_raw_parts(self.ptr.cast(), self.len) }
    }
}

#[no_mangle]
pub extern "C" fn vec_atom_new() -> vec_atom_t {
    Vec::<Atom>::new().into()
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_free(vec: vec_atom_t) {
    drop(vec);
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_len(vec: *const vec_atom_t) -> usize {
    (*vec).len
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_pop(vec: *mut vec_atom_t) -> atom_t {
    let vec_contents = core::mem::replace(&mut *vec, core::mem::zeroed());
    let mut rust_vec: Vec<Atom> = vec_contents.into();
    let result_atom: atom_t = rust_vec.pop().into();
    *vec = rust_vec.into();
    result_atom
}

#[no_mangle]
pub unsafe extern "C" fn vec_atom_push(vec: *mut vec_atom_t, atom: atom_t) {
    let vec_contents = core::mem::replace(&mut *vec, core::mem::zeroed());
    let mut rust_vec: Vec<Atom> = vec_contents.into();
    rust_vec.push(atom.into_inner());
    *vec = rust_vec.into();
}

/// WARNING: The atom returned from this function remains owned by the vec_atom_t.  It must NOT be
/// accessed after the vec_atom_get has been modified or freed
#[no_mangle]
pub unsafe extern "C" fn vec_atom_get(vec: *const vec_atom_t, idx: usize) -> atom_ref_t {
    let vec = &*vec;
    let atom: &Atom = &vec.as_slice()[idx];
    atom.into()
}

pub type atom_array_t = array_t<atom_ref_t>;
pub type c_atoms_callback_t = lambda_t<atom_array_t>;

pub type c_atom_callback_t = lambda_t<atom_ref_t>;

#[no_mangle]
pub extern "C" fn atoms_are_equivalent(first: *const atom_ref_t, second: *const atom_ref_t) -> bool {
    let first = unsafe{ &*first }.borrow();
    let second = unsafe{ &*second }.borrow();
    crate::atom::matcher::atoms_are_equivalent(first, second)
}

/////////////////////////////////////////////////////////////////
// Code below is a boilerplate code to implement C API correctly.
// It is not a part of C API.

pub fn bindings_into_ptr(bindings: Bindings) -> *mut bindings_t {
    Box::into_raw(Box::new(bindings_t{bindings}))
}

pub fn ptr_into_bindings(bindings: *mut bindings_t) -> Bindings {
    unsafe {Box::from_raw(bindings)}.bindings
}

pub fn return_atoms(atoms: &Vec<Atom>, callback: c_atoms_callback_t, context: *mut c_void) {
    let results: Vec<atom_ref_t> = atoms.iter()
        .map(|atom| atom.into()).collect();
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
        unsafe{ &(*self.get_ptr()).typ }.borrow().clone()
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let mut ret = Vec::new();
        match self.api().execute {
            Some(func) => {
                let error = func(self.get_ptr(), (args as *mut Vec<Atom>).cast::<vec_atom_t>(), //TODO BEFORE MERGE, this will kill us if we leave it like this
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
                let other: atom_ref_t = other.into();
                func(self.get_ptr(), &other,
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
