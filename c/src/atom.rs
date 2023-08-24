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

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Atom Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @brief Represents whether an atom is a Symbol, Variable, Expression, or Grounded atom.
/// @ingroup atom_group
///
#[repr(C)]
pub enum atom_type_t {
    /// @brief The atom is a Symbol atom
    SYMBOL,
    /// @brief The atom is a Variable atom
    VARIABLE,
    /// @brief The atom is an Expression atom
    EXPR,
    /// @brief The atom is a Grounded atom
    GROUNDED,
}

/// @brief Function signature for a callback providing access to an atom
/// @ingroup atom_group
/// @param[in]  atom  A reference to the atom.  This atom should not be modified or freed by the callback.
/// @param[in]  context  The context state pointer initially passed to the upstream function initiating the callback.
///
pub type c_atom_callback_t = extern "C" fn(atom: atom_ref_t, context: *mut c_void);

//Implementation Notes: both `atom_t` and `atom_ref_t` are transparent wrappers around a RustAtom,
// which internally knows whether it owns or borrows the native `Atom` struct.  The reason for this
// design choice is because at allows a pointer to `atom_ref` to be used interchangeably with a
// pointer to `atom_t`
#[repr(C)]
enum RustAtom {
    None,
    Owned(*mut RustOpaqueAtom),
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
            Self::Owned(atom) => unsafe{ &(**atom).0 },
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
            Self::Owned(atom) => unsafe{ Box::from_raw(atom).0 },
            Self::Borrowed(_) => panic!("Can't extract borrowed atom"),
        }
    }
}

/// @struct atom_t
/// @brief Represents an Atom of any type
/// @ingroup atom_group
/// @note `atom_t` must be freed with `atom_free()`, or passed by value
/// to a function that takes ownership of the atom.
///
//NOTE: In the future, we will want this struct to actually mirror a Rust atom's internals, or
// possibly just reexport them.
#[repr(transparent)]
pub struct atom_t {
    atom: RustAtom,
}

impl From<Atom> for atom_t {
    fn from(atom: Atom) -> Self {
        Self{ atom: RustAtom::Owned(Box::into_raw(Box::new(RustOpaqueAtom(atom)))) }
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

/// @struct atom_ref_t
/// @ingroup atom_group
/// @brief Refers to an Atom owned by another object
/// @note It is not necessary to free `atom_ref_t`
/// @note A pointer to `atom_t` can be passed to any function requiring a pointer to `atom_ref_t`.
/// @warning `atom_ref_t` must not be accessed beyond the lifetime of the object which owns the atom
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

/// @brief Create an `atom_ref_t` that points to another atom you own
/// @ingroup atom_group
/// @param[in]  atom  The atom to reference
/// @return an `atom_ref_t` referencing `atom`
/// @warning The returned `atom_ref_t` must not be accessed after the atom it refers to has been freed,
/// or after ownership of the original atom has been transferred to another function
///
/// Returns an `atom_ref_t` that points to the supplied atom.
#[no_mangle]
pub unsafe extern "C" fn atom_ref(atom: *const atom_t) -> atom_ref_t {
    let atom = unsafe { (*atom).borrow() };
    atom.into()
}

/// @brief Create an `atom_ref_t` that points nothing
/// @ingroup atom_group
/// @return an `atom_ref_t` referencing nothing
///
/// Returns an atom_ref_t that does not point to any atom
#[no_mangle]
pub unsafe extern "C" fn atom_ref_null() -> atom_ref_t {
    atom_ref_t::null()
}

/// @brief Create a new Symbol atom with the specified name
/// @ingroup atom_group
/// @param[in]  name  The name of the symbol
/// @return A newly created `atom_t` for the Symbol Atom
/// @note The caller must take ownership responsibility for the returned `atom_t`
///
/// Create a new Symbol atom with the specified name.
#[no_mangle]
pub unsafe extern "C" fn atom_sym(name: *const c_char) -> atom_t {
    // cstr_as_str() keeps pointer ownership, but Atom::sym() copies resulting
    // String into Atom::Symbol::symbol field.
    Atom::sym(cstr_as_str(name)).into()
}

/// @brief Create a new Expression atom with the specified children atoms
/// @ingroup atom_group
/// @param[in]  children  A packed buffer of `atom_t *`, representing the children atoms
/// @param[in]  size  The number of elements in `children`
/// @return An `atom_t` for the Expression atom, containing all 'children' atoms
/// @note The caller must take ownership responsibility for the returned `atom_t`
/// @warning This function takes ownership of all `children` atoms, so they must not be subsequently accessed
///
#[no_mangle]
pub unsafe extern "C" fn atom_expr(children: *mut atom_t, size: usize) -> atom_t {
    let c_arr = std::slice::from_raw_parts_mut(children, size);
    let children: Vec<Atom> = c_arr.into_iter().map(|atom| {
        core::mem::replace(atom, atom_t::null()).into_inner()
    }).collect();
    Atom::expr(children).into()
}

/// @brief Create a new Expression atom with the children contained in an `atom_vec_t`
/// @ingroup atom_group
/// @param[in]  children  An `atom_vec_t` containing all children atoms
/// @return An `atom_t` for the Expression atom, containing all 'children' atoms
/// @note The caller must take ownership responsibility for the returned `atom_t`
/// @warning This function takes ownership of the `children` vec, so it must not be subsequently accessed
///
#[no_mangle]
pub unsafe extern "C" fn atom_expr_from_vec(children: atom_vec_t) -> atom_t {
    let children: Vec<Atom> = children.into_owned().into();
    Atom::expr(children).into()
}

/// @brief Create a new Variable atom with the specified identifier
/// @ingroup atom_group
/// @param[in]  name  The identifier for the newly created Variable atom
/// @return An `atom_t` for the Variable atom
/// @note The caller must take ownership responsibility for the returned `atom_t`
///
#[no_mangle]
pub unsafe extern "C" fn atom_var(name: *const c_char) -> atom_t {
    Atom::var(cstr_as_str(name)).into()
}

/// @ingroup atom_group
/// @param[in]  gnd  A pointer to a buffer to back the new Grounded Atom.  See the documentation of `gnd_t` for more info.
/// @return an `atom_t` for the Grounded atom
/// @note The caller must take ownership responsibility for the returned `atom_t`
///
#[no_mangle]
pub extern "C" fn atom_gnd(gnd: *mut gnd_t) -> atom_t {
    Atom::gnd(CGrounded(AtomicPtr::new(gnd))).into()
}

/// @brief Creates a Grounded Atom referencing a Space
/// @ingroup atom_group
/// @param[in]  space  A pointer to an `space_t` for accessing the space
/// @return an `atom_t` for the Grounded atom
/// @note The caller must take ownership responsibility for the returned `atom_t`
/// @note This function does not consume the space and the space still must be freed with `space_free()`
///
#[no_mangle]
pub extern "C" fn atom_gnd_for_space(space: *const space_t) -> atom_t {
    let space = unsafe { &*space }.borrow();
    Atom::gnd(space.clone()).into()
}

/// @brief Frees an atom and all associated resources
/// @ingroup atom_group
/// @param[in]  atom  The atom to free
///
#[no_mangle]
pub unsafe extern "C" fn atom_free(atom: atom_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(atom.into_inner());
}

/// @brief Makes a "deep copy" of an atom.  Useful to turn an `atom_ref_t` into an `atom_t`
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to clone
/// @return A newly created `atom_t` for the cloned atom
/// @note The caller must take ownership responsibility for the returned `atom_t`
///
#[no_mangle]
pub extern "C" fn atom_clone(atom: *const atom_ref_t) -> atom_t {
    unsafe{ &*atom }.borrow().clone().into()
}

/// @brief Checks if two atom objects represent the same conceptual atom
/// @ingroup atom_group
/// @param[in]  a  A pointer to an `atom_t` or an `atom_ref_t` representing the first atom
/// @param[in]  b  A pointer to an `atom_t` or an `atom_ref_t` representing the second atom
/// @return `true` is the atoms are conceptually identical
///
#[no_mangle]
pub unsafe extern "C" fn atom_eq(a: *const atom_ref_t, b: *const atom_ref_t) -> bool {
    (&*a).borrow() == (&*b).borrow()
}

/// @brief Checks if two atoms are alpha equivalent.
/// @ingroup atom_group
/// @param[in]  a  A pointer to an `atom_t` or an `atom_ref_t` representing the first atom
/// @param[in]  b  A pointer to an `atom_t` or an `atom_ref_t` representing the second atom
/// @return `true` if the atoms can be converted to each other by renaming variables, otherwise `false`
///
#[no_mangle]
pub extern "C" fn atoms_are_equivalent(a: *const atom_ref_t, b: *const atom_ref_t) -> bool {
    let a = unsafe{ &*a }.borrow();
    let b = unsafe{ &*b }.borrow();
    crate::atom::matcher::atoms_are_equivalent(a, b)
}

/// @brief Returns the type of an atom
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to inspect
/// @return An `atom_type_t` indicating the type of `atom`
///
#[no_mangle]
pub unsafe extern "C" fn atom_get_type(atom: *const atom_ref_t) -> atom_type_t {
    match (*atom).borrow() {
        Atom::Symbol(_) => atom_type_t::SYMBOL,
        Atom::Variable(_) => atom_type_t::VARIABLE,
        Atom::Expression(_) => atom_type_t::EXPR,
        Atom::Grounded(_) => atom_type_t::GROUNDED,
    }
}

/// @brief Returns `true` if the referenced atom is invalid, otherwise returns `false`
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to inspect
/// @return `true` if the referenced atom is invalid, otherwise returns `false`
///
#[no_mangle]
pub unsafe extern "C" fn atom_is_null(atom: *const atom_ref_t) -> bool {
    (*atom).is_null()
}

/// @brief Renders a human-readable text description of an atom
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to render
/// @param[out]  buf  A buffer into which the text will be rendered
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the description string, minus the string terminator character.  If
/// `return_value > buf_len + 1`, then the text was not fully rendered and this function should be
/// called again with a larger buffer.
///
#[no_mangle]
pub extern "C" fn atom_to_str(atom: *const atom_ref_t, buf: *mut c_char, buf_len: usize) -> usize {
    let atom = unsafe{ (&*atom).borrow() };
    write_into_buf(atom, buf, buf_len)
}

/// @brief Renders the name of an atom into a text buffer
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to get the name of
/// @param[out]  buf  A buffer into which the text will be written
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the name string, minus the string terminator character.  If
/// `return_value > buf_len + 1`, then the text was not fully written and this function should be
/// called again with a larger buffer.
///
#[no_mangle]
pub extern "C" fn atom_get_name(atom: *const atom_ref_t, buf: *mut c_char, buf_len: usize) -> usize {
    let atom = unsafe{ (&*atom).borrow() };
    match atom {
        Atom::Symbol(s) => write_into_buf(s.name(), buf, buf_len),
        Atom::Variable(v) => write_into_buf(v.name(), buf, buf_len),
        _ => panic!("Only Symbol and Variable has name attribute!"),
    }
}

/// @brief Provides access to all children atoms within an expression atom
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to access
/// @param[in]  callback  A function that will be called to return access to each atom
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
/// @note This function should only be called with Expression atoms
///
#[no_mangle]
pub unsafe extern "C" fn atom_get_children(atom: *const atom_ref_t,
        callback: c_atom_vec_callback_t, context: *mut c_void) {
    if let Atom::Expression(ref e) = (&*atom).borrow() {
        return_atoms(e.children(), callback, context);
    } else {
        panic!("Only Expression atoms have children!");
    }
}

/// @brief Performs a depth-first exhaustive iteration of an atom and all its children recursively
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to iterate
/// @param[in]  callback  A function that will be called to return access to each contained atom
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
/// @note The first result returned will be the atom itself.  Only Expression atoms contain deeper trees to iterate.
///
#[no_mangle]
pub unsafe extern "C" fn atom_iterate(atom: *const atom_ref_t,
        callback: c_atom_callback_t, context: *mut c_void) {
    let atom = (&*atom).borrow();
    for inner_atom in AtomIter::new(atom) {
        callback(inner_atom.into(), context);
    }
}

/// @brief Retrieve the grounded type of a Grounded Atom
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to access
/// @return The `atom_t` expressing the grounded type of `atom`
/// @note Grounded Types are themselves atoms used by the MeTTa type system for customized behavior.
///   A grounded type is different from a generic atom type, expressed by `atom_type_t`.
///   Only Grounded atoms have grounded types.  TODO: Where to LINK to learn more??
/// @note The caller must take ownership responsibility for the returned `atom_t`
///
#[no_mangle]
pub extern "C" fn atom_get_grounded_type(atom: *const atom_ref_t) -> atom_t {
    if let Atom::Grounded(ref g) = unsafe{ (&*atom).borrow() } {
        g.type_().into()
    } else {
        panic!("Only Grounded atoms has grounded type attribute!");
    }
}

/// @brief Access a pointer to an object backing a Grounded Atom
/// @ingroup atom_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to access
/// @return The pointer to the `gnd_t` orginally used to create the Grounded Atom
/// @note This function is only valid for Grounded Atoms implemented via the HyperonC API
/// @warning The returned pointer must not be accessed after the atom has been freed or modified elsewhere
///
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

/// @brief Access the space wrapped inside a Grounded atom
/// @ingroup atom_group
/// @see atom_gnd_for_space
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` that wraps a Space
/// @return A Space handle to the space inside a grounded atom
/// @note The returned space is borrowed from the atom.  It must not be accessed after the atom has been freed or modified elsewhere
///
#[no_mangle]
pub unsafe extern "C" fn atom_get_space(atom: *const atom_ref_t) -> space_t {
    let atom = (&*atom).borrow();
    if let Some(space) = Atom::as_gnd::<DynSpace>(atom) {
        space.clone().into()
    } else {
        panic!("Atom does not reference a space");
    }
}

/// Private convenience function to call an `c_atom_vec_callback_t` callback with each atom in a vec
pub(crate) fn return_atoms(atoms: &Vec<Atom>, callback: c_atom_vec_callback_t, context: *mut c_void) {
    callback(&(&atoms[..]).into(), context);
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Grounded Atom Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @struct gnd_api_t
/// @brief A table of callback functions to implement a Grounded Atom with behavior defined in C
/// @ingroup grounded_atom_group
/// @see atom_gnd
/// @see atom_get_object
///
#[repr(C)]
pub struct gnd_api_t {
    /// @brief An optional function to implement executable atom behavior
    /// @param[in]  gnd  A pointer to the Grounded Atom object
    /// @param[in]  args  A pointer to an `atom_vec_t` containing the argument atoms for this execution
    /// @param[out]  out  A pointer to a mutable `atom_vec_t`, into which result atoms may be added by the execution
    /// @return An `exec_error_t` status that informs the MeTTa interpreter if execution may continue or whether to handle a fault
    /// @note Assigning NULL to this field means the atom is not executable
    ///
    execute: Option<extern "C" fn(gnd: *const gnd_t, args: *const atom_vec_t, out: *mut atom_vec_t) -> exec_error_t>,

    /// @brief An optional function to match the atom with another atom
    /// @param[in]  gnd  A pointer to the Grounded Atom object
    /// @param[in]  other  The other atom to match with
    /// @return  A `bindings_set_t` that contains all matches between the Grounded Atom and the `other` atom.  Returning an empty
    /// Bindings Set means the atom do not match
    /// @note Assigning NULL to this field means the atom will match only other atoms which are considered equal by the `eq` function below
    ///
    match_: Option<extern "C" fn(gnd: *const gnd_t, other: *const atom_ref_t) -> bindings_set_t>,

    /// @brief Tests whether two atoms instantiated from the same interface are equal
    /// @param[in]  gnd  A pointer to the Grounded Atom object
    /// @param[in]  other  A pointer to the other grounded atom's object
    /// @return  `true` if the two atoms are equal, and `false` if they are not
    ///
    eq: extern "C" fn(gnd: *const gnd_t, other: *const gnd_t) -> bool,

    /// @brief Makes a deep copy of a Grounded Atom
    /// @param[in]  gnd  A pointer to the Grounded Atom object
    /// @return  A pointer to a newly allocated buffer to back a new Grounded Atom, which has been initialized as a clone of the atom backed by the incoming `gnd` argument.
    /// @see atom_gnd
    ///
    clone: extern "C" fn(gnd: *const gnd_t) -> *mut gnd_t,

    /// @brief Renders a human-readable text description of the Grounded Atom into a buffer
    /// @param[in]  gnd  A pointer to the Grounded Atom object
    /// @param[out]  buf  A buffer into which the text should be rendered
    /// @param[in]  buf_len  The maximum allocated size of `buf`
    /// @return The length of the fully rendered text description, minus the string terminator character.  Regardless of whether the text was able to be completely rendered into the buffer.
    /// @see atom_to_str
    /// @warning This function implementation must write a null terminator character at the end of the text, and must never overwrite the `buf_len` parameter.
    ///
    display: extern "C" fn(gnd: *const gnd_t, buf: *mut c_char, buf_len: usize) -> usize,

    /// @brief Frees the backing object belonging to a Grounded Atom, and all associated resources
    /// @param[in]  gnd  A pointer to the Grounded Atom object
    /// @note This function must free the `typ` field in the `gnd_t` header
    ///
    free: extern "C" fn(gnd: *mut gnd_t),
}

/// @struct gnd_t
/// @brief A struct header that must preface a buffer used as a backing object for a Grounded Atom
/// @ingroup grounded_atom_group
/// @see atom_gnd
/// @see atom_get_object
///
//FUTURE TODO: Asking the user to maintain this header on their allocation is error-prone.  For example
//the user may forget to free the typ field.  I'd like to revisit this API after alpha, and make it
//more opaque - and potentially also offer some small amount of allocation-free storage (like 16 bytes)
//for grounded atom types that are fundamentally small.
#[repr(C)]
pub struct gnd_t {
    /// @brief A pointer to the table of functions that implement the Grounded Atom's behavior
    api: *const gnd_api_t,
    /// @brief An atom representing the grounded type of the Grounded Atom
    /// @see atom_get_grounded_type
    typ: atom_t,
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
        unsafe { &*(*self.get_ptr()).api }
    }

    fn free(&mut self) {
        (self.api().free)(self.get_mut_ptr());
    }

}

impl Grounded for CGrounded {
    fn type_(&self) -> Atom {
        unsafe{ &(*self.get_ptr()).typ }.borrow().clone()
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        match self.api().execute {
            Some(func) => {
                let mut ret = atom_vec_t::new();
                let c_args: atom_vec_t = args.into();
                let error = func(self.get_ptr(), &c_args, &mut ret);
                let ret = if error.is_no_err() {
                    Ok(ret.into())
                } else {
                    let error = error.into_inner();
                    Err(error)
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
                let other: atom_ref_t = other.into();
                let set = func(self.get_ptr(), &other);
                Box::new(set.into_inner().into_iter())
            },
            None => match_by_equality(self, other)
        }
    }
}

impl PartialEq for CGrounded {
    fn eq(&self, other: &CGrounded) -> bool {
        let self_api_ptr = unsafe{ (&*self.get_ptr()).api };
        let other_api_ptr = unsafe{ (&*other.get_ptr()).api };
        if self_api_ptr == other_api_ptr {
            (self.api().eq)(self.get_ptr(), other.get_ptr())
        } else {
            false
        }
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

pub struct RustOpaqueExecError(ExecError);

/// @struct exec_error_t
/// @brief Represents a status used to communicate between Grounded Atom execution and the MeTTa interpreter
/// @ingroup grounded_atom_group
/// @note `exec_error_t` must be freed with `exec_error_free()`, or returned from an `execute` function
/// @note The struct members should never be accessed directly, so accessor functions must be used
///
#[repr(C)]
pub enum exec_error_t {
    NoErr,
    Status(*mut RustOpaqueExecError)
}

impl From<ExecError> for exec_error_t {
    fn from(error: ExecError) -> Self {
        exec_error_t::Status( Box::into_raw(Box::new(RustOpaqueExecError(error))) )
    }
}

impl exec_error_t {
    pub(crate) fn is_no_err(&self) -> bool {
        match self {
            Self::NoErr => true,
            Self::Status(_) => false,
        }
    }
    pub(crate) fn into_inner(self) -> ExecError {
        match self {
            Self::NoErr => panic!(),  //Illegal to access exec_error_t with NoErr status
            Self::Status(box_ptr) => unsafe{ Box::from_raw(box_ptr).0 }
        }
    }
}

/// @brief Creates a new `exec_error_t` representing a runtime error that will halt the MeTTa interpreter
/// @ingroup grounded_atom_group
/// @param[in]  message  A human-readable error message, for the interpreter to propagate to the user
/// @return The newly created `exec_error_t`
/// @note The caller must take ownership responsibility for the returned `exec_error_t`, and ultimately free
///   it with `exec_error_free()` or return it from an `execute` function
///
#[no_mangle]
pub extern "C" fn exec_error_runtime(message: *const c_char) -> exec_error_t {
    let error = ExecError::Runtime(cstr_into_string(message));
    error.into()
}

/// @brief Creates a new `exec_error_t` representing a "Don't Reduce" status, telling the MeTTa interpreter to process the atoms as they are
/// @ingroup grounded_atom_group
/// @return The newly created `exec_error_t`
/// @note The caller must take ownership responsibility for the returned `exec_error_t`, and ultimately free
///   it with `exec_error_free()` or return it from an `execute` function
///
#[no_mangle]
pub extern "C" fn exec_error_no_reduce() -> exec_error_t {
    ExecError::NoReduce.into()
}

/// @brief Creates a new `exec_error_t` representing a "No Error" status.  This is the default interpreter status
/// @ingroup grounded_atom_group
/// @return The newly created `exec_error_t`
/// @note The caller must take ownership responsibility for the returned `exec_error_t`, and ultimately free
///   it with `exec_error_free()` or return it from an `execute` function
///
#[no_mangle]
pub extern "C" fn exec_error_no_err() -> exec_error_t {
    exec_error_t::NoErr
}

/// @brief Frees an `exec_error_t`
/// @ingroup grounded_atom_group
/// @param[in]  error  The `exec_error_t` to free
///
#[no_mangle]
pub extern "C" fn exec_error_free(error: exec_error_t) {
    if !error.is_no_err() {
        let error = error.into_inner();
        drop(error);
    }
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Atom Vec Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @struct atom_vec_t
/// @brief Contains a vector (list) of Atoms
/// @ingroup atom_vec_group
/// @note `atom_vec_t` must be freed with `atom_vec_free()`, or passed by value
/// to a function that takes ownership of the vec.
/// @warning It is unsafe to directly access the fields of this struct, so accessor functions must be used
#[repr(C)]
pub struct atom_vec_t {
    /// Internal.  Should not be accessed directly
    ptr: *mut RustOpaqueAtom,
    /// Internal.  Should not be accessed directly
    len: usize,
    /// Internal.  Should not be accessed directly
    capacity: usize,
    /// Internal.  Should not be accessed directly
    owned: bool,
}

impl atom_vec_t {
    fn new() -> Self {
        Vec::<Atom>::new().into()
    }
    fn as_slice(&self) -> &[Atom] {
        unsafe{ core::slice::from_raw_parts(self.ptr.cast(), self.len) }
    }
    /// Converts a borrowed vec into an owned vec
    fn into_owned(self) -> Self {
        match self.owned {
            true => self,
            false => {
                self.as_slice().to_vec().into()
            }
        }
    }
}

impl From<Vec<Atom>> for atom_vec_t {
    fn from(vec: Vec<Atom>) -> Self {
        //When Vec::into_raw_parts is stabilized then use it.  https://github.com/rust-lang/rust/issues/65816
        let mut vec = core::mem::ManuallyDrop::new(vec);
        Self {
            ptr: vec.as_mut_ptr().cast(),
            len: vec.len(),
            capacity: vec.capacity(),
            owned: true,
        }
    }
}

impl From<&[Atom]> for atom_vec_t {
    fn from(slice: &[Atom]) -> Self {
        Self {
            ptr: slice.as_ptr().cast_mut().cast(),
            len: slice.len(),
            capacity: slice.len(),
            owned: false,
        }
    }
}

impl From<atom_vec_t> for Vec<Atom> {
    fn from(vec: atom_vec_t) -> Self {
        if !vec.owned {
            panic!("Error! Attempt to take ownership of borrowed atom_vec_t");
        }
        let rust_vec = unsafe{ Vec::from_raw_parts(vec.ptr.cast(), vec.len, vec.capacity) };
        core::mem::forget(vec);
        rust_vec
    }
}

impl Drop for atom_vec_t {
    fn drop(&mut self) {
        if self.owned {
            let vec: Vec<Atom> = unsafe{ Vec::from_raw_parts(self.ptr.cast(), self.len, self.capacity) };
            drop(vec);
        }
    }
}

impl Clone for atom_vec_t {
    fn clone(&self) -> Self {
        self.as_slice().to_vec().into()
    }
}

/// @brief Function signature for a callback providing access to an `atom_vec_t`
/// @ingroup atom_vec_group
/// @param[in]  vec  The `atom_vec_t` being provided.  This vec should not be modified or freed by the callback.
/// @param[in]  context  The context state pointer initially passed to the upstream function initiating the callback.
///
pub type c_atom_vec_callback_t = extern "C" fn(vec: *const atom_vec_t, context: *mut c_void);

/// @brief Creates a new empty `atom_vec_t`
/// @ingroup atom_vec_group
/// @return The newly created `atom_vec_t`
/// @note The caller must take ownership responsibility for the returned `atom_vec_t`
///
#[no_mangle]
pub extern "C" fn atom_vec_new() -> atom_vec_t {
    atom_vec_t::new()
}

/// @brief Creates a new `atom_vec_t` by cloning an existing `atom_vec_t`
/// @ingroup atom_vec_group
/// @param[in]  vec  A pointer to an existing `atom_vec_t` to clone
/// @return A newly created `atom_vec_t`
/// @note The caller must take ownership responsibility for the returned `atom_vec_t`
///
#[no_mangle]
pub extern "C" fn atom_vec_clone(vec: *const atom_vec_t) -> atom_vec_t {
    let vec = unsafe{ &*vec };
    vec.clone()
}

/// @brief Creates a new `atom_vec_t` from a C-style array
/// @ingroup atom_vec_group
/// @param[in]  atoms  A packed buffer of `atom_t *`, representing the atoms to put into the vec.
/// @param[in]  size  The number of elements in `atoms`
/// @return The newly created `atom_vec_t`
/// @note The caller must take ownership responsibility for the returned `atom_vec_t`
/// @warning This function takes ownership of all `atoms`, so they must not be subsequently accessed or freed
///
#[no_mangle]
pub unsafe extern "C" fn atom_vec_from_array(atoms: *mut atom_t, size: usize) -> atom_vec_t {
    let c_arr = std::slice::from_raw_parts_mut(atoms, size);
    let atoms: Vec<Atom> = c_arr.into_iter().map(|atom| {
        core::mem::replace(atom, atom_t::null()).into_inner()
    }).collect();
    atoms.into()
}

/// @brief Frees a `atom_vec_t`
/// @ingroup atom_vec_group
/// @param[in]  vec  The vec to free
///
#[no_mangle]
pub unsafe extern "C" fn atom_vec_free(vec: atom_vec_t) {
    drop(vec);
}

/// @brief Returns the number of elements in a vec
/// @ingroup atom_vec_group
/// @param[in]  vec  The vec to be inspected
/// @return The count of the number of elements contained within the vec
///
#[no_mangle]
pub unsafe extern "C" fn atom_vec_len(vec: *const atom_vec_t) -> usize {
    (*vec).len
}

/// @brief Removes the last element from a vec, and returns it
/// @ingroup atom_vec_group
/// @param[in]  vec  The vec from which to pop the atom
/// @return The last `atom_t` contained in the vec
/// @note The caller must take ownership responsibility for the returned `atom_t`
/// @note In the event that the vec is empty, a `null` `atom_t` will be returned
///
#[no_mangle]
pub unsafe extern "C" fn atom_vec_pop(vec: *mut atom_vec_t) -> atom_t {
    let vec_contents = core::mem::replace(&mut *vec, core::mem::zeroed());
    if !vec_contents.owned {
        panic!("Error! Attempt to modify read-only atom_vec_t");
    }
    let mut rust_vec: Vec<Atom> = vec_contents.into();
    let result_atom: atom_t = rust_vec.pop().into();
    *vec = rust_vec.into();
    result_atom
}

/// @brief Push the atom onto the end of the vec
/// @ingroup atom_vec_group
/// @param[in]  vec  a pointer to an `atom_vec_t` to push the atom onto
/// @param[in]  atom  the atom to push onto the vec
/// @warning This function takes ownership of the supplied `atom_t`, and it must not be subsequently accessed of freed.
///
#[no_mangle]
pub unsafe extern "C" fn atom_vec_push(vec: *mut atom_vec_t, atom: atom_t) {
    let vec_contents = core::mem::replace(&mut *vec, core::mem::zeroed());
    if !vec_contents.owned {
        panic!("Error! Attempt to modify read-only atom_vec_t");
    }
    let mut rust_vec: Vec<Atom> = vec_contents.into();
    rust_vec.push(atom.into_inner());
    *vec = rust_vec.into();
}

/// @brief Access an atom at a specified index in a vec
/// @ingroup atom_vec_group
/// @param[in]  vec  The vec in which to access the atom
/// @param[in]  idx  The index of the element to access
/// @return A reference to the atom at the specified index
/// @note In the event that no atom exists at the specified `idx`, a `null` `atom_ref_t` will be returned
/// @warning The atom referenced by the return value from this function remains owned by the `atom_vec_t`.
/// It must not be accessed after the vec has been modified or freed.
///
#[no_mangle]
pub unsafe extern "C" fn atom_vec_get(vec: *const atom_vec_t, idx: usize) -> atom_ref_t {
    let vec = &*vec;
    let atom: &Atom = &vec.as_slice()[idx];
    atom.into()
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Matching and Binding Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @brief Matches one atom with another, establishing bindings between them
/// @ingroup matching_group
/// @param[in]  a  A pointer to an `atom_t` or an `atom_ref_t` to match
/// @param[in]  b  A pointer to another `atom_t` or an `atom_ref_t` to match against
/// @return  A `bindings_set_t` representing all variable <-> atom bindings established by the match
/// @note The caller must take ownership responsibility for the returned `bindings_set_t`, and free it with `bindings_set_free()`
///
#[no_mangle]
pub extern "C" fn atom_match_atom(a: *const atom_ref_t, b: *const atom_ref_t) -> bindings_set_t {
    let a = unsafe{ (&*a).borrow() };
    let b = unsafe{ (&*b).borrow() };
    let result_set: BindingsSet = crate::atom::matcher::match_atoms(a, b).collect();
    result_set.into()
}

/// @brief Represents a single Bindings frame, which is a group of mutually-compatible variable <-> atom associations, providing a scope in which variable have definde values
/// @ingroup matching_group
///
#[repr(C)]
pub struct bindings_t {
    /// Internal.  Should not be accessed directly
    bindings: *mut RustBindings,
}

// Internal wrapper type so CBindgen doesn't try and export Bindings
struct RustBindings(Bindings);

impl From<Bindings> for bindings_t {
    fn from(bindings: Bindings) -> Self {
        Self {
            bindings: Box::into_raw(Box::new(RustBindings(bindings)))
        }
    }
}

impl bindings_t {
    pub(crate) fn borrow(&self) -> &Bindings {
        unsafe{ &(*self.bindings).0 }
    }
    pub(crate) fn borrow_mut(&mut self) -> &mut Bindings {
        unsafe{ &mut (*self.bindings).0 }
    }
    pub(crate) fn into_inner(self) -> Bindings {
        unsafe{*Box::from_raw(self.bindings)}.0
    }
}

/// @brief Represents a set of Bindings frames.  Potentially expressing all possible matches produced by a match operarion.
/// @ingroup matching_group
///
#[repr(C)]
pub struct bindings_set_t {
    /// Internal.  Should not be accessed directly
    set: *mut RustBindingsSet,
}

// Internal wrapper type so CBindgen doesn't try and export BindingsSet
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

/// @brief Function signature for a callback providing mutable access to individual Bindings frames within a Bindings Set
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t`.  It is ok to call functions that modify the `bindings_t` within the callback
/// @param[in]  context  The context state pointer initially passed to the upstream function initiating the callback
///
pub type bindings_mut_callback_t = extern "C" fn(bindings: *mut bindings_t, context: *mut c_void);

/// @brief Function signature for a callback providing access to a variable <-> atom pair, associated with a binding
/// @ingroup matching_group
/// @param[in]  var  A reference to the Variable atom.  This atom should not be modified or freed by the callback
/// @param[in]  value  A reference to the other atom associated with the variable.  This atom should not be modified or freed by the callback
/// @param[in]  context  The context state pointer initially passed to the upstream function initiating the callback
///
pub type c_var_binding_callback_t = extern "C" fn(var: atom_ref_t, value: atom_ref_t, context: *mut c_void);

/// @brief Creates a new `bindings_t` containing no variable <-> atom associations, leaving all variables free to match any atom.
/// @ingroup matching_group
/// @return  The new `bindings_t`
/// @note The caller must take ownership responsibility for the returned `bindings_t`, and free it with `bindings_free()`
///
#[no_mangle]
pub extern "C" fn bindings_new() -> bindings_t {
    Bindings::new().into()
}

/// @brief Frees a `bindings_t`
/// @ingroup matching_group
/// @param[in]  bindings  The `bindings_t` to free
///
#[no_mangle]
pub extern "C" fn bindings_free(bindings: bindings_t) {
    let bindings = bindings.into_inner();
    drop(bindings); //Drop happens automatically, but we're explicit here for clarity
}

/// @brief Makes a "deep copy" of a `bindings_t`
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t` to clone
/// @return A newly created `bindings_t`
/// @note The caller must take ownership responsibility for the returned `bindings_t`
///
#[no_mangle]
pub extern "C" fn bindings_clone(bindings: *const bindings_t) -> bindings_t {
    let bindings = unsafe{ &*bindings };
    bindings.borrow().clone().into()
}

/// @brief Renders a text description of a `bindings_t`
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t` to render
/// @param[out]  buf  A buffer into which the text will be rendered
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the description string, minus the string terminator character.  If
/// `return_value > buf_len + 1`, then the text was not fully rendered and this function should be
/// called again with a larger buffer.
///
#[no_mangle]
pub extern "C" fn bindings_to_str(bindings: *const bindings_t, buf: *mut c_char, buf_len: usize) -> usize {
    let bindings = unsafe{ &*bindings };
    write_into_buf(bindings.borrow(), buf, buf_len)
}

/// @brief Checks if two `bindings_t` objects contain identical associations
/// @ingroup matching_group
/// @param[in]  a  A pointer to a `bindings_t` representing the first Bindings frame
/// @param[in]  b  A pointer to a `bindings_t` representing the second Bindings frame
/// @return `true` is the Bindings frames are identical, and `false` otherwise
///
#[no_mangle]
pub extern "C" fn bindings_eq(a: *const bindings_t, b: *const bindings_t) -> bool {
    let left = unsafe{ &*a };
    let right = unsafe{ &*b };
    left.borrow() == right.borrow()
}

/// @brief Iterates each variable <-> Atom association within a `bindings_t`
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t` to iterate
/// @param[in]  callback  A function that will be called for each variable <-> atom pair within the Bindings frame
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
///
#[no_mangle]
pub extern "C" fn bindings_traverse(bindings: *const bindings_t, callback: c_var_binding_callback_t, context: *mut c_void) {
    let bindings = unsafe{ &*bindings };

    //EXPLANATION: The reason we do all the arg conversion in one loop, and then call the callback
    // in a second loop is because the C API is fundamentally being given atom_refs, which the
    // callback is not expected to free.  This makes sense for the API long-term.  But in
    // the short term it creates a problem because atom_ref_t has a static lifetime but points
    // to outside memory.  Using two loops and a temporary buffer ensures the temporary atoms aren't
    // freed until the callbacks have finished.  When the Rust API is improved, this code should be
    // simplified.
    //
    //TODO: We ought to rework the Rust Bindings API so Atoms in Bindings don't need to be copied and
    // returned by value.  Probably best to save this work until we are implementing comprehensions,
    // as part of non-determinism / inference control
    let callback_args: Vec<(Atom, Atom)> = bindings.borrow().iter().map(|(var, atom)| {
        (Atom::Variable(var.clone()), atom)
    }).collect();
    callback_args.iter().for_each(|(var, atom)| callback(var.into(), atom.into(), context));
}

/// @brief Adds a new variable <-> atom association within a `bindings_t`
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t` to add the association to
/// @param[in]  var  The Variable atom (L-value) for the variable <-> atom association
/// @param[in]  atom  The other atom (R-value) for the variable <-> atom association
/// @return `true` if the `bindings_t` has been sucessfully updated, and `false` if the new association would conflict with existing associations in the Bindings frame
/// @warning This function takes ownership of both the `var` and `atom` atoms, so they must not be subsequently accessed or freed
///
#[no_mangle]
pub extern "C" fn bindings_add_var_binding(bindings: *mut bindings_t, var: atom_t, atom: atom_t) -> bool {
    let bindings = unsafe{ &mut*bindings }.borrow_mut();
    let var = match var.into_inner() {
        Atom::Variable(variable) => variable,
        _ => panic!("var argument must be variable atom")
    };
    let atom = atom.into_inner();
    match bindings.clone().add_var_binding_v2(var, atom) {
        Ok(new_bindings) => {
            *bindings = new_bindings;
            true
        },
        Err(_) => false
    }
}

/// @brief Checks if a `bindings_t` contains no associations
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t` to inspect
/// @return `true` is the Bindings frame contains no associations, and `false` otherwise
///
#[no_mangle]
pub extern "C" fn bindings_is_empty(bindings: *const bindings_t) -> bool{
    let bindings = unsafe{ &*bindings }.borrow();
    bindings.is_empty()
}

/// @brief Returns the atom bound to the supplied variable name in the `bindings_t`
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t` to inspect
/// @param[in]  var_name  A NULL-terminated C-style string containing the name of the variable
/// @return The `atom_t` representing the atom that corresponds to the specified variable, or a NULL `atom_ref_t` if the variable is not present.
/// @note The caller must take ownership responsibility for the returned `atom_t`, if it is not NULL
///
#[no_mangle]
pub extern "C" fn bindings_resolve(bindings: *const bindings_t, var_name: *const c_char) -> atom_t
{
    let bindings = unsafe{ &*bindings }.borrow();
    let var = VariableAtom::new(cstr_into_string(var_name));

    bindings.resolve(&var).into()
}

/// @brief Returns the atom bound to the supplied variable name in the `bindings_t`, and removes it from the `bindings_t`
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t` to inspect
/// @param[in]  var_name  A NULL-terminated C-style string containing the name of the variable
/// @return The `atom_t` representing the atom that corresponds to the specified variable, or a NULL `atom_ref_t` if the variable is not present.
/// @note The caller must take ownership responsibility for the returned `atom_t`, if it is not NULL
///
#[no_mangle]
pub extern "C" fn bindings_resolve_and_remove(bindings: *mut bindings_t, var_name: *const c_char) -> atom_t {
    let bindings = unsafe{ &mut*bindings }.borrow_mut();
    let var = VariableAtom::new(cstr_into_string(var_name));

    bindings.resolve_and_remove(&var).into()
}

/// @brief Merges two `bindings_t` Bindings frames together into a Bindings Set
/// @ingroup matching_group
/// @param[in]  _self  The first `bindings_t` to merge.  Ownership of this argument is taken by this function
/// @param[in]  other  A pointer to the second `bindings_t` to merge.  This argument is not modified
/// @return The `bindings_set_t` that results from the merge
/// @warning This function takes ownership of the `_self` argument.  After calling this function, the bindings_t passed as _self must not be accessed or freed
/// @note The `bindings_set_t` returned from this function must be freed with `bindings_set_free()`
///
#[no_mangle]
pub extern "C" fn bindings_merge(_self: bindings_t, other: *const bindings_t) -> bindings_set_t
{
    let other = unsafe{ &*other }.borrow();
    let owned_self = _self.into_inner();

    let new_set = owned_self.merge_v2(other);
    new_set.into()
}

/// @brief Removes all variable associations from a `bindings_t` except those in the supplied list
/// @ingroup matching_group
/// @param[in]  bindings  A pointer to the `bindings_t` to modify
/// @param[in]  vars  A pointer to a vec of Variable atoms, to specify which variables should remain in the Bindings
///
#[no_mangle]
pub extern "C" fn bindings_narrow_vars(bindings: *mut bindings_t, vars: *const atom_vec_t) {
    let bindings = unsafe{ &mut*bindings}.borrow_mut();
    let vars = unsafe{&*vars}.as_slice();
    let vars_iter = vars.into_iter().map(|atom| {
        TryInto::<&VariableAtom>::try_into(atom)
            .expect("Only variable atoms allowed for bindings_narrow_vars")
    });
    let vars_set = HashSet::from_iter(vars_iter);

    let mut new_bindings = bindings.narrow_vars(&vars_set);
    core::mem::swap(&mut new_bindings, bindings);
}

/// @brief Creates a new `bindings_set_t` without any Bindings frames.  Conceptually this means no valid matches exist
/// @ingroup matching_group
/// @return The empty `bindings_set_t`
/// @note The `bindings_set_t` returned from this function must be freed with `bindings_set_free()`
///
#[no_mangle]
pub extern "C" fn bindings_set_empty() -> bindings_set_t {
    BindingsSet::empty().into()
}

/// @brief Creates a new `bindings_set_t` with one new Bindings frame.  Conceptually this means all variables are able to take on any value
/// @ingroup matching_group
/// @return The new `bindings_set_t`
/// @note The `bindings_set_t` returned from this function must be freed with `bindings_set_free()`
///
#[no_mangle]
pub extern "C" fn bindings_set_single() -> bindings_set_t {
    BindingsSet::single().into()
}

/// @brief Creates a new `bindings_set_t` with the specified Bindings frame
/// @ingroup matching_group
/// @param[in]  bindings  The `bindings_t` to incorporate into the new Bindings Set.  Ownership of this argument is taken by this function
/// @return The new `bindings_set_t`
/// @warning This function takes ownership of the `bindings` argument, and it must not be subsequently accessed or freed after calling this function
/// @note The `bindings_set_t` returned from this function must be freed with `bindings_set_free()`
///
#[no_mangle]
pub extern "C" fn bindings_set_from_bindings(bindings: bindings_t) -> bindings_set_t {
    BindingsSet::from(bindings.into_inner()).into()
}

/// @brief Adds a Bindings frame to an existing `bindings_set_t`
/// @ingroup matching_group
/// @param[in]  set  The `bindings_set_t` to add `bindings` into
/// @param[in]  bindings  The `bindings_t` to incorporate into `set`.  Ownership of this argument is taken by this function
/// @warning This function takes ownership of the `bindings` argument, and it must not be subsequently accessed or freed after calling this function
///
#[no_mangle]
pub extern "C" fn bindings_set_push(set: *mut bindings_set_t, bindings: bindings_t) {
    let set = unsafe{ (&mut *set).borrow_mut() };
    set.push(bindings.into_inner());
}

/// @brief Frees a `bindings_set_t`
/// @ingroup matching_group
/// @param[in]  set  The `bindings_set_t` to free
///
#[no_mangle]
pub extern "C" fn bindings_set_free(set: bindings_set_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(set.into_inner());
}

/// @brief Makes a "deep copy" of a `bindings_set_t`
/// @ingroup matching_group
/// @param[in]  set  A pointer to the `bindings_set_t` to use as the source for the clone
/// @return The new cloned `bindings_set_t`
/// @note The `bindings_set_t` returned from this function must be freed with `bindings_set_free()`
///
#[no_mangle]
pub extern "C" fn bindings_set_clone(set: *const bindings_set_t) -> bindings_set_t {
    let set = unsafe{ (&*set).borrow() };
    set.clone().into()
}

/// @brief Checks if two `bindings_set_t` objects contain identical associations
/// @ingroup matching_group
/// @param[in]  a  A pointer to a `bindings_set_t` representing the first Bindings Set
/// @param[in]  b  A pointer to a `bindings_set_t` representing the second Bindings Set
/// @return `true` is the Bindings Sets are identical, and `false` otherwise
///
#[no_mangle]
pub extern "C" fn bindings_set_eq(a: *const bindings_set_t, b: *const bindings_set_t) -> bool {
    let a = unsafe{ (&*a).borrow() };
    let b = unsafe{ (&*b).borrow() };
    a == b
}

/// @brief Renders a text description of a `bindings_set_t`
/// @ingroup matching_group
/// @param[in]  set  A pointer to the `bindings_set_t` to render
/// @param[out]  buf  A buffer into which the text will be rendered
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the description string, minus the string terminator character.  If
/// `return_value > buf_len + 1`, then the text was not fully rendered and this function should be
/// called again with a larger buffer.
///
#[no_mangle]
pub extern "C" fn bindings_set_to_str(set: *const bindings_set_t, buf: *mut c_char, buf_len: usize) -> usize {
    let set = unsafe{ (&*set).borrow() };
    write_into_buf(set, buf, buf_len)
}

/// @brief Checks if a `bindings_set_t` contains no Bindings frames, and thus indicates no match
/// @ingroup matching_group
/// @param[in]  set  A pointer to the `bindings_set_t` to inspect
/// @return `true` is the Bindings set is empty, and `false` otherwise
///
#[no_mangle]
pub extern "C" fn bindings_set_is_empty(set: *const bindings_set_t) -> bool {
    let set = unsafe{ (&*set).borrow() };
    set.is_empty()
}

/// @brief Checks if a `bindings_set_t` contains a frame with no associations, and is thus allows variables to take on any value
/// @ingroup matching_group
/// @param[in]  set  A pointer to the `bindings_set_t` to inspect
/// @return `true` is the Bindings set is "single", and `false` otherwise
/// @note Perhaps the word "single" is not a good description of its meaning.  See https://github.com/trueagi-io/hyperon-experimental/issues/281
///
#[no_mangle]
pub extern "C" fn bindings_set_is_single(set: *const bindings_set_t) -> bool {
    let set = unsafe{ (&*set).borrow() };
    set.is_single()
}

/// @brief Provides sequential access to each Bindings frame within a Bindings set.
/// @ingroup matching_group
/// @param[in]  set  A pointer to the `bindings_set_t` to iterate
/// @param[in]  callback  A function that will be called to provide access to each Bindings frame
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
/// @note The current implementation of this function allows Bindings frames to be modified by the callback function, however this may not be possible in future versions of Hyperon
///
#[no_mangle]
pub extern "C" fn bindings_set_iterate(set: *mut bindings_set_t, callback: bindings_mut_callback_t, context: *mut c_void) {
    let set = unsafe{ (&mut *set).borrow_mut() };
    for bindings in set.iter_mut() {
        let mut cbindings = bindings_t{bindings: (bindings as  *mut Bindings).cast()};
        callback(&mut cbindings, context);
    }
}

/// @brief Asserts equality between two Variable atoms in a Bindings set
/// @ingroup matching_group
/// @param[in]  set  A pointer to the `bindings_set_t` to modify
/// @param[in]  a  A pointer to the first Variable atom for the variable <-> variable association
/// @param[in]  b  A pointer to the second Variable atom for the variable <-> variable association
/// @note Both `a` and `b` must be Variable atoms
///
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

/// @brief Adds a new variable <-> atom association to every Bindings frame in a Bindings set
/// @ingroup matching_group
/// @param[in]  set  A pointer to the `bindings_set_t` to modify
/// @param[in]  var  A pointer to the Variable atom
/// @param[in]  value  A pointer to the other atom to associate the variable with
/// @note This function is equivalent to calling `bindings_add_var_binding` for each Bindings frame in the
///    Bindings set, but new Bindings frames may be generated if the new association conflicts with existing
///    associations in existing frames
///
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

/// @brief Merges the contents of one Bindings set into another Bindings set
/// @ingroup matching_group
/// @param[in]  _self  A pointer to the `bindings_set_t` to modify
/// @param[in]  other  A pointer to another `bindings_set_t`
/// @note This function merges each Bindings frame in the `_self` set with each frame in the `other` set, using the
///    same logic as `bindings_merge`.  New frames will potentially be created when associations are found to conflict
///
#[no_mangle]
pub extern "C" fn bindings_set_merge_into(_self: *mut bindings_set_t, other: *const bindings_set_t) {
    let _self = unsafe{ (&mut *_self).borrow_mut() };
    let other = unsafe{ (&*other).borrow() };
    let mut owned_self = BindingsSet::empty();
    core::mem::swap(_self, &mut owned_self);

    let mut result_set = owned_self.merge(other);
    core::mem::swap(_self, &mut result_set);
}
