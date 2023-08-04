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
// @relates atom_t
// @relates atom_ref_t
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
// @relates atom_ref_t
/// @return an `atom_ref_t` referencing nothing
///
/// Returns an atom_ref_t that does not point to any atom
#[no_mangle]
pub unsafe extern "C" fn atom_ref_null() -> atom_ref_t {
    atom_ref_t::null()
}

/// @brief Create a new Symbol atom with the specified name
/// @ingroup atom_group
// @relates atom_t
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
// @relates atom_t
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
// @relates atom_t
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
// @relates atom_t
/// @param[in]  name  The identifier for the newly created Variable atom
/// @return An `atom_t` for the Variable atom
/// @note The caller must take ownership responsibility for the returned `atom_t`
///
#[no_mangle]
pub unsafe extern "C" fn atom_var(name: *const c_char) -> atom_t {
    Atom::var(cstr_as_str(name)).into()
}

/// @ingroup atom_group
// @relates atom_t
// @relates gnt_t
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
// @relates atom_t
// @relates atom_ref_t
// @relates space_t
/// @param[in]  space  A pointer to an `space_t` for accessing the space
/// @return an `atom_t` for the Grounded atom
/// @note The caller must take ownership responsibility for the returned `atom_t`
/// @note This function does not consume the space and the space still must be freed with `space_free()`
///
#[no_mangle]
pub extern "C" fn atom_gnd_for_space(space: *const space_t) -> atom_t {
    let space = unsafe { &(*space).0 };
    Atom::gnd(space.clone()).into()
}

/// @brief Frees an atom and all associated resources
/// @ingroup atom_group
// @relates atom_t
// @relates atom_ref_t
/// @param[in]  atom  The atom to free
///
#[no_mangle]
pub unsafe extern "C" fn atom_free(atom: atom_t) {
    // drop() does nothing actually, but it is used here for clarity
    drop(atom.into_inner());
}

/// @brief Makes a "deep copy" of an atom.  Useful to turn an `atom_ref_t` into an `atom_t`
/// @ingroup atom_group
// @relates atom_t
// @relates atom_ref_t
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
// @relates atom_t
// @relates atom_ref_t
/// @param[in]  atoma  A pointer to an `atom_t` or an `atom_ref_t` representing the first atom
/// @param[in]  atoma  A pointer to an `atom_t` or an `atom_ref_t` representing the second atom
/// @return `true` is the atoms are conceptually identical
/// @note The caller must take ownership responsibility for the returned `atom_t`
///
#[no_mangle]
pub unsafe extern "C" fn atom_eq(atoma: *const atom_ref_t, atomb: *const atom_ref_t) -> bool {
    (&*atoma).borrow() == (&*atomb).borrow()
}

/// @brief Returns the type of an atom
/// @ingroup atom_group
// @relates atom_t
// @relates atom_ref_t
// @relates atom_type_t
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
// @relates atom_t
// @relates atom_ref_t
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to inspect
/// @return `true` if the referenced atom is invalid, otherwise returns `false`
///
#[no_mangle]
pub unsafe extern "C" fn atom_is_null(atom: *const atom_ref_t) -> bool {
    (*atom).is_null()
}

/// @brief Renders a human-readable text description of an atom
/// @ingroup atom_group
// @relates atom_t
// @relates atom_ref_t
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
// @relates atom_t
// @relates atom_ref_t
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
// @relates atom_t
// @relates atom_ref_t
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to access
/// @param[in]  callback  A function the implementation will call, to return access to the atoms
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
// @relates atom_t
// @relates atom_ref_t
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` to iterate
/// @param[in]  callback  A function the implementation will call, to return access to each contained atom
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
// @relates atom_t
// @relates atom_ref_t
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
// @relates atom_t
// @relates atom_ref_t
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
// @relates atom_t
// @relates atom_ref_t
/// @see atom_gnd_for_space
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` that wraps a Space
/// @return The pointer to the `space_t` inside a grounded atom
/// @warning The returned space is borrowed from the atom.  It must not be accessed after the atom has been freed or modified elsewhere
///
#[no_mangle]
pub unsafe extern "C" fn atom_get_space(atom: *const atom_ref_t) -> *const space_t {
    let atom = (&*atom).borrow();
    if let Some(space) = Atom::as_gnd::<DynSpace>(atom) {
        (space as *const DynSpace).cast()
    } else {
        panic!("Atom does not reference a space");
    }
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Grounded Atom Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

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
    /// @param[out]  out  A pointer to a mutable `atom_vec_t`, into which newly created atoms may be added by the execution
    /// @return An `exec_error_t` status that informs the MeTTa interpreter if execution may continue or whether to handle a fault
    /// @note Assigning NULL to this field means the atom is not executable
    ///
    execute: Option<extern "C" fn(gnd: *const gnd_t, args: *const atom_vec_t, out: *mut atom_vec_t) -> *mut exec_error_t>,

    /// @brief An optional function to match the atom with another atom
    /// @param[in]  gnd  A pointer to the Grounded Atom object
    /// @param[in]  other  The other atom to match with
    /// @param[out]  callback  A function to call, to supply the bindings.  If this function is never called that means the atoms do not match
    /// @param[in]  context  The opaque context pointer to pass to the `callback` function when it is called
    /// @note Assigning NULL to this field means the atom will match only other atoms which are considered equal by the `eq` function below
    ///
    match_: Option<extern "C" fn(gnd: *const gnd_t, other: *const atom_ref_t, callback: bindings_mut_callback_t, context: *mut c_void)>,

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

    #[no_mangle]
    pub extern "C" fn grounded_match_callback(cbindings: *mut bindings_t, context: *mut c_void) {
        let bindings = ptr_into_bindings(cbindings);
        let vec_bnd = unsafe{ &mut *context.cast::<Vec<Bindings>>() };
        vec_bnd.push(bindings);
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
                let ret = if error.is_null() {
                    Ok(ret.into())
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
                    CGrounded::grounded_match_callback, context);
                Box::new(results.into_iter())
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

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Atom Vec Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @struct atom_vec_t
/// @brief Contains a vector (list) of Atoms
/// @ingroup atom_vec_group
/// @note `atom_vec_t` must be freed with `atom_vec_free()`, or passed by value
/// to a function that takes ownership of the vec.
/// @warning It is unsafe to directly access the fields of this struct, so accessor functions must be used.
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

/// @brief Function signature for a callback providing access to an `atom_vec_t`
/// @ingroup atom_vec_group
/// @param[in]  vec  The `atom_vec_t` being provided.  This vec should not be modified or freed by the callback.
/// @param[in]  context  The context state pointer initially passed to the upstream function initiating the callback.
///
pub type c_atom_vec_callback_t = extern "C" fn(vec: *const atom_vec_t, context: *mut c_void);

/// @brief Creates a new empty `atom_vec_t`
/// @ingroup atom_vec_group
// @relates atom_vec_t
/// @return The newly created `atom_vec_t`
/// @note The caller must take ownership responsibility for the returned `atom_vec_t`
///
#[no_mangle]
pub extern "C" fn atom_vec_new() -> atom_vec_t {
    atom_vec_t::new()
}

/// @brief Creates a new `atom_vec_t` from a C-style array
/// @ingroup atom_vec_group
// @relates atom_vec_t
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
// @relates atom_vec_t
/// @param[in]  vec  The vec to free
///
#[no_mangle]
pub unsafe extern "C" fn atom_vec_free(vec: atom_vec_t) {
    drop(vec);
}

/// @brief Returns the number of elements in a vec
/// @ingroup atom_vec_group
// @relates atom_vec_t
/// @param[in]  vec  The vec to be inspected
/// @return The count of the number of elements contained within the vec
///
#[no_mangle]
pub unsafe extern "C" fn atom_vec_len(vec: *const atom_vec_t) -> usize {
    (*vec).len
}

/// @brief Removes the last element from a vec, and returns it
/// @ingroup atom_vec_group
// @relates atom_vec_t
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
// @relates atom_vec_t
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
// @relates atom_vec_t
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

/// The object returned from this function must be freed with bindings_set_free()
#[no_mangle]
pub extern "C" fn atom_match_atom(a: *const atom_ref_t, b: *const atom_ref_t) -> bindings_set_t {
    let a = unsafe{ (&*a).borrow() };
    let b = unsafe{ (&*b).borrow() };
    let result_set: BindingsSet = crate::atom::matcher::match_atoms(a, b).collect();
    result_set.into()
}

pub struct exec_error_t {
    pub error: ExecError,
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
pub extern "C" fn bindings_traverse(cbindings: *const bindings_t, callback: c_var_binding_callback_t, context: *mut c_void) {
    let bindings = unsafe{&(*cbindings).bindings};

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
    let callback_args: Vec<(Atom, Atom)> = bindings.iter().map(|(var, atom)| {
        (Atom::Variable(var.clone()), atom)
    }).collect();
    callback_args.iter().for_each(|(var, atom)| callback(var.into(), atom.into(), context));
}

#[no_mangle]
pub extern "C" fn bindings_add_var_binding(bindings: * mut bindings_t, var: atom_t, atom: atom_t) -> bool {
    let bindings = unsafe{ &mut(*bindings).bindings };
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
pub extern "C" fn bindings_narrow_vars(bindings: *mut bindings_t, vars: *const atom_vec_t) {
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

pub type c_var_binding_callback_t = extern "C" fn(var: atom_ref_t, value: atom_ref_t, context: *mut c_void);

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

pub fn return_atoms(atoms: &Vec<Atom>, callback: c_atom_vec_callback_t, context: *mut c_void) {
    callback(&(&atoms[..]).into(), context);
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

        CGrounded::grounded_match_callback( cbindings, context);

        assert_eq!(results, vec![Bindings::from(vec![
                (VariableAtom::new("var"), Atom::sym("atom_test"))])]);
    }

}
