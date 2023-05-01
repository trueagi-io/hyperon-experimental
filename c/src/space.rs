use hyperon::space::grounding::*;
use hyperon::space::*;
use hyperon::atom::*;
use hyperon::matcher::*;

use crate::atom::*;
use crate::util::*;

use std::os::raw::*;
use core::cell::RefCell;
use std::rc::Rc;

//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-
// Space & SpaceMut trait interface wrapper
//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-

//QUESTION FOR VITALY: This C interface could be simplified a lot by making the CSpace object
// automatically handles calls to the observer instead of exporting the mechanism to register
// observers.  However, my reasoning was that there must be a good reason the observer
// interaction is part of the Space trait, and not part of a wrapper around a Space, and that
// was the reason  I carried the observer mechanism forward into this C API.
//
//However, if there isn't a situation where a given space needs to manipulate the events seen
// by the observer (hiding some events?, creating new events?) then we can simplify the API as
// well as eliminate a potential source of mistakes by storing observers and handling observer
// interactions automatically for CSpaces.

#[repr(C)]
pub enum space_event_type_t {
    SPACE_EVENT_TYPE_ADD,
    SPACE_EVENT_TYPE_REMOVE,
    SPACE_EVENT_TYPE_REPLACE,
}

#[repr(C)]
pub enum space_event_field_t {
    SPACE_EVENT_FIELD_ADD,
    SPACE_EVENT_FIELD_REMOVE,
    SPACE_EVENT_FIELD_REPLACE_PATTERN,
    SPACE_EVENT_FIELD_REPLACE_TEMPLATE,
}

pub struct space_event_t {
    event: SpaceEvent,
}

#[no_mangle]
pub extern "C" fn space_event_get_type(event: *const space_event_t) -> space_event_type_t {
    let event = unsafe{ &(*event).event };
    match event {
        SpaceEvent::Add(_) => space_event_type_t::SPACE_EVENT_TYPE_ADD,
        SpaceEvent::Remove(_) => space_event_type_t::SPACE_EVENT_TYPE_REMOVE,
        SpaceEvent::Replace(_, _) => space_event_type_t::SPACE_EVENT_TYPE_REPLACE,
    }
}

/// Returned atom must be freed with atom_free
#[no_mangle]
pub extern "C" fn space_event_get_field_atom(event: *const space_event_t, field: space_event_field_t) -> *mut atom_t {
    let event = unsafe{ &(*event).event };
    match field {
        space_event_field_t::SPACE_EVENT_FIELD_ADD => {
            if let SpaceEvent::Add(atom) = event {
                atom_into_ptr(atom.clone())
            } else {
                panic!("SpaceEvent wasn't an Add event")
            }
        },
        space_event_field_t::SPACE_EVENT_FIELD_REMOVE => {
            if let SpaceEvent::Remove(atom) = event {
                atom_into_ptr(atom.clone())
            } else {
                panic!("SpaceEvent wasn't a Remove event")
            }
        },
        space_event_field_t::SPACE_EVENT_FIELD_REPLACE_PATTERN |
        space_event_field_t::SPACE_EVENT_FIELD_REPLACE_TEMPLATE => {
            if let SpaceEvent::Replace(pattern_atom, template_atom) = event {
                match field {
                    space_event_field_t::SPACE_EVENT_FIELD_REPLACE_PATTERN => atom_into_ptr(pattern_atom.clone()),
                    space_event_field_t::SPACE_EVENT_FIELD_REPLACE_TEMPLATE => atom_into_ptr(template_atom.clone()),
                    _ => unreachable!()
                }
            } else {
                panic!("SpaceEvent wasn't a Replace event")
            }
        }
    }
}

/// Returned space_event_t must be freed with space_event_free
/// 
/// WARNING: This function takes ownership of the supplied atom, so it should not be freed or
/// accessed subsequently
#[no_mangle]
pub extern "C" fn space_event_new_add(atom: *mut atom_t) -> *mut space_event_t {   
    let event = SpaceEvent::Add(ptr_into_atom(atom));
    Box::into_raw(Box::new(space_event_t{ event }))    
}

/// Returned space_event_t must be freed with space_event_free
/// 
/// WARNING: This function takes ownership of the supplied atom, so it should not be freed or
/// accessed subsequently
#[no_mangle]
pub extern "C" fn space_event_new_remove(atom: *mut atom_t) -> *mut space_event_t {   
    let event = SpaceEvent::Remove(ptr_into_atom(atom));
    Box::into_raw(Box::new(space_event_t{ event }))    
}

/// Returned space_event_t must be freed with space_event_free
/// 
/// WARNING: This function takes ownership of both supplied atoms, so they should not be freed or
/// accessed subsequently
#[no_mangle]
pub extern "C" fn space_event_new_replace(pattern: *mut atom_t, tmpl: *mut atom_t) -> *mut space_event_t {   
    let event = SpaceEvent::Replace(ptr_into_atom(pattern), ptr_into_atom(tmpl));
    Box::into_raw(Box::new(space_event_t{ event }))    
}

#[no_mangle]
pub extern "C" fn space_event_free(event: *mut space_event_t) {
    let event = unsafe{ Box::from_raw(event) };
    drop(event);
}

/// A handle to a space observer.
/// 
//QUESTION FOR VITALY: Is the idea that the same observer will observe many spaces, including
// both C spaces & native Rust spaces?  Personally, it seems like it might be a better
// structure to put the internal mutability, ie. Rc<RefCell<T>> pattern, inside the observer,
// object itself, rather than on the register interface.  But perhaps I am missing something?
pub struct space_observer_t{
    observer: Rc<RefCell<dyn SpaceObserver>>,
}

/// Notifies an observer of an event
#[no_mangle]
pub extern "C" fn space_observer_notify(observer: *const space_observer_t, event: *const space_event_t) {
    let observer = unsafe{ &(*observer).observer };
    let event = unsafe{ &(*event).event };
    observer.borrow_mut().notify(event);
}

/// Frees an observer handle when the space implementation is finished with it.
#[no_mangle]
pub extern "C" fn space_observer_free(observer: *mut space_observer_t) {
    let observer = unsafe{ Box::from_raw(observer) };
    drop(observer);
}

/// A table of functions to define the behavior of a space implemented in C
/// 
/// register_observer
///   \arg payload \c is the pointer to the space's payload
///   \arg observer \c is the observer object to register with the space.
///     NOTE: It is the responsibility of the space implementation to store all space_observer_t
///     pointers receieved from calls to `register_observer` inside the payload, and to free them
///     with space_observer_free before returning from free_payload.
/// 
/// query
///   Returns a bindings_set_t representing the query results
///   \arg payload \c is the pointer to the space's payload
///   \arg atom \c is the query atom.  This function should NOT take ownership of the query atom.
/// 
/// subst
///   \arg payload \c is the pointer to the space's payload
///   \arg pattern \c is the pattern atom to match.  This function should NOT take ownership of the pattern atom.
///   \arg tmpl \c is the template atom.  This function should NOT take ownership of the template atom.
///   NOTE: If a subst function is provided, it will be called.  If NULL is provided, the default
///     implementation will be called.
/// 
/// add
///   \arg payload \c is the pointer to the space's payload
///   \arg atom \c is the atom to add to the space.  This function SHOULD take ownership of the atom.
/// 
/// remove
///   \arg payload \c is the pointer to the space's payload
///   \arg atom \c is the atom to remove from the space.  This function should NOT take ownership of the atom.
///   Returns `true` if the atom was removed, otherwise returns `false`
/// 
/// replace
///   \arg payload \c is the pointer to the space's payload
///   \arg from \c is the atom to replace in the space.  This function should NOT take ownership of the `from` atom.
///   \arg to \c is the atom to replace it with.  This function SHOULD take ownership of the `to` atom.
///   Returns `true` if the atom was replaced, otherwise returns `false`
/// 
/// free_payload
///   \arg payload \c is the pointer to the space's payload
///   NOTE: This function is responsible for freeing the payload buffer, as well as any other objects
///   and resources stored by the space.  This includes `atom_t` objects, `space_observer_t` objects,
///   as well as any other buffers stored in the space
#[repr(C)]
pub struct space_api_t {
    register_observer: extern "C" fn(payload: *mut c_void, observer: *mut space_observer_t),

    query: extern "C" fn(payload: *mut c_void, atom: *const atom_t) -> *mut bindings_set_t,

    subst: Option<extern "C" fn(payload: *mut c_void, pattern: *const atom_t, tmpl: *const atom_t) -> *mut vec_atom_t>,

    add: extern "C" fn(payload: *mut c_void, atom: *mut atom_t),

    remove: extern "C" fn(payload: *mut c_void, atom: *const atom_t) -> bool,

    replace: extern "C" fn(payload: *mut c_void, from: *const atom_t, to: *mut atom_t) -> bool,

    free_payload: extern "C" fn(payload: *mut c_void),
}

//QUESTION FOR VITALY: Looking to the CGrounded type for guidance about your preferred
// patterns, I see that the type CGrounded wraps an AtomicPtr<>, but I don't understand
// the situation where multiple threads would have shared access to this pointer and need
// to atomically update it
struct CSpace {
    api: *const space_api_t,
    payload: *mut c_void,
}

impl Space for CSpace {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>) {
        let api = unsafe{ &*self.api };
        let observer_ptr = Box::into_raw(Box::new(space_observer_t{observer}));
        (api.register_observer)(self.payload, observer_ptr);
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        let api = unsafe{ &*self.api };
        let query = (query as *const Atom).cast::<atom_t>();
        let result_set = (api.query)(self.payload, query);
        let bindings_box: Box<bindings_set_t> = unsafe{ Box::from_raw(result_set) };
        bindings_box.set
    }
    fn subst(&self, pattern: &Atom, tmpl: &Atom) -> Vec<Atom> {
        let api = unsafe{ &*self.api };
        if let Some(subst_fn) = api.subst {
            let pattern = (pattern as *const Atom).cast::<atom_t>();
            let tmpl = (tmpl as *const Atom).cast::<atom_t>();
            let atom_vec = subst_fn(self.payload, pattern, tmpl);
            let vec_box = unsafe{ Box::from_raw(atom_vec) };
            (*vec_box).0
        } else {
            DefaultSpace(self).subst(pattern, tmpl)
        }
    }
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        Some(self)
    }
}

/// A stub internal object, to get access to the default method of Space trait
struct DefaultSpace<'a>(&'a CSpace);
impl Space for DefaultSpace<'_> {
    fn register_observer(&self, _observer: Rc<RefCell<dyn SpaceObserver>>) {}
    fn query(&self, query: &Atom) -> BindingsSet { self.0.query(query) }
    fn as_any(&self) -> Option<&dyn std::any::Any> { Some(self.0) }
}

impl SpaceMut for CSpace {
    fn add(&mut self, atom: Atom) {
        let api = unsafe{ &*self.api };
        let atom = atom_into_ptr(atom);
        (api.add)(self.payload, atom);
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        let api = unsafe{ &*self.api };
        let atom = (atom as *const Atom).cast::<atom_t>();
        (api.remove)(self.payload, atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        let api = unsafe{ &*self.api };
        let from = (from as *const Atom).cast::<atom_t>();
        let to = atom_into_ptr(to);
        (api.replace)(self.payload, from, to)
    }
    fn as_space(&self) -> &dyn Space {
        self
    }
}

impl Drop for CSpace {
    fn drop(&mut self) {
        let api = unsafe{ &*self.api };
        (api.free_payload)(self.payload);
    }
}

pub type space_t = SharedApi<Box<dyn SpaceMut>>;

/// Creates a new space_t, backed by an implementation in C
/// 
/// WARNING: This function takes ownership of the payload, and it should not be freed or
/// accessed after it has been provided to this function
/// 
/// The returned space_t must be freed with space_free
#[no_mangle]
pub extern "C" fn space_new(api: *const space_api_t, payload: *mut c_void) -> *mut space_t {
    let c_space = CSpace {api, payload};
    space_t::new(Box::new(c_space))
}

#[no_mangle]
pub extern "C" fn space_free(space: *mut space_t) {
    space_t::drop(space)
}

/// Returns the pointer to the payload for a space created from C
/// 
/// WARNING: The returned payload ptr must not be freed, nor must it be accessed after the space
/// has been freed
#[no_mangle]
pub extern "C" fn space_get_payload(space: *mut space_t) -> *mut c_void {
    if let Some(any_ref) = unsafe{ (*space).borrow_mut().as_any() } {
        if let Some(c_space) = any_ref.downcast_ref::<CSpace>() {
            return c_space.payload;
        }
    }
    panic!("Only CSpace has a payload")
}

/// WARNING: This function takes ownership of the supplied atom, and it should not be freed or
/// accessed after it has been provided to this function
#[no_mangle]
pub unsafe extern "C" fn space_add(space: *mut space_t, atom: *mut atom_t) {
    (*space).borrow_mut().add(ptr_into_atom(atom));
}

#[no_mangle]
pub unsafe extern "C" fn space_remove(space: *mut space_t, atom: *const atom_t) -> bool {
    (*space).borrow_mut().remove(&(*atom).atom)
}

/// WARNING: This function takes ownership of the `to` atom, and it should not be freed or
/// accessed after it has been provided to this function
#[no_mangle]
pub unsafe extern "C" fn space_replace(space: *mut space_t, from: *const atom_t, to: *mut atom_t) -> bool {
    (*space).borrow_mut().replace(&(*from).atom, ptr_into_atom(to))
}

#[no_mangle]
pub extern "C" fn space_query(space: *const space_t,
        pattern: *const atom_t, callback: lambda_t<* const bindings_t>, context: *mut c_void) {
    let results = unsafe { (*space).borrow().query(&((*pattern).atom)) };
    for result in results.into_iter() {
        let b = (&result as *const Bindings).cast();
        callback(b, context);
    }
}

#[no_mangle]
pub extern "C" fn space_subst(space: *const space_t,
        pattern: *const atom_t, templ: *const atom_t,
        callback: c_atoms_callback_t, context: *mut c_void) {
    let results = unsafe { (*space).borrow().subst(&((*pattern).atom), &((*templ).atom)) };
    return_atoms(&results, callback, context);
}

//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-
// Grounding Space
//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-

/// Creates a new space_t, backed by a GroundSpace
/// 
/// The returned space_t must be freed with space_free
#[no_mangle]
pub extern "C" fn space_new_grounding_space() -> *mut space_t {
    space_t::new(Box::new(GroundingSpace::new()))
}

//QUESTION FOR VITALY: Should we deprecate all the grounding_space_t functions, and replace them
// with calls through the abstract API, or with calls that take a space_t, and downcast it to a
// grounding_space when the function is specific to a grounding_space?
//
//Or alternatively, we can keep the grounding_space_t, and provide bidirectional conversion
// functions.

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
    (*space).borrow_mut().add(ptr_into_atom(atom));
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_remove(space: *mut grounding_space_t, atom: *const atom_t) -> bool {
    (*space).borrow_mut().remove(&(*atom).atom)
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_replace(space: *mut grounding_space_t, from: *const atom_t, to: *mut atom_t) -> bool {
    (*space).borrow_mut().replace(&(*from).atom, ptr_into_atom(to))
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_len(space: *const grounding_space_t) -> usize {
    (*space).borrow().iter().count()
}

#[no_mangle]
pub unsafe extern "C" fn grounding_space_get(space: *const grounding_space_t, idx: usize) -> *mut atom_t {
    // TODO: highly ineffective implementation, should be reworked after replacing
    // the GroundingSpace struct by Space trait in code.
    
    //QUESTION FOR VITALY: Do all spaces have integer-indexable atoms?  If so, it might make sense
    // to make get(idx) into a trait method of Space.  Or alternatively should all spaces support
    // atom iteration, we could add an iter() method to the trait.  However, as you point out,
    // Iterator::skip() or Iterator::nth() is a costly way to perform integer-based access in a
    // large set.
    atom_into_ptr((*space).borrow().iter().skip(idx).next()
        .expect(format!("Index is out of bounds: {}", idx).as_str()).clone())
}

#[no_mangle]
pub extern "C" fn grounding_space_query(space: *const grounding_space_t,
        pattern: *const atom_t, callback: lambda_t<* const bindings_t>, context: *mut c_void) {
    let results = unsafe { (*space).borrow().query(&((*pattern).atom)) };
    for result in results.into_iter() {
        let b = bindings_into_ptr(result);
        callback(b, context);
    }
}

#[no_mangle]
pub extern "C" fn grounding_space_subst(space: *const grounding_space_t,
        pattern: *const atom_t, templ: *const atom_t,
        callback: c_atoms_callback_t, context: *mut c_void) {
    let results = unsafe { (*space).borrow().subst(&((*pattern).atom), &((*templ).atom)) };
    return_atoms(&results, callback, context);
}


