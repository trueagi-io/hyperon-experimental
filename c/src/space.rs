use hyperon::space::grounding::*;
use hyperon::space::*;
use hyperon::atom::*;
use hyperon::matcher::*;

use crate::atom::*;
use crate::util::*;

use std::os::raw::*;
use core::cell::RefCell;
use std::rc::{Rc, Weak};

//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-
// Space & SpaceMut trait interface wrapper
//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-

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

/// Returned atom_t* is borrowed from the space_event_t. The return value must not be freed or
/// accessed after the space_event_t has been freed
#[no_mangle]
pub extern "C" fn space_event_get_field_atom(event: *const space_event_t, field: space_event_field_t) -> *const atom_t {
    let event = unsafe{ &(*event).event };
    match field {
        space_event_field_t::SPACE_EVENT_FIELD_ADD => {
            if let SpaceEvent::Add(atom) = event {
                (atom as *const Atom).cast()
            } else {
                panic!("SpaceEvent wasn't an Add event")
            }
        },
        space_event_field_t::SPACE_EVENT_FIELD_REMOVE => {
            if let SpaceEvent::Remove(atom) = event {
                (atom as *const Atom).cast()
            } else {
                panic!("SpaceEvent wasn't a Remove event")
            }
        },
        space_event_field_t::SPACE_EVENT_FIELD_REPLACE_PATTERN |
        space_event_field_t::SPACE_EVENT_FIELD_REPLACE_TEMPLATE => {
            if let SpaceEvent::Replace(pattern_atom, template_atom) = event {
                match field {
                    space_event_field_t::SPACE_EVENT_FIELD_REPLACE_PATTERN => (pattern_atom as *const Atom).cast(),
                    space_event_field_t::SPACE_EVENT_FIELD_REPLACE_TEMPLATE => (template_atom as *const Atom).cast(),
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

/// A table of functions to define the behavior of a SpaceObserver implemented in C
///
/// notify
///   \arg payload \c is the pointer to the observer's payload
///   \arg event \c is the event the observer is notified about.  This function should NOT take ownership
///   of the event.
///
/// free_payload
///   \arg payload \c is the pointer to the observer's payload
///   NOTE: This function is responsible for freeing the payload buffer, as well as any other objects
///   and resources stored by the observer.
#[repr(C)]
pub struct space_observer_api_t {
    notify: extern "C" fn(payload: *mut c_void, event: *const space_event_t),

    free_payload: extern "C" fn(payload: *mut c_void),
}

struct CObserver {
    api: *const space_observer_api_t,
    payload: *mut c_void,
}

impl SpaceObserver for CObserver {
    fn notify(&mut self, event: &SpaceEvent) {
        let api = unsafe{ &*self.api };
        let event = (event as *const SpaceEvent).cast();
        (api.notify)(self.payload, event);
    }
}

impl Drop for CObserver {
    fn drop(&mut self) {
        let api = unsafe{ &*self.api };
        (api.free_payload)(self.payload);
    }
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

/// Creates an observer handle for a SpaceObserver implemented in C
///
/// WARNING: This function takes ownership of the payload, and it should not be freed after it
/// has been provided to this function
///
/// The returned space_observer_t must be freed with space_observer_free
#[no_mangle]
pub extern "C" fn space_observer_new(api: *const space_observer_api_t, payload: *mut c_void) -> *mut space_observer_t {
    let observer = CObserver {api, payload};
    let observer = space_observer_t{observer: Rc::new(RefCell::new(observer))};
    Box::into_raw(Box::new(observer))
}

/// Frees an observer handle when the space implementation is finished with it.
#[no_mangle]
pub extern "C" fn space_observer_free(observer: *mut space_observer_t) {
    let observer = unsafe{ Box::from_raw(observer) };
    drop(observer);
}

/// A table of functions to define the behavior of a space implemented in C
///
/// query
///   Returns a bindings_set_t representing the query results
///   \arg params \c is the pointer to the space's params
///   \arg atom \c is the query atom.  This function should NOT take ownership of the query atom.
///
/// subst
///   \arg params \c is the pointer to the space's params
///   \arg pattern \c is the pattern atom to match.  This function should NOT take ownership of the pattern atom.
///   \arg tmpl \c is the template atom.  This function should NOT take ownership of the template atom.
///   NOTE: If a subst function is provided, it will be called.  If NULL is provided, the default
///     implementation will be called.
///
/// add
///   \arg params \c is the pointer to the space's params
///   \arg atom \c is the atom to add to the space.  This function SHOULD take ownership of the atom.
///
/// remove
///   \arg params \c is the pointer to the space's params
///   \arg atom \c is the atom to remove from the space.  This function should NOT take ownership of the atom.
///   Returns `true` if the atom was removed, otherwise returns `false`
///
/// replace
///   \arg params \c is the pointer to the space's params
///   \arg from \c is the atom to replace in the space.  This function should NOT take ownership of the `from` atom.
///   \arg to \c is the atom to replace it with.  This function SHOULD take ownership of the `to` atom.
///   Returns `true` if the atom was replaced, otherwise returns `false`
///
/// atom_count
///   \arg params \c is the pointer to the space's params
///   NOTE: If an atom_count function is provided, it will be called.  NULL should be provided for spaces
///     that cannot readily determine the number of contained atoms
/// 
/// new_atom_iterator_state
///   \arg params \c is the pointer to the space's params
///   Returns an allocated pointer to state necessary to perform an iteration over all atoms
///   NOTE: The new_atom_iterator_state function is optional.  NULL should be provided for spaces
///     that cannot traverse all contained atoms in an orderly way
/// 
/// next_atom
///   \arg params \c is the pointer to the space's params
///   \arg state \c is the buffer allocated by new_atom_iterator_state
///   Returns a pointer to the next atom in the iteration sequence.  The returned pointer should point
///   to an atom owned by the space, and thus the implementation will not free it.  This function should
///   return NULL to signal the iteration has finished.
///   NOTE: The next_atom function is optional.  NULL should be provided for spaces that cannot
///     traverse all contained atoms in an orderly way.
/// 
/// free_atom_iterator_state
///   \arg params \c is the pointer to the space's params
///   \arg state \c is the buffer allocated by new_atom_iterator_state, that must be freed
///   NOTE: The free_atom_iterator_state function is optional.  NULL should be provided for spaces
///     that cannot traverse all contained atoms in an orderly way
/// 
/// free_payload
///   \arg payload \c is the pointer to the space's payload
///   NOTE: This function is responsible for freeing the payload buffer, as well as any other objects
///   and resources stored by the space.  This includes `atom_t` objects, as well as any other buffers
///   stored in the space
#[repr(C)]
pub struct space_api_t {
    query: extern "C" fn(params: *const space_params_t, atom: *const atom_t) -> *mut bindings_set_t,

    subst: Option<extern "C" fn(params: *const space_params_t, pattern: *const atom_t, tmpl: *const atom_t) -> *mut vec_atom_t>,

    add: extern "C" fn(params: *const space_params_t, atom: *mut atom_t),

    remove: extern "C" fn(params: *const space_params_t, atom: *const atom_t) -> bool,

    replace: extern "C" fn(params: *const space_params_t, from: *const atom_t, to: *mut atom_t) -> bool,

    atom_count: Option<extern "C" fn(params: *const space_params_t) -> usize>,

    new_atom_iterator_state: Option<extern "C" fn(params: *const space_params_t) -> *mut c_void>,

    next_atom: Option<extern "C" fn(params: *const space_params_t, state: *mut c_void) -> *const atom_t>,

    free_atom_iterator_state: Option<extern "C" fn(params: *const space_params_t, state: *mut c_void)>,

    free_payload: extern "C" fn(payload: *mut c_void),
}

pub struct space_observer_list_t {
    observers: RefCell<Vec<Weak<RefCell<dyn SpaceObserver>>>>
}

/// Notifies all observers of an event
#[no_mangle]
pub extern "C" fn space_observer_list_notify_all(list: *const space_observer_list_t, event: *const space_event_t) {
    let list = unsafe{ &(*list).observers };
    let event = unsafe{ &(*event).event };

    let mut cleanup = false;
    for observer in list.borrow().iter() {
        if let Some(observer) = observer.upgrade() {
            observer.borrow_mut().notify(event);
        } else {
            cleanup = true;
        }
    }
    if cleanup {
        list.borrow_mut().retain(|w| w.strong_count() > 0);
    }
}

/// Data associated with this particular space, including the space's payload and observers
#[repr(C)]
pub struct space_params_t {
    payload: *mut c_void,
    observers: Box<space_observer_list_t>,
}

struct CSpace {
    api: *const space_api_t,
    params: space_params_t,
}

impl CSpace {
    fn new(api: *const space_api_t, payload: *mut c_void) -> Self {
        CSpace{api, params: space_params_t{payload, observers: Box::new(space_observer_list_t::new())}}
    }
}

impl space_observer_list_t {
    fn new() -> Self {
        space_observer_list_t{observers: RefCell::new(vec![])}
    }
}

impl Space for CSpace {
    fn register_observer(&self, observer: Rc<RefCell<dyn SpaceObserver>>) {
        let observers_vec = &(*self.params.observers).observers;
        observers_vec.borrow_mut().push(Rc::downgrade(&observer) as Weak<RefCell<dyn SpaceObserver>>)
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        let api = unsafe{ &*self.api };
        let query = (query as *const Atom).cast::<atom_t>();
        let result_set = (api.query)(&self.params, query);
        let bindings_box: Box<bindings_set_t> = unsafe{ Box::from_raw(result_set) };
        bindings_box.set
    }
    fn subst(&self, pattern: &Atom, tmpl: &Atom) -> Vec<Atom> {
        let api = unsafe{ &*self.api };
        if let Some(subst_fn) = api.subst {
            let pattern = (pattern as *const Atom).cast::<atom_t>();
            let tmpl = (tmpl as *const Atom).cast::<atom_t>();
            let atom_vec = subst_fn(&self.params, pattern, tmpl);
            let vec_box = unsafe{ Box::from_raw(atom_vec) };
            (*vec_box).0
        } else {
            DefaultSpace(self).subst(pattern, tmpl)
        }
    }
    fn atom_count(&self) -> Option<usize> {
        let api = unsafe{ &*self.api };
        if let Some(atom_count_fn) = api.atom_count {
            let count = atom_count_fn(&self.params);
            Some(count)
        } else {
            None
        }
    }
    fn atom_iter(&self) -> Option<SpaceIter> {
        struct CSpaceIterator<'a>(&'a CSpace, *mut c_void);
        impl<'a> Iterator for CSpaceIterator<'a> {
            type Item = &'a Atom;
            fn next(&mut self) -> Option<&'a Atom> {
                let api = unsafe{ &*self.0.api };
                if let Some(next_atom) = api.next_atom {
                    let atom_ptr = next_atom(&self.0.params, self.1);
                    if atom_ptr.is_null() {
                        None
                    } else {
                        let atom = unsafe { &(*atom_ptr).atom };
                        Some(atom)
                    }
                } else {
                    panic!("next_atom function must be implemented if new_atom_iterator_state is implemented");
                }
            }
        }
        impl Drop for CSpaceIterator<'_> {
            fn drop(&mut self) {
                let api = unsafe{ &*self.0.api };
                if let Some(free_atom_iterator_state) = api.free_atom_iterator_state {
                    free_atom_iterator_state(&self.0.params, self.1);
                } else {
                    panic!("free_atom_iterator_state function must be implemented if new_atom_iterator_state is implemented");
                }
            }
        }

        let api = unsafe{ &*self.api };
        if let Some(new_atom_iterator_state) = api.new_atom_iterator_state {
            let ctx = new_atom_iterator_state(&self.params);
            let new_iter = CSpaceIterator(self, ctx);
            Some(SpaceIter::new(new_iter))
        } else {
            None
        }
    }
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        Some(self)
    }
}

impl std::fmt::Display for CSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CSpace")
    }
}
impl std::fmt::Debug for CSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CSpace")
    }
}

/// A stub internal object, to get access to the default method of Space trait
#[derive(Debug)]
struct DefaultSpace<'a>(&'a CSpace);
impl Space for DefaultSpace<'_> {
    fn register_observer(&self, _observer: Rc<RefCell<dyn SpaceObserver>>) {}
    fn query(&self, query: &Atom) -> BindingsSet { self.0.query(query) }
    fn atom_count(&self) -> Option<usize> { self.0.atom_count() }
    fn atom_iter(&self) -> Option<SpaceIter> { self.0.atom_iter() }
    fn as_any(&self) -> Option<&dyn std::any::Any> { Some(self.0) }
}
impl std::fmt::Display for DefaultSpace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DefaultSpace")
    }
}

impl SpaceMut for CSpace {
    fn add(&mut self, atom: Atom) {
        let api = unsafe{ &*self.api };
        let atom = atom_into_ptr(atom);
        (api.add)(&self.params, atom);
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        let api = unsafe{ &*self.api };
        let atom = (atom as *const Atom).cast::<atom_t>();
        (api.remove)(&self.params, atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        let api = unsafe{ &*self.api };
        let from = (from as *const Atom).cast::<atom_t>();
        let to = atom_into_ptr(to);
        (api.replace)(&self.params, from, to)
    }
    fn as_space(&self) -> &dyn Space {
        self
    }
}

impl Drop for CSpace {
    fn drop(&mut self) {
        let api = unsafe{ &*self.api };
        (api.free_payload)(self.params.payload);
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
    let c_space = CSpace::new(api, payload);
    space_t::new(Box::new(c_space))
}

#[no_mangle]
pub extern "C" fn space_free(space: *mut space_t) {
    space_t::drop(space)
}

#[no_mangle]
pub unsafe extern "C" fn space_eq(a: *const space_t, b: *const space_t) -> bool {
    *a == *b
}

/// Returns the pointer to the payload for a space created from C
///
/// WARNING: The returned payload ptr must not be freed, nor must it be accessed after the space
/// has been freed
#[no_mangle]
pub extern "C" fn space_get_payload(space: *mut space_t) -> *mut c_void {
    if let Some(any_ref) = unsafe{ (*space).borrow_mut().as_any() } {
        if let Some(c_space) = any_ref.downcast_ref::<CSpace>() {
            return c_space.params.payload;
        }
    }
    panic!("Only CSpace has a payload")
}

#[no_mangle]
pub extern "C" fn space_register_observer(space: *mut space_t, observer: *const space_observer_t) {
    let space = unsafe{ (*space).borrow_mut() };
    let observer = unsafe{ &(*observer).observer };
    space.register_observer(observer.clone());
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

/// NOTE: This function will return -1 for spaces on which it is impossible or impractical
/// to determine the number of atoms
#[no_mangle]
pub extern "C" fn space_atom_count(space: *const space_t) -> isize {
    match unsafe { (*space).borrow().atom_count() } {
        Some(count) => count as isize,
        None => -1
    }
}

/// NOTE: This function will do nothing for spaces on which space_atom_count() returns a
/// non-positive number
#[no_mangle]
pub extern "C" fn space_iterate(space: *const space_t,
        callback: c_atom_callback_t, context: *mut c_void) {
    if let Some(atom_iter) = unsafe { (*space).borrow().atom_iter() } {
        for atom in atom_iter {
            callback((atom as *const Atom).cast(), context);
        }
    }
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
