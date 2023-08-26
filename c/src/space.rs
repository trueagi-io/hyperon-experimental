use hyperon::common::FlexRef;
use hyperon::space::grounding::*;
use hyperon::space::*;
use hyperon::atom::*;
use hyperon::matcher::*;

use crate::atom::*;

use std::os::raw::*;

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Space Client Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @struct space_t
/// @brief A Space handle, providing access to a Space in which atoms may exist in relation to other atoms
/// @ingroup space_client_group
/// @note Multiple `space_t` handles may refer to the same underlying space.  An underlying space
///    will only be deallocated when all `space_t` handles that refer to it have been freed
/// @note `space_t` must be freed with `space_free()`, or passed by value
///    to a function that takes ownership of the space.
///
#[repr(C)]
pub struct space_t{
    /// Internal.  Should not be accessed directly
    space: *const RustOpaqueSpace
}

struct RustOpaqueSpace(DynSpace);

//INTERNAL NOTE: There are two reasons we need to box this space_t, rather than going directly from
// the internal Rc to a raw pointer.
// 1. The Rc inside DynSpace isn't pub, because we don't want to expose it to the Rust clients
// 2. More importantly, the <dyn SpaceMut> type is unSized, so we can't recreate the Rc without
//   knowing the size, and that knowledge is lost if we convert to a raw ptr.  So the indirection is
//   neessary to preserve the allocation meta-data.
impl space_t {
    pub(crate) fn into_inner(self) -> DynSpace {
        unsafe{ Box::from_raw(self.space.cast_mut()).0 }
    }
    pub(crate) fn borrow(&self) -> &DynSpace {
        unsafe{ &(&*self.space).0 }
    }
}

impl From<DynSpace> for space_t {
    fn from(space: DynSpace) -> Self {
        space_t{space: Box::into_raw(Box::new(RustOpaqueSpace(space))) }
    }
}

impl PartialEq for space_t {
    fn eq(&self, other: &Self) -> bool {
        self.borrow() == other.borrow()
    }
}

/// @brief Creates a new Space, backed by an implementation in C
/// @ingroup space_client_group
/// @param[in]  api  A pointer to the table of functions that implement the Space's behavior
/// @param[in]  payload  A pointer to a buffer to back the new Space
/// @return A newly created `space_t` handle, to access the Space
/// @warning This function takes ownership of the payload buffer, and it should not be freed or accessed after it
///    has been provided to this function
/// @note The caller must take ownership responsibility for the returned `space_t`, and free it with `space_free()`
///
#[no_mangle]
pub extern "C" fn space_new(api: *const space_api_t, payload: *mut c_void) -> space_t {
    let c_space = CSpace::new(api, payload);
    DynSpace::new(c_space).into()
}

/// @brief Frees a `space_t` handle
/// @ingroup space_client_group
/// @param[in]  space  The handle to free
/// @note The underlying Space may be deallocated if all handles that refer to it have been freed, otherwise
///    the Space itself won't be freed
///
#[no_mangle]
pub extern "C" fn space_free(space: space_t) {
    let space = space.into_inner();
    drop(space)
}

/// @brief Creates a new `space_t` handle that refers to the same underlying space as another `space_t`
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to clone
/// @return A newly created `space_t` handle, providing access to the same underlying Space
/// @note Each `space_t` is an independent object and must be individually freed, but both `space_t` objects
///    reference the same underlying space, which is only deallocated when all references to it have been freed.
/// @note The caller must take ownership responsibility for the returned `space_t`, and free it with `space_free()`
///
#[no_mangle]
pub extern "C" fn space_clone_handle(space: *const space_t) -> space_t {
    let space = unsafe { &*space }.borrow();
    space.clone().into()
}

/// @brief Checks if two `space_t` handles refer to the same underlying Space
/// @ingroup space_client_group
/// @param[in]  a  A pointer to the first `space_t`
/// @param[in]  b  A pointer to the second `space_t`
/// @return `true` is the underlying Space is the same for both `space_t` objects, and `false` otherwise
///
#[no_mangle]
pub extern "C" fn space_eq(a: *const space_t, b: *const space_t) -> bool {
    let a = unsafe{ &*a }.borrow();
    let b = unsafe{ &*b }.borrow();
    *a == *b
}

/// @brief Access the payload object belonging to a space implemented in C
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to access
/// @return The pointer to the payload object orginally used to create the Space
/// @note This function is only valid for Spaces implemented via the HyperonC API
/// @note This API is likely to change in the future to implement multi-threading as the space's payload
///    object must not be modified while other operations can access the space
/// @warning The returned payload ptr must not be freed, nor may it be accessed after the space
///    has been freed or modified
///
#[no_mangle]
pub extern "C" fn space_get_payload(space: *mut space_t) -> *mut c_void {
    let dyn_space = unsafe{ &*space }.borrow();
    if let Some(any_ref) = dyn_space.borrow_mut().as_any() {
        if let Some(c_space) = any_ref.downcast_ref::<CSpace>() {
            return c_space.params.payload;
        }
    }
    panic!("Only CSpace has a payload")
}

/// @brief Adds an atom to the Space
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to access
/// @param[in]  atom  An `atom_t` representing the Atom to add
/// @warning This function takes ownership of the supplied atom, and it should not be freed or accessed after
///    it has been provided to this function
///
#[no_mangle]
pub extern "C" fn space_add(space: *mut space_t, atom: atom_t) {
    let dyn_space = unsafe{ &*space }.borrow();
    dyn_space.borrow_mut().add(atom.into_inner());
}

/// @brief Removes a specific atom from the Space
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to access
/// @param[in]  atom  A pointer to an `atom_t` or `atom_ref_t` to specifying the atom to remove from the Space
/// @return `true` if the atom was found and removed from the Space, `false` otherwise
///
#[no_mangle]
pub extern "C" fn space_remove(space: *mut space_t, atom: *const atom_ref_t) -> bool {
    let dyn_space = unsafe{ &*space }.borrow();
    let atom = unsafe{ &*atom }.borrow();
    dyn_space.borrow_mut().remove(atom)
}

/// @brief Replaces an Atom in the Space with another Atom
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to access
/// @param[in]  from  A pointer to an `atom_t` or `atom_ref_t` to specify the atom to replace
/// @param[in]  to  An `atom_t` to provide a new Atom, to replace the `from` atom in the Space
/// @return `true` if an Atom was replaced in the Space, `false` otherwise
/// @note The `to` atom will only be added to the Space if the `from` atom is found.  Regardless, this
///    functino will take ownership of the `to` atom.
/// @warning This function takes ownership of the `to` atom, and it should not be freed or accessed
///    after it has been provided to this function
///
#[no_mangle]
pub extern "C" fn space_replace(space: *mut space_t, from: *const atom_ref_t, to: atom_t) -> bool {
    let dyn_space = unsafe{ &*space }.borrow();
    let from = unsafe{ &*from }.borrow();
    dyn_space.borrow_mut().replace(from, to.into_inner())
}

/// @brief Queries a Space for atoms matching a pattern
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to access
/// @param[in]  pattern  A pointer to an `atom_t` or `atom_ref_t` to specify the pattern to match within the Space
/// @return A `bindings_set_t` representing all possible results of the match
/// @note The caller must take ownership responsibility for the returned `bindings_set_t`, and free it with `bindings_set_free()`
///
#[no_mangle]
pub extern "C" fn space_query(space: *const space_t, pattern: *const atom_ref_t) -> bindings_set_t
{
    let dyn_space = unsafe{ &*space }.borrow();
    let pattern = unsafe{ &*pattern }.borrow();
    let results = dyn_space.borrow().query(pattern);
    results.into()
}

/// @brief Substitutes all Atoms matching a pattern with Atoms constructed from a template
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to access
/// @param[in]  pattern  A pointer to an `atom_t` or `atom_ref_t` to match Atom(s) in the Space
/// @param[in]  templ  A pointer to an `atom_t` or `atom_ref_t` to specify a template from which to
///    construct the new atoms
/// @param[in]  callback  A function that will be called to provide access to a vec of newly created atoms
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the
///    `callback` function
///
#[no_mangle]
pub extern "C" fn space_subst(space: *const space_t,
        pattern: *const atom_ref_t, templ: *const atom_ref_t,
        callback: c_atom_vec_callback_t, context: *mut c_void) {
    let dyn_space = unsafe{ &*space }.borrow();
    let pattern = unsafe{ &*pattern }.borrow();
    let templ = unsafe{ &*templ }.borrow();
    let results = dyn_space.borrow().subst(pattern, templ);
    return_atoms(&results, callback, context);
}

/// @brief Returns the number of top-level atoms in a Space, if it can be readily determined
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to access
/// @return The number of top-level atoms in the Space, or -1 for spaces in which it is impossible or
///    impractical to determine the number of atoms
///
#[no_mangle]
pub extern "C" fn space_atom_count(space: *const space_t) -> isize {
    let dyn_space = unsafe{ &*space }.borrow();
    match dyn_space.borrow().atom_count() {
        Some(count) => count as isize,
        None => -1
    }
}

/// @brief Iterates all top-level Atoms in a Space, if that is possible
/// @ingroup space_client_group
/// @param[in]  space  A pointer to the `space_t` handle to access
/// @param[in]  callback  A function that will be called for each top-level Atom in the Space
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the
///    `callback` function
/// @return `true` if the space was sucessfully iterated, or `false` if the space does not support iteration
///
#[no_mangle]
pub extern "C" fn space_iterate(space: *const space_t,
        callback: c_atom_callback_t, context: *mut c_void) -> bool {
    let dyn_space = unsafe{ &*space }.borrow();
    match dyn_space.borrow().atom_iter() {
        Some(atom_iter) => {
            for atom in atom_iter {
                callback(atom.into(), context);
            }
            true
        },
        None => false
    }
}

//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-
// Grounding Space
//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-

/// @brief Creates a new Space, backed by a GroundSpace
/// @ingroup space_client_group
/// @return a `space_t` handle to the newly created Grounding Space
/// @note The caller takes ownership responsibility for the returned `space_t`, and it must be
///    freed with `space_free()`
///
#[no_mangle]
pub extern "C" fn space_new_grounding_space() -> space_t {
    DynSpace::new(GroundingSpace::new()).into()
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Space Observer Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @brief Represents different types of Space Events
/// @ingroup space_observer_group
///
#[repr(C)]
pub enum space_event_type_t {
    /// @brief The event is an `Add` event
    SPACE_EVENT_TYPE_ADD,
    /// @brief The event is a `Remove` event
    SPACE_EVENT_TYPE_REMOVE,
    /// @brief The event is a `Replace` event
    SPACE_EVENT_TYPE_REPLACE,
}

/// @brief Accessor constants, to access the fields of a `space_event_t`
/// @ingroup space_observer_group
///
#[repr(C)]
pub enum space_event_field_t {
    /// @brief Access the atom field of an `Add` event
    SPACE_EVENT_FIELD_ADD,
    /// @brief Access the atom field of a `Remove` event
    SPACE_EVENT_FIELD_REMOVE,
    /// @brief Access the pattern field of a `Replace` event
    SPACE_EVENT_FIELD_REPLACE_PATTERN,
    /// @brief Access the template field of a `Replace` event
    SPACE_EVENT_FIELD_REPLACE_TEMPLATE,
}

/// @struct space_event_t
/// @brief Represents a Space Event
/// @ingroup space_observer_group
/// @note Space Events can be read by observers monitoring a space, or created by custom by custom Space
///    implementations, to communicate activity within the Space to observers
///
#[repr(C)]
pub struct space_event_t {
    /// Internal.  Should not be accessed directly
    event: *mut RustSpaceEvent,
}

struct RustSpaceEvent(SpaceEvent);

impl From<SpaceEvent> for space_event_t {
    fn from(event: SpaceEvent) -> Self {
        Self{ event: Box::into_raw(Box::new(RustSpaceEvent(event))) }
    }
}

impl space_event_t {
    /// WARNING: The output of this function must NOT be passed to into_inner
    pub(crate) fn ref_wrapper(event: &SpaceEvent) -> Self {
        Self{ event: (event as *const SpaceEvent).cast_mut().cast() }
    }
    pub(crate) fn borrow(&self) -> &SpaceEvent {
        &unsafe{ &*self.event }.0
    }
    pub(crate) fn into_inner(self) -> SpaceEvent {
        unsafe{ Box::from_raw(self.event).0 }
    }
}

/// @struct space_observer_api_t
/// @brief A table of callback functions to define the behavior of a SpaceObserver implemented in C
/// @ingroup space_observer_group
/// @see space_register_observer
///
#[repr(C)]
pub struct space_observer_api_t {

    /// @brief Called to pass an event to the observer
    /// @param[in]  payload  The pointer to the observer's payload
    /// @param[in]  event  The event the observer is notified about
    ///
    notify: extern "C" fn(payload: *mut c_void, event: *const space_event_t),

    /// @brief Responsible for freeing the payload passed to `space_register_observer`
    /// @param[in]  payload  The pointer to the observer's payload to free
    /// @note This function is responsible for freeing the payload buffer, as well as any other objects
    ///   and resources owned by the observer.
    free_payload: extern "C" fn(payload: *mut c_void),
}

struct CObserver {
    api: *const space_observer_api_t,
    payload: *mut c_void,
}

impl SpaceObserver for CObserver {
    fn notify(&mut self, event: &SpaceEvent) {
        let api = unsafe{ &*self.api };
        let event = space_event_t::ref_wrapper(event);
        (api.notify)(self.payload, &event);
    }
}

impl Drop for CObserver {
    fn drop(&mut self) {
        let api = unsafe{ &*self.api };
        (api.free_payload)(self.payload);
    }
}

/// @struct space_observer_t
/// @brief Represents a Space Observer, registered with a Space
/// @ingroup space_observer_group
///
#[repr(C)]
pub struct space_observer_t {
    /// Internal.  Should not be accessed directly
    observer: *const RustSpaceObserver
}

struct RustSpaceObserver(std::cell::RefCell<CObserver>);

impl From<SpaceObserverRef<CObserver>> for space_observer_t {
    fn from(observer: SpaceObserverRef<CObserver>) -> Self {
        Self{ observer: std::rc::Rc::into_raw(observer.into_inner()).cast() }
    }
}

impl space_observer_t {
    fn borrow_inner(&self) -> &mut CObserver {
        let cell = unsafe{ &mut *(&(&*self.observer).0 as *const std::cell::RefCell<CObserver>).cast_mut() };
        cell.get_mut()
    }
    fn into_inner(self) -> SpaceObserverRef<CObserver> {
        unsafe{ std::rc::Rc::from_raw(self.observer.cast::<std::cell::RefCell<CObserver>>()).into() }
    }
}

/// @brief Gets the type of a Space Event
/// @ingroup space_observer_group
/// @param[in]  event  A pointer to the event to inspect
/// @return The type of the event
///
#[no_mangle]
pub extern "C" fn space_event_get_type(event: *const space_event_t) -> space_event_type_t {
    let event = unsafe{ &*event }.borrow();
    match event {
        SpaceEvent::Add(_) => space_event_type_t::SPACE_EVENT_TYPE_ADD,
        SpaceEvent::Remove(_) => space_event_type_t::SPACE_EVENT_TYPE_REMOVE,
        SpaceEvent::Replace(_, _) => space_event_type_t::SPACE_EVENT_TYPE_REPLACE,
    }
}

/// @brief Accesses the atom associated with a field of a `space_event_t`
/// @ingroup space_observer_group
/// @param[in]  event  A pointer to the event to access
/// @param[in]  field  A `space_event_field_t` specifying which field to access
/// @return An `atom_ref_t` referencing the specified atom within the event
/// @warning The returned `atom_ref_t` is borrowed from the `space_event_t`, and it must not be modified or
///    accessed after the event has been freed
///
#[no_mangle]
pub extern "C" fn space_event_get_field_atom(event: *const space_event_t, field: space_event_field_t) -> atom_ref_t {
    let event = unsafe{ &*event }.borrow();
    match field {
        space_event_field_t::SPACE_EVENT_FIELD_ADD => {
            if let SpaceEvent::Add(atom) = event {
                atom.into()
            } else {
                panic!("SpaceEvent wasn't an Add event")
            }
        },
        space_event_field_t::SPACE_EVENT_FIELD_REMOVE => {
            if let SpaceEvent::Remove(atom) = event {
                atom.into()
            } else {
                panic!("SpaceEvent wasn't a Remove event")
            }
        },
        space_event_field_t::SPACE_EVENT_FIELD_REPLACE_PATTERN |
        space_event_field_t::SPACE_EVENT_FIELD_REPLACE_TEMPLATE => {
            if let SpaceEvent::Replace(pattern_atom, template_atom) = event {
                match field {
                    space_event_field_t::SPACE_EVENT_FIELD_REPLACE_PATTERN => pattern_atom.into(),
                    space_event_field_t::SPACE_EVENT_FIELD_REPLACE_TEMPLATE => template_atom.into(),
                    _ => unreachable!()
                }
            } else {
                panic!("SpaceEvent wasn't a Replace event")
            }
        }
    }
}

/// @brief Registers a new observer, to monitor activity within the Space
/// @ingroup space_observer_group
/// @param[in]  space  A pointer to the `space_t` handle of the space to observe
/// @param[in]  observer_api  A pointer to the table of functions that implement the observer's behavior
/// @param[in]  observer_payload  A pointer to a caller-defined object usable by the observer's
///    implementation functions
/// @return A `space_observer_t` created to observe the space
/// @note The caller must take ownership responsibility for the returned `space_observer_t`, and it must
///    be freed with `space_observer_free()`
/// @warning This function takes ownership of the `observer_payload`, and it should not be freed after it
///    has been provided to this function
///
#[no_mangle]
pub extern "C" fn space_register_observer(space: *mut space_t, observer_api: *const space_observer_api_t, observer_payload: *mut c_void) -> space_observer_t {
    let dyn_space = unsafe{ &*space }.borrow();
    let space = dyn_space.borrow_mut();
    let observer = CObserver {api: observer_api, payload: observer_payload};
    let observer = space.common().register_observer(observer);
    observer.into()
}

/// @brief Frees a `space_observer_t`
/// @ingroup space_observer_group
/// @param[in]  observer  The `space_observer_t` to free
/// @note Freeing a Space Observer will atomatically de-register it with its associated space
///
#[no_mangle]
pub extern "C" fn space_observer_free(observer: space_observer_t) {
    let observer = observer.into_inner();
    drop(observer);
}

/// @brief Returns a pointer to the payload associated with the space_observer_t
/// @ingroup space_observer_group
/// @param[in]  observer  A pointer to the `space_observer_t` in which to access the payload
/// @return A pointer to the payload object associated with the Space Observer
/// @warning The returned pointer must not be accessed after the `space_observer_t` has been freed,
///    or after any operations have occurred that may have caused events to occur in the associated
///    space
/// @warning The returned pointer should never be freed directly.  Call `space_observer_free()` when
///    you are finished with the observer
///
#[no_mangle]
pub extern "C" fn space_observer_get_payload(observer: *const space_observer_t) -> *mut c_void {
    let c_observer_ref = unsafe{ &*observer }.borrow_inner();
    c_observer_ref.payload
}

//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-
// Space Implementation Interface (Space & SpaceMut trait interface wrapper)
//-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-

/// @struct space_api_t
/// @brief A table of functions to define the behavior of a Space implemented in C
/// @ingroup space_impl_group
/// @see space_new
///
#[repr(C)]
pub struct space_api_t {

    /// @brief Performs a query against atoms in a space
    /// @param[in]  params  A pointer to the space's params
    /// @param[in]  atom  A pointer to the query atom
    /// @return A `bindings_set_t` representing the query results
    /// @see space_query
    ///
    query: extern "C" fn(params: *const space_params_t, atom: *const atom_ref_t) -> bindings_set_t,

    /// @brief Substitutes atoms matched by a query with atoms in a form derived from a template
    /// @param[in]  params  A pointer to the space's params
    /// @param[in]  pattern  A pointer to the the pattern atom to match
    /// @param[in]  tmpl  A pointer to the the template atom from which to construct the substituted atoms
    /// @return An `atom_vec_t` containing all newly created atoms from the substitution
    /// @see space_subst
    /// @note If a subst function is provided, it will be called.  If NULL is provided, the default
    ///    implementation will be called.
    ///
    subst: Option<extern "C" fn(params: *const space_params_t, pattern: *const atom_ref_t, tmpl: *const atom_ref_t) -> atom_vec_t>,

    /// @brief Adds an atom to the space
    /// @param[in]  params  A pointer to the space's params
    /// @param[in]  atom  An `atom_t` representing the atom to add to the space
    /// @see space_add
    /// @note This function must take ownership responsibility for the provided `atom`, and ultimately free
    ///    it when the space is freed
    ///
    add: extern "C" fn(params: *const space_params_t, atom: atom_t),

    /// @brief Removes an atom from the space
    /// @param[in]  params  A pointer to the space's params
    /// @param[in]  atom  A pointer to an `atom_ref_t` or `atom_t` to specify which atom to remove from the space
    /// @return `true` if the atom was removed, otherwise returns `false`
    /// @see space_remove
    /// @warning This function should NOT take ownership of the `atom` argument
    ///
    remove: extern "C" fn(params: *const space_params_t, atom: *const atom_ref_t) -> bool,

    /// @brief Replaces one atom in the space with another
    /// @param[in]  params  A pointer to the space's params
    /// @param[in]  from  A pointer to an `atom_ref_t` or `atom_t` to specify the existing atom to replace in the space
    /// @param[in]  to  An `atom_t` representing the atom to add to the space
    /// @return `true` if the atom was replaced, otherwise returns `false`
    /// @see space_replace
    /// @warning This function should **NOT** take ownership of the `atom` argument, but this function
    ///    **SHOULD** take ownership of the `to` atom
    ///
    replace: extern "C" fn(params: *const space_params_t, from: *const atom_ref_t, to: atom_t) -> bool,

    /// @brief Returns the number of atoms contained within the space
    /// @param[in]  params  A pointer to the space's params
    /// @return The number of top-level atoms contained within the Space
    /// @see space_atom_count
    /// @note If an `atom_count` function is provided, it will be called.  NULL should be provided for spaces
    ///    that cannot readily determine the number of contained atoms
    ///
    atom_count: Option<extern "C" fn(params: *const space_params_t) -> isize>,

    /// @brief Initializes state necessary to perform an iteration over all top-level atoms
    /// @param[in]  params  A pointer to the space's params
    /// @return A pointer to an allocated "iterator state" object storing the necessary information to
    ///    iterate each top-level atom contained within the space
    /// @see space_iterate
    /// @note If an `new_atom_iterator_state` function is provided, it will be called.  NULL should be
    ///    provided for spaces that cannot iterate all top-level atoms in an orderly way
    ///
    new_atom_iterator_state: Option<extern "C" fn(params: *const space_params_t) -> *mut c_void>,

    /// @brief Advances to the next top-level atom in the iteration sequence of the Space
    /// @param[in]  params  A pointer to the space's params
    /// @param[in]  state  A pointer to the iteration state allocated by `new_atom_iterator_state()`
    /// @return A reference to an atom owned by the space, or a `null` `atom_ref_t` to signal the
    ///    iteration has finished
    /// @see space_iterate
    /// @note This function is optional for Spaces that don't support iteration.  However, if a
    ///    `new_atom_iterator_state` implementation is provided then `next_atom` must also be provided
    ///
    next_atom: Option<extern "C" fn(params: *const space_params_t, state: *mut c_void) -> atom_ref_t>,

    /// @brief Frees the iterator state allocated by `new_atom_iterator_state`
    /// @param[in]  params  A pointer to the space's params
    /// @param[in]  state  A pointer to the iteration state allocated by `new_atom_iterator_state()`
    /// @see space_iterate
    /// @note This function is optional for Spaces that don't support iteration.  However, if a
    ///    `new_atom_iterator_state` implementation is provided then `free_atom_iterator_state` must
    ///    also be provided
    ///
    free_atom_iterator_state: Option<extern "C" fn(params: *const space_params_t, state: *mut c_void)>,

    /// @brief Frees the payload buffer passed when the space was created
    /// @param[in]  payload  The pointer to the space's payload
    /// @see space_new
    /// @note This function is responsible for freeing the payload buffer, as well as any other objects
    ///    and resources stored by the space.  This includes freeing all `atom_t` objects and other buffers,
    ///    as well as closing any connections and releasing any other resources held by the Space
    ///
    free_payload: extern "C" fn(payload: *mut c_void),
}

#[derive(Default)]
pub struct RustSpaceCommonData {
    common: SpaceCommon
}

/// @struct space_params_t
/// @brief Data associated with this particular space, including the space's payload and observers
/// @ingroup space_impl_group
///
#[repr(C)]
pub struct space_params_t {
    /// @brief A pointer to the payload passed when the Space was created
    payload: *mut c_void,
    /// @brief Opaque Data maintained by Hyperon
    common: Box<RustSpaceCommonData>,
}

/// @brief Notifies all associated observers of an event
/// @ingroup space_impl_group
/// @param[in]  params  A pointer to the space's params
/// @param[in]  event  A pointer to the event, to broadcast to all observers of the Space
///
#[no_mangle]
pub extern "C" fn space_params_notify_all_observers(params: *const space_params_t, event: *const space_event_t) {
    let common = unsafe{ &(*params).common.common };
    let event = unsafe{ &*event }.borrow();
    common.notify_all_observers(event);
}

/// @brief Creates a new `space_event_t` representing an `Add` event
/// @ingroup space_impl_group
/// @param[in]  atom  The atom that is being added to the Space, to embed into the event
/// @return The newly created `space_event_t`
/// @note The caller must take ownership responsibility for the returned `space_event_t` and it must be freed with `space_event_free()`
/// @warning This function takes ownership of the `atom` parameter, so it must not be subsequently access or freed
///
#[no_mangle]
pub extern "C" fn space_event_new_add(atom: atom_t) -> space_event_t {
    let event = SpaceEvent::Add(atom.into_inner());
    event.into()
}

/// @brief Creates a new `space_event_t` representing a `Remove` event
/// @ingroup space_impl_group
/// @param[in]  atom  The atom that is being removed from the Space, to embed into the event
/// @return The newly created `space_event_t`
/// @note The caller must take ownership responsibility for the returned `space_event_t` and it must be freed with `space_event_free()`
/// @warning This function takes ownership of the `atom` parameter, so it must not be subsequently access or freed
///
#[no_mangle]
pub extern "C" fn space_event_new_remove(atom: atom_t) -> space_event_t {
    let event = SpaceEvent::Remove(atom.into_inner());
    event.into()
}

/// @brief Creates a new `space_event_t` representing a `Replace` event
/// @ingroup space_impl_group
/// @param[in]  pattern  The atom that is being matched in the Space, to embed into the event
/// @param[in]  tmpl  The atom that is being used to construct new atoms in the Space, to embed into the event
/// @return The newly created `space_event_t`
/// @note The caller must take ownership responsibility for the returned `space_event_t` and it must be freed with `space_event_free()`
/// @warning This function takes ownership of both the `pattern` and the `tmpl` parameter, so neither may be subsequently access nor freed
///
#[no_mangle]
pub extern "C" fn space_event_new_replace(pattern: atom_t, tmpl: atom_t) -> space_event_t {
    let event = SpaceEvent::Replace(pattern.into_inner(), tmpl.into_inner());
    event.into()
}

/// @brief Frees a `space_event_t`
/// @ingroup space_impl_group
/// @param[in]  event  The `space_event_t` to free
///
#[no_mangle]
pub extern "C" fn space_event_free(event: space_event_t) {
    let event = event.into_inner();
    drop(event);
}

struct CSpace {
    api: *const space_api_t,
    params: space_params_t,
}

impl CSpace {
    fn new(api: *const space_api_t, payload: *mut c_void) -> Self {
        CSpace{api, params: space_params_t{payload, common: Box::new(RustSpaceCommonData::default())}}
    }
}

impl Space for CSpace {
    fn common(&self) -> FlexRef<SpaceCommon> {
        FlexRef::from_simple(&(*self.params.common).common)
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        let api = unsafe{ &*self.api };
        let query: atom_ref_t = query.into();
        let result_set = (api.query)(&self.params, &query);
        result_set.into_inner()
    }
    fn subst(&self, pattern: &Atom, tmpl: &Atom) -> Vec<Atom> {
        let api = unsafe{ &*self.api };
        if let Some(subst_fn) = api.subst {
            let pattern: atom_ref_t = pattern.into();
            let tmpl: atom_ref_t = tmpl.into();
            let atom_vec = subst_fn(&self.params, &pattern, &tmpl);
            atom_vec.into()
        } else {
            DefaultSpace(self).subst(pattern, tmpl)
        }
    }
    fn atom_count(&self) -> Option<usize> {
        let api = unsafe{ &*self.api };
        if let Some(atom_count_fn) = api.atom_count {
            let count = atom_count_fn(&self.params);
            if count >= 0 {
                Some(count as usize)
            } else {
                None
            }
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
                    let atom_ref = next_atom(&self.0.params, self.1);
                    if atom_ref.is_null() {
                        None
                    } else {
                        Some(atom_ref.into_ref())
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
            if ctx.is_null() {
                None
            } else {
                let new_iter = CSpaceIterator(self, ctx);
                Some(SpaceIter::new(new_iter))
            }
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
    fn common(&self) -> FlexRef<SpaceCommon> { self.0.common() }
    fn query(&self, query: &Atom) -> BindingsSet { self.0.query(query) }
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
        (api.add)(&self.params, atom.into());
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        let api = unsafe{ &*self.api };
        let atom: atom_ref_t = atom.into();
        (api.remove)(&self.params, &atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        let api = unsafe{ &*self.api };
        let from: atom_ref_t = from.into();
        (api.replace)(&self.params, &from, to.into())
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
