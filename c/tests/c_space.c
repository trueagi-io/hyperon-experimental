
#include <stdio.h>
#include <strings.h>

#include "util.h"
#include "c_space.h"

typedef struct _observer_list_item {
    space_observer_t* observer;
    struct _observer_list_item* next;
} observer_list_item;

void notify_all_observers(observer_list_item* list, space_event_t* event) {
    observer_list_item* cur_observer_item = list;
    while (cur_observer_item != NULL) {
        space_observer_notify(cur_observer_item->observer, event);
        cur_observer_item = cur_observer_item->next;
    }
}

typedef struct _atom_list_item {
    atom_t* atom;
    struct _atom_list_item* next;
} atom_list_item;

void register_observer(void* space_ptr, space_observer_t* observer) {
    custom_space_buf* space = space_ptr;
    observer_list_item* new_item = malloc(sizeof(observer_list_item));
    new_item->observer = observer;
    new_item->next = NULL;
    if (space->observers == NULL) {
        space->observers = new_item;
    } else {
        observer_list_item* cur_item = space->observers;
        while (cur_item->next != NULL) {cur_item = cur_item->next;}
        cur_item->next = new_item;
    }
}

void collect_variable_atoms(const atom_t* atom, void* vec_ptr) {
    vec_atom_t* vec = vec_ptr;
    if (atom_get_type(atom) == VARIABLE) {
        vec_atom_push(vec, atom_clone(atom));
    }
}

void narrow_vars_callback(bindings_t* bindings, void* vars_vec_ptr) {
    vec_atom_t* vars = vars_vec_ptr;
    bindings_narrow_vars(bindings, vars);
}

// NOTE: this is a naive implementation barely good enough to pass the tests
// Don't take this as a guide to implementing a space query function
bindings_set_t* query(void* space_ptr, const atom_t* query_atom) {
    custom_space_buf* space = space_ptr;

    vec_atom_t* query_vars = vec_atom_new();
    atom_iterate(query_atom, collect_variable_atoms, query_vars);

    bindings_set_t* new_bindings_set = bindings_set_single();

    atom_list_item* cur_atom_item = space->atoms;
    while (cur_atom_item != NULL) {
        bindings_set_t* match_results = atom_match_atom(cur_atom_item->atom, query_atom);
        bindings_set_iterate(match_results, narrow_vars_callback, query_vars);
        bindings_set_merge_into(new_bindings_set, match_results);
        cur_atom_item = cur_atom_item->next;
    }

    vec_atom_free(query_vars);
    return new_bindings_set;
}

/// adds an atom to a cspace without calling the observers
void add_atom_internal(custom_space_buf* space, atom_t* atom) {
    atom_list_item* new_item = malloc(sizeof(atom_list_item));
    new_item->atom = atom;
    new_item->next = NULL;
    if (space->atoms == NULL) {
        space->atoms = new_item;
    } else {
        atom_list_item* cur_item = space->atoms;
        while (cur_item->next != NULL) {cur_item = cur_item->next;}
        cur_item->next = new_item;
    }

    space->atom_count += 1;
}

void add_atom(void* space_ptr, atom_t* atom) {
    custom_space_buf* space = space_ptr;

    space_event_t* event = space_event_new_add(atom_clone(atom));
    notify_all_observers(space->observers, event);
    space_event_free(event);

    add_atom_internal(space, atom);
}

/// adds an atom to a cspace without calling the observers
bool remove_atom_internal(custom_space_buf* space, const atom_t* atom) {

    //unLink the new atom into our space and free it, if we find it
    atom_list_item* cur_atom_item = space->atoms;
    atom_list_item** prev_item_ptr = &(space->atoms);
    while (cur_atom_item != NULL) {
        if (atom_eq(cur_atom_item->atom, atom)) {
            *prev_item_ptr = cur_atom_item->next;
            atom_free(cur_atom_item->atom);
            free(cur_atom_item);

            space->atom_count -= 1;

            return true;
        } else {
            prev_item_ptr = &(cur_atom_item->next);
            cur_atom_item = cur_atom_item->next;
        }
    }

    //We never found the atom
    return false;
}

bool remove_atom(void* space_ptr, const atom_t* atom) {
    custom_space_buf* space = space_ptr;

    if (remove_atom_internal(space, atom)) {
        space_event_t* event = space_event_new_remove(atom_clone(atom));
        notify_all_observers(space->observers, event);
        space_event_free(event);
        return true;
    }

    return false;
}

bool replace_atom(void* space_ptr, const atom_t* from, atom_t* to) {
    custom_space_buf* space = space_ptr;

    if (remove_atom_internal(space, from)) {
        add_atom_internal(space, to);
        space_event_t* event = space_event_new_replace(atom_clone(from), atom_clone(to));
        notify_all_observers(space->observers, event);
        space_event_free(event);
        return true;
    } else {
        atom_free(to);
        return false;
    }
}

void free_payload(void* space_ptr) {
    custom_space_buf* space = space_ptr;

    atom_list_item* cur_atom_item = space->atoms;
    while (cur_atom_item != NULL) {
        atom_free(cur_atom_item->atom);
        atom_list_item* next_item = cur_atom_item->next;
        free(cur_atom_item);
        cur_atom_item = next_item;
    }

    observer_list_item* cur_observer_item = space->observers;
    while (cur_observer_item != NULL) {
        space_observer_free(cur_observer_item->observer);
        observer_list_item* next_item = cur_observer_item->next;
        free(cur_observer_item);
        cur_observer_item = next_item;
    }

    free(space);
}

// Space API Declaration to pass to space_new()
static space_api_t const C_SPACE_API= {
    .register_observer = &register_observer,
    .query = &query,
    .subst = NULL,
    .add = &add_atom,
    .remove = &remove_atom,
    .replace = &replace_atom,
    .free_payload = &free_payload
};

space_t* custom_space_new() {

    custom_space_buf* c_space = malloc(sizeof(custom_space_buf));
    c_space->observers = NULL;
    c_space->atoms = NULL;
    c_space->atom_count = 0;
    space_t* space = space_new(&C_SPACE_API, c_space);

    return space;
}