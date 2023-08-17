
#include <stdio.h>
#include <strings.h>

#include "util.h"
#include "c_space.h"

typedef struct _atom_list_item {
    atom_t atom;
    struct _atom_list_item* next;
} atom_list_item;

void collect_variable_atoms(atom_ref_t atom, void* vec_ptr) {
    atom_vec_t* vec = vec_ptr;
    if (atom_get_type(&atom) == VARIABLE) {
        atom_vec_push(vec, atom_clone(&atom));
    }
}

void narrow_vars_callback(bindings_t* bindings, void* vars_vec_ptr) {
    atom_vec_t* vars = vars_vec_ptr;
    bindings_narrow_vars(bindings, vars);
}

// NOTE: this is a naive implementation barely good enough to pass the tests
// Don't take this as a guide to implementing a space query function
bindings_set_t query(const space_params_t* params, const atom_t* query_atom) {
    custom_space_buf* space = params->payload;

    atom_vec_t query_vars = atom_vec_new();
    atom_iterate(query_atom, collect_variable_atoms, &query_vars);

    bindings_set_t new_bindings_set = bindings_set_single();

    atom_list_item* cur_atom_item = space->atoms;
    while (cur_atom_item != NULL) {
        bindings_set_t match_results = atom_match_atom(&cur_atom_item->atom, query_atom);
        bindings_set_iterate(&match_results, narrow_vars_callback, &query_vars);
        bindings_set_merge_into(&new_bindings_set, &match_results);
        bindings_set_free(match_results);
        cur_atom_item = cur_atom_item->next;
    }

    atom_vec_free(query_vars);
    return new_bindings_set;
}

/// adds an atom to a cspace without calling the observers
void add_atom_internal(custom_space_buf* space, atom_t atom) {
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

void add_atom(const space_params_t* params, atom_t atom) {
    custom_space_buf* space = params->payload;

    space_event_t event = space_event_new_add(atom_clone(&atom));
    space_params_notify_all_observers(params, &event);
    space_event_free(event);

    add_atom_internal(space, atom);
}

/// adds an atom to a cspace without calling the observers
bool remove_atom_internal(custom_space_buf* space, const atom_t* atom) {

    //unLink the new atom into our space and free it, if we find it
    atom_list_item* cur_atom_item = space->atoms;
    atom_list_item** prev_item_ptr = &(space->atoms);
    while (cur_atom_item != NULL) {
        if (atom_eq(&cur_atom_item->atom, atom)) {
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

bool remove_atom(const space_params_t* params, const atom_t* atom) {
    custom_space_buf* space = params->payload;

    if (remove_atom_internal(space, atom)) {
        space_event_t event = space_event_new_remove(atom_clone(atom));
        space_params_notify_all_observers(params, &event);
        space_event_free(event);
        return true;
    }

    return false;
}

bool replace_atom(const space_params_t* params, const atom_t* from, atom_t to) {
    custom_space_buf* space = params->payload;

    if (remove_atom_internal(space, from)) {
        add_atom_internal(space, to);
        space_event_t event = space_event_new_replace(atom_clone(from), atom_clone(&to));
        space_params_notify_all_observers(params, &event);
        space_event_free(event);
        return true;
    } else {
        atom_free(to);
        return false;
    }
}

ssize_t atom_count(const space_params_t* params) {
    custom_space_buf* space = params->payload;
    return space->atom_count;
}

void* new_atom_iterator_state(const space_params_t* params) {
    custom_space_buf* space = params->payload;
    atom_list_item** cur_item_ptr = malloc(sizeof(atom_list_item*));
    *cur_item_ptr = space->atoms;
    return cur_item_ptr;
}

atom_ref_t next_atom(const space_params_t* params, void* iterator_state) {
    atom_list_item** cur_item_ptr = iterator_state;
    if (*cur_item_ptr != NULL) {
        atom_ref_t atom_to_return = atom_ref(&(*cur_item_ptr)->atom);
        *cur_item_ptr = (*cur_item_ptr)->next;
        return atom_to_return;
    } else {
        return atom_ref_null();
    }
}

void free_atom_iterator_state(const space_params_t* params, void* iterator_state) {
    free(iterator_state);
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

    free(space);
}

// Space API Declaration to pass to space_new()
static space_api_t const C_SPACE_API= {
    .query = &query,
    .subst = NULL,
    .add = &add_atom,
    .remove = &remove_atom,
    .replace = &replace_atom,
    .atom_count = &atom_count,
    .new_atom_iterator_state = &new_atom_iterator_state,
    .next_atom = &next_atom,
    .free_atom_iterator_state = &free_atom_iterator_state,
    .free_payload = &free_payload
};

space_t custom_space_new() {

    custom_space_buf* c_space = malloc(sizeof(custom_space_buf));
    c_space->atoms = NULL;
    c_space->atom_count = 0;
    space_t space = space_new(&C_SPACE_API, c_space);

    return space;
}
