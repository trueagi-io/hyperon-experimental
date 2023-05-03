
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

void collect_child_atoms(atom_array_t atoms, void* vec_ptr) {
    vec_atom_t* vec = vec_ptr;
    for (int i=0; i<atoms.size; i++) {
        vec_atom_push(vec, atom_clone(atoms.items[i]));
    }
}

void atom_match(const atom_t* q, const atom_t* s, bindings_t* bindings) {

    //Assume both atoms are expression atoms
    vec_atom_t* q_vec = vec_atom_new();
    vec_atom_t* s_vec = vec_atom_new();
    atom_get_children(q, &collect_child_atoms, q_vec);
    atom_get_children(s, &collect_child_atoms, s_vec);

    if (vec_atom_size(q_vec) == vec_atom_size(s_vec)) {

        bool is_match = true;
        for (int i=0; i<vec_atom_size(q_vec); i++) {
            atom_t* q_atom = vec_atom_get(q_vec, i);
            atom_t* s_atom = vec_atom_get(s_vec, i);
            if (atom_get_type(q_atom) != VARIABLE &&
                atom_get_type(s_atom) != VARIABLE &&
                !atom_eq(q_atom, s_atom)) {
                is_match = false;
                break;
            }
            atom_free(q_atom);
            atom_free(s_atom);
        }

        if (is_match) {
            for (int i=0; i<vec_atom_size(q_vec); i++) {
                atom_t* q_atom = vec_atom_get(q_vec, i);
                atom_t* s_atom = vec_atom_get(s_vec, i);
                if (atom_get_type(q_atom) == VARIABLE && atom_get_type(s_atom) != VARIABLE) {
                    char str_buf[BUF_SIZE];
                    atom_get_name(q_atom, &str_to_buf, &str_buf);
                    var_atom_t binding = {.var = (char*)&str_buf, .atom = atom_clone(s_atom)};
                    bindings_add_var_binding(bindings, &binding);
                }
                atom_free(q_atom);
                atom_free(s_atom);
            }
        }
    }

    vec_atom_free(q_vec);
    vec_atom_free(s_vec);
}

// NOTE: this is a naive implementation barely good enough to pass the tests
// Don't take this as a guide to implementing a space query function
bindings_set_t* query(void* space_ptr, const atom_t* query_atom) {
    custom_space_buf* space = space_ptr;

    bindings_t* new_bindings = bindings_new();

    atom_list_item* cur_atom_item = space->atoms;
    while (cur_atom_item != NULL) {
        atom_match(query_atom, cur_atom_item->atom, new_bindings);
        cur_atom_item = cur_atom_item->next;
    }

    return bindings_set_from_bindings(new_bindings);
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