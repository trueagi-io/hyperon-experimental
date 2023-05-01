#include <stdio.h>
#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"

//QUESTION FOR VITALY: Should we rename this file "check_space.c"?  That is a more accurate reflection
// of the new function, but ultimately a potential disruption for a purely cosmetic reason

#define BUF_SIZE 4096

void setup(void) {
}

void teardown(void) {
}

struct output_t {
    char str[1024];
    char len;
};

void copy_to_output(char const* str, void* context) {
    struct output_t *output = context;
    output->len += snprintf(output->str + output->len, 1024 - output->len, "%s, ", str);
}

void query_callback_single_atom(const struct var_atom_t* atom, void* data)
{
    struct output_t* out = data;

    out->len += snprintf(out->str + out->len, 1024 - out->len, "%s: ", atom->var);
    atom_to_str(atom->atom, copy_to_output, out);
    atom_free(atom->atom);
}

void query_callback(struct bindings_t const* results, void* data)
{
    struct output_t* out = data;

    bindings_traverse(results, query_callback_single_atom, out);
}

START_TEST (test_query)
{
    grounding_space_t* space = grounding_space_new();
    grounding_space_add(space, expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0));
    atom_t* query = expr(atom_sym("+"), atom_sym("A"), atom_var("b"), 0);

    struct output_t result = { "", 0 };
    grounding_space_query(space, query, query_callback, &result);
    ck_assert_str_eq(result.str, "b: B, ");

    grounding_space_free(space);
}
END_TEST

START_TEST (test_add)
{
    grounding_space_t* space = grounding_space_new();
    atom_t* atom = expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0);

    grounding_space_add(space, atom_clone(atom));

    ck_assert_int_eq(grounding_space_len(space), 1);
    ck_assert(atom_eq(grounding_space_get(space, 0), atom));

    atom_free(atom);
    grounding_space_free(space);
}
END_TEST

START_TEST (test_remove)
{
    grounding_space_t* space = grounding_space_new();
    atom_t* atom = expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0);
    grounding_space_add(space, atom_clone(atom));

    grounding_space_remove(space, atom);

    ck_assert_int_eq(grounding_space_len(space), 0);

    atom_free(atom);
    grounding_space_free(space);
}
END_TEST

START_TEST (test_replace)
{
    grounding_space_t* space = grounding_space_new();
    atom_t* atom1 = expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0);
    atom_t* atom2 = expr(atom_sym("+"), atom_var("b"), atom_sym("A"), 0);
    grounding_space_add(space, atom_clone(atom1));

    grounding_space_replace(space, atom1, atom_clone(atom2));

    ck_assert_int_eq(grounding_space_len(space), 1);
    ck_assert(atom_eq(grounding_space_get(space, 0), atom2));

    atom_free(atom1);
    atom_free(atom2);
    grounding_space_free(space);
}
END_TEST

START_TEST (test_abstract_space_grounding)
{
    space_t* space = space_new_grounding_space();
    space_add(space, expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0));
    atom_t* query = expr(atom_sym("+"), atom_sym("A"), atom_var("b"), 0);

    struct output_t result = { "", 0 };
    space_query(space, query, query_callback, &result);
    ck_assert_str_eq(result.str, "b: B, ");

    space_free(space);
}
END_TEST

void str_to_buf(const char *str, void *context) {
    strncpy(context, str, BUF_SIZE);
};

// QUESTION FOR VITALY: What's the simplest way to test the observer mechanism?  Obviously I could
// add my own observer, but I assume there is a better in-situ test that is possible?
typedef struct _observer_list_item {
    space_observer_t* observer;
    struct _observer_list_item* next;
} observer_list_item;

void notify_all_observers(observer_list_item* list, space_event_t* event) {
    observer_list_item* cur_observer_item = list;
    while (cur_observer_item != NULL) {
        space_observer_notify(cur_observer_item->observer, event);
        observer_list_item* next_item = cur_observer_item->next;
    }
}

typedef struct _atom_list_item {
    atom_t* atom;
    struct _atom_list_item* next;
} atom_list_item;

typedef struct _custom_space_buf {
    observer_list_item* observers;
    atom_list_item* atoms;
    size_t atom_count;
} custom_space_buf;

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

    //QUESTION FOR VITALY: Not sure if it's right to notify on all removes, or just on
    // sucessful removes.  Assuming all for now
    space_event_t* event = space_event_new_remove(atom_clone(atom));
    notify_all_observers(space->observers, event);
    space_event_free(event);

    return remove_atom_internal(space, atom);
}

bool replace_atom(void* space_ptr, const atom_t* from, atom_t* to) {
    custom_space_buf* space = space_ptr;

    //QUESTION FOR VITALY: Not sure if it's right to notify on all replaces, or just on
    // sucessful ones.  Assuming all for now
    space_event_t* event = space_event_new_replace(atom_clone(from), atom_clone(to));
    notify_all_observers(space->observers, event);
    space_event_free(event);

    if (remove_atom_internal(space, from)) {
        add_atom_internal(space, to);
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

START_TEST (test_custom_c_space)
{
    space_api_t const C_SPACE_API= { 
        .register_observer = &register_observer,
        .query = &query,
        .subst = NULL,
        .add = &add_atom,
        .remove = &remove_atom,
        .replace = &replace_atom,
        .free_payload = &free_payload
    };
    custom_space_buf* c_space = malloc(sizeof(custom_space_buf));
    c_space->observers = NULL;
    c_space->atoms = NULL;
    c_space->atom_count = 0;
    space_t* space = space_new(&C_SPACE_API, c_space);

    atom_t* a = atom_sym("A");
    atom_t* b = atom_sym("B");
    atom_t* c = atom_sym("C");
    space_add(space, atom_sym("A"));
    space_add(space, atom_sym("B"));
    space_add(space, atom_sym("C"));
    ck_assert(space_remove(space, a));
    ck_assert(!space_remove(space, a));
    ck_assert(space_remove(space, c));
    ck_assert(space_remove(space, b));
    space_add(space, atom_sym("A"));
    ck_assert(space_replace(space, a, atom_sym("B")));
    ck_assert(!space_replace(space, a, atom_sym("Junk")));
    ck_assert(space_remove(space, b));
    atom_free(a);
    atom_free(b);
    atom_free(c);

    space_add(space, expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0));
    atom_t* query = expr(atom_sym("+"), atom_sym("A"), atom_var("b"), 0);
    struct output_t result = { "", 0 };
    space_query(space, query, query_callback, &result);
    ck_assert_str_eq(result.str, "b: B, ");

    custom_space_buf* c_space_buf = space_get_payload(space);
    ck_assert(c_space_buf->atom_count == 1);

    atom_free(query);
    space_free(space);
}

void init_test(TCase* test_case) {
    tcase_set_timeout(test_case, 300); //300s = 5min.  To test for memory leaks
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_query);
    tcase_add_test(test_case, test_add);
    tcase_add_test(test_case, test_remove);
    tcase_add_test(test_case, test_replace);
    tcase_add_test(test_case, test_abstract_space_grounding);
    tcase_add_test(test_case, test_custom_c_space);
}

TEST_MAIN(init_test);
