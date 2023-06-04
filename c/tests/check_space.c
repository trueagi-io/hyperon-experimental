#include <stdio.h>
#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"

#include "c_space.h"

void setup(void) {
}

void teardown(void) {
}

struct output_t {
    char str[1024];
    char len;
};

void reset_output(struct output_t* output) {
    output->str[0] = 0;
    output->len = 0;
}

void atom_string_callback(const atom_t* atom, void* data)
{
    struct output_t* out = data;
    out->len += atom_to_str(atom, out->str + out->len, 1024 - out->len);
    out->len += snprintf(out->str + out->len, 1024 - out->len, ", ");
}

void query_callback_single_atom(const struct var_atom_t* atom, void* data)
{
    struct output_t* out = data;
    out->len += snprintf(out->str + out->len, 1024 - out->len, "%s: ", atom->var);
    atom_string_callback(atom->atom, out);
    atom_free(atom->atom);
}

void query_callback(struct bindings_t const* results, void* data)
{
    struct output_t* out = data;

    bindings_traverse(results, query_callback_single_atom, out);
}

void collect_atoms(const atom_t* atom, void* vec_ptr) {
    vec_atom_t* vec = vec_ptr;
    vec_atom_push(vec, atom_clone(atom));
}

START_TEST (test_grounding_space_query)
{
    space_t* space = space_new_grounding_space();;
    space_add(space, expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0));
    atom_t* query = expr(atom_sym("+"), atom_sym("A"), atom_var("b"), 0);

    struct output_t result = { "", 0 };
    space_query(space, query, query_callback, &result);
    ck_assert_str_eq(result.str, "b: B, ");

    space_free(space);
}
END_TEST

START_TEST (test_grounding_space_add)
{
    space_t* space = space_new_grounding_space();;
    atom_t* atom = expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0);

    space_add(space, atom_clone(atom));

    ck_assert_int_eq(space_atom_count(space), 1);

    vec_atom_t* atoms = vec_atom_new();
    space_iterate(space, collect_atoms, atoms);
    ck_assert(atom_eq(vec_atom_get(atoms, 0), atom));

    vec_atom_free(atoms);
    atom_free(atom);
    space_free(space);
}
END_TEST

START_TEST (test_grounding_space_remove)
{
    space_t* space = space_new_grounding_space();;
    atom_t* atom = expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0);
    space_add(space, atom_clone(atom));

    space_remove(space, atom);

    ck_assert_int_eq(space_atom_count(space), 0);

    atom_free(atom);
    space_free(space);
}
END_TEST

START_TEST (test_grounding_space_replace)
{
    space_t* space = space_new_grounding_space();;
    atom_t* atom1 = expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0);
    atom_t* atom2 = expr(atom_sym("+"), atom_var("b"), atom_sym("A"), 0);
    space_add(space, atom_clone(atom1));

    space_replace(space, atom1, atom_clone(atom2));

    ck_assert_int_eq(space_atom_count(space), 1);

    vec_atom_t* atoms = vec_atom_new();
    space_iterate(space, collect_atoms, atoms);
    ck_assert(atom_eq(vec_atom_get(atoms, 0), atom2));

    vec_atom_free(atoms);
    atom_free(atom1);
    atom_free(atom2);
    space_free(space);
}
END_TEST

typedef struct _my_observer {
    size_t      atom_count;
} my_observer_t;

void observer_notify(void* payload, const space_event_t* event) {
    my_observer_t* observer = payload;
    switch (space_event_get_type(event)) {
        case SPACE_EVENT_TYPE_ADD:
            observer->atom_count++;
            break;
        case SPACE_EVENT_TYPE_REMOVE:
            observer->atom_count--;
            break;
        case SPACE_EVENT_TYPE_REPLACE:
            break;
    }
}

void observer_free_payload(void* payload) {
    free(payload);
}

static space_observer_api_t const C_OBSERVER_API= {
    .notify = &observer_notify,
    .free_payload = &observer_free_payload
};

START_TEST (test_custom_c_space)
{
    space_t* space = custom_space_new();

    my_observer_t* observer_payload = malloc(sizeof(my_observer_t));
    observer_payload->atom_count = 0;
    space_observer_t* observer = space_register_observer(space, &C_OBSERVER_API, observer_payload);

    space_observer_t* observer_2 = space_register_observer(space, &C_OBSERVER_API, malloc(sizeof(my_observer_t)));

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

    //Test that dropping an observer here doesn't cause problems with the other observer
    space_observer_free(observer_2);

    space_add(space, expr(atom_sym("+"), atom_var("a"), atom_sym("B"), 0));
    atom_t* query = expr(atom_sym("+"), atom_sym("A"), atom_var("b"), 0);
    struct output_t result = { "", 0 };
    space_query(space, query, query_callback, &result);
    ck_assert_str_eq(result.str, "b: B, ");

    //Test that we can iterate the atoms in the space
    reset_output(&result);
    space_iterate(space, atom_string_callback, &result);
    ck_assert_str_eq(result.str, "(+ $a B), ");

    ck_assert_int_eq(space_atom_count(space), 1);

    custom_space_buf* c_space_buf = space_get_payload(space);
    ck_assert(c_space_buf->atom_count == 1);

    my_observer_t* payload_ptr = space_observer_get_payload(observer);
    ck_assert(payload_ptr->atom_count == 1);

    space_observer_free(observer);
    atom_free(query);
    space_free(space);
}

void init_test(TCase* test_case) {
    tcase_set_timeout(test_case, 300); //300s = 5min.  To test for memory leaks
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_grounding_space_query);
    tcase_add_test(test_case, test_grounding_space_add);
    tcase_add_test(test_case, test_grounding_space_remove);
    tcase_add_test(test_case, test_grounding_space_replace);
    tcase_add_test(test_case, test_custom_c_space);
}

TEST_MAIN(init_test);
