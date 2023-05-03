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

// QUESTION FOR VITALY: What's the simplest way to test the observer mechanism?  Obviously I could
// add my own observer, but I assume there is a better in-situ test that is possible?

START_TEST (test_custom_c_space)
{
    space_t* space = custom_space_new();

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
