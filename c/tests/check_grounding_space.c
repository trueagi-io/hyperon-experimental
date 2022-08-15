#include <stdio.h>
#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"

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

void query_callback(binding_array_t results, void* data) {
    struct output_t *output = data;
    for (int i = 0; i < results.size; ++i) {
        binding_t const* result = results.items + i;
        output->len += snprintf(output->str + output->len, 1024 - output->len, "%s: ", results.items->var);
        atom_to_str(result->atom, copy_to_output, output);
    }
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

void init_test(TCase* test_case) {
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_query);
    tcase_add_test(test_case, test_add);
    tcase_add_test(test_case, test_remove);
    tcase_add_test(test_case, test_replace);
}

TEST_MAIN(init_test);
