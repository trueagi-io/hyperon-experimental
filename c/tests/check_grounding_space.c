#include <stdio.h>
#include <check.h>
#include <hyperon/hyperon.h>

#include "util.h"

void setup(void)
{
}

void teardown(void)
{
}

struct output_t {
	char str[1024];
	char len;
};

void copy_to_output(char const* str, void* context) {
	struct output_t *output = context;
	output->len += snprintf(output->str + output->len, 1024 - output->len, "%s, ", str);
}

void query_callback(binding_t const* results, uintptr_t size, void* data) {
	struct output_t *output = data;
	char atom_str[1024];
	for (int i = 0; i < size; ++i) {
		binding_t const* result = results + i;
		output->len += snprintf(output->str + output->len, 1024 - output->len, "%s: ", results->var);
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

	grounding_space_add(space, atom_copy(atom));

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
	grounding_space_add(space, atom_copy(atom));

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
	grounding_space_add(space, atom_copy(atom1));

	grounding_space_replace(space, atom1, atom_copy(atom2));

	ck_assert_int_eq(grounding_space_len(space), 1);
	ck_assert(atom_eq(grounding_space_get(space, 0), atom2));

	atom_free(atom1);
	atom_free(atom2);
	grounding_space_free(space);
}
END_TEST

Suite * capi_suite(void)
{
    Suite *s;
    TCase *tc_core;
    TCase *tc_limits;

    s = suite_create("Suite");

    tc_core = tcase_create("Core");

    tcase_add_checked_fixture(tc_core, setup, teardown);
    tcase_add_test(tc_core, test_query);
    tcase_add_test(tc_core, test_add);
    tcase_add_test(tc_core, test_remove);
    tcase_add_test(tc_core, test_replace);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void) {
	int number_failed;
    Suite *s;
    SRunner *sr;

    s = capi_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}

