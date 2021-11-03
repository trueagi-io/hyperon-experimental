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

void query_callback(binding_t const* results, uintptr_t size, void* data) {
	struct output_t *output = data;
	char atom_str[1024];
	for (int i = 0; i < size; ++i) {
		binding_t const* result = results + i;
		atom_to_str(result->atom, atom_str, 1024);
		output->len += snprintf(output->str + output->len, 1024 - output->len, "%s: %s, ", results->var, atom_str);
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

Suite * capi_suite(void)
{
    Suite *s;
    TCase *tc_core;
    TCase *tc_limits;

    s = suite_create("Suite");

    tc_core = tcase_create("Core");

    tcase_add_checked_fixture(tc_core, setup, teardown);
    tcase_add_test(tc_core, test_query);
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

