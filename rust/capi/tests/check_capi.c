#include <check.h>
#include <hyperon.h>

#include "int_gnd.h"

char buffer[4096];
char const* _atom_to_str(atom_t const* atom) {
	atom_to_str(atom, buffer, sizeof(buffer)/sizeof(buffer[0]));
	return buffer;
}

void setup(void)
{
}

void teardown(void)
{
}

START_TEST (test_sym)
{
	char name[] = "test";
	atom_t* atom = atom_sym(name);
	name[0] = 'r';
	
	ck_assert_str_eq(_atom_to_str(atom), "test");

	free_atom(atom);
}
END_TEST

START_TEST (test_expr)
{
	atom_t* expr[] = {atom_sym("test"), atom_var("var"), atom_sym("five"), atom_gnd(int_new(42))};
	int size = sizeof(expr)/sizeof(expr[0]);
	
	atom_t* atom = atom_expr(expr, size);
	ck_assert_str_eq(_atom_to_str(atom), "(test $var five 42)");

	free_atom(atom);
	for (int i = 0; i < size; ++i) {
		free_atom(expr[i]);
	}
}
END_TEST

Suite * capi_suite(void)
{
    Suite *s;
    TCase *tc_core;
    TCase *tc_limits;

    s = suite_create("C API");

    tc_core = tcase_create("Core");

    tcase_add_checked_fixture(tc_core, setup, teardown);
    tcase_add_test(tc_core, test_sym);
    tcase_add_test(tc_core, test_expr);
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

