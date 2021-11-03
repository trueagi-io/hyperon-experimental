#include <check.h>
#include <hyperon/hyperon.h>

#include "util.h"
#include "int_gnd.h"

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
	
	ck_assert_str_eq(stratom(atom), "test");

	atom_free(atom);
}
END_TEST

START_TEST (test_expr)
{
	atom_t* atom = expr(atom_sym("test"), atom_var("var"), atom_sym("five"), atom_gnd(int_new(42)), 0);

	ck_assert_str_eq(stratom(atom), "(test $var five 42)");

	atom_free(atom);
}
END_TEST

START_TEST (test_to_str)
{
	char str[5];
	atom_t* atom = atom_sym("test");

	ck_assert_uint_eq(atom_to_str(atom, 0, 0), 5);
	atom_to_str(atom, str, 5);
	ck_assert_str_eq(str, "test");
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
    tcase_add_test(tc_core, test_sym);
    tcase_add_test(tc_core, test_expr);
    tcase_add_test(tc_core, test_to_str);
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

