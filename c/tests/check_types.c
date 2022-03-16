#include <check.h>
#include <hyperon/hyperon.h>

#include "util.h"
#include "int_gnd.h"

void setup(void) {
	init_logger();
}

void teardown(void) {
}

START_TEST (test_check_type)
{
	grounding_space_t* space = grounding_space_new();
	grounding_space_add(space, expr(atom_sym(":"), atom_sym("do"), atom_sym("Verb"), 0));
    atom_type_t* verb = atom_type_specific(atom_sym("Verb"));

    atom_t* nonsense = atom_sym("nonsense");
    ck_assert(check_type(space, nonsense, ATOM_TYPE_UNDEFINED));
    ck_assert(!check_type(space, nonsense, verb));
    atom_free(nonsense);

    atom_type_free(verb);
}
END_TEST

START_TEST (test_validate_expr)
{
	grounding_space_t* space = grounding_space_new();
	grounding_space_add(space, expr(atom_sym(":"), atom_sym("a"), atom_sym("A"), 0));
	grounding_space_add(space, expr(atom_sym(":"), atom_sym("b"), atom_sym("B"), 0));
	grounding_space_add(space, expr(atom_sym(":"), atom_sym("foo"), expr(atom_sym("->"), atom_sym("A"), atom_sym("B"), 0), 0));

    atom_t* foo = expr(atom_sym("foo"), atom_sym("a"), 0);
    ck_assert(validate_expr(space, foo));
    atom_free(foo);
}
END_TEST

Suite * capi_suite(void) {
    Suite *s;
    TCase *tc_core;
    TCase *tc_limits;

    s = suite_create("Suite");

    tc_core = tcase_create("Core");

    tcase_add_checked_fixture(tc_core, setup, teardown);
    tcase_add_test(tc_core, test_check_type);
    tcase_add_test(tc_core, test_validate_expr);
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

