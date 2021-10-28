#include <check.h>
#include <hyperon.h>

#include "util.h"
#include "int_gnd.h"

void setup(void)
{
}

void teardown(void)
{
}

START_TEST (test_parsing_expr)
{
	sexpr_space_t* text = sexpr_space_new();
	grounding_space_t* space = grounding_space_new();

	sexpr_space_register_token(text, "\\d+", int_atom_from_str);
	ck_assert(sexpr_space_add_str(text, "(= (fac $n) (* $n (fac (- $n 1))))"));
	sexpr_space_into_grounding_space(text, space);

	ck_assert(1 == grounding_space_len(space));
	ck_assert(atom_eq(grounding_space_get(space, 0),
				expr(atom_sym("="), expr(atom_sym("fac"), atom_var("n"), 0),
					expr(atom_sym("*"), atom_var("n"),
						expr(atom_sym("fac"),
							expr(atom_sym("-"), atom_var("n"), atom_gnd(int_new(1)), 0),
							0),
						0),
					0)));

	grounding_space_free(space);
	sexpr_space_free(text);
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
    tcase_add_test(tc_core, test_parsing_expr);
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

