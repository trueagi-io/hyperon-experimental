#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"
#include "int_gnd.h"

void setup(void) {
}

void teardown(void) {
}

START_TEST (test_parsing_expr)
{
	sexpr_space_t* text = sexpr_space_new();
	grounding_space_t* space = grounding_space_new();

	droppable_t empty_context = { 0, 0 };
	sexpr_space_register_token(text, "\\d+", int_atom_from_str, empty_context);
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

void init_test(TCase* test_case) {
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_parsing_expr);
}

TEST_MAIN(init_test);
