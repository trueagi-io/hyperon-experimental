#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"
#include "int_gnd.h"

void setup(void) {
}

void teardown(void) {
}

START_TEST (test_check_type)
{
	grounding_space_t* space = grounding_space_new();
	grounding_space_add(space, expr(atom_sym(":"), atom_sym("do"), atom_sym("Verb"), 0));
    atom_t* verb = atom_sym("Verb");

    atom_t* nonsense = atom_sym("nonsense");
    ck_assert(check_type(space, nonsense, ATOM_TYPE_UNDEFINED));
    ck_assert(check_type(space, nonsense, verb));
    atom_free(nonsense);

    atom_free(verb);
}
END_TEST

START_TEST (test_validate_atom)
{
	grounding_space_t* space = grounding_space_new();
	grounding_space_add(space, expr(atom_sym(":"), atom_sym("a"), atom_sym("A"), 0));
	grounding_space_add(space, expr(atom_sym(":"), atom_sym("b"), atom_sym("B"), 0));
	grounding_space_add(space, expr(atom_sym(":"), atom_sym("foo"), expr(atom_sym("->"), atom_sym("A"), atom_sym("B"), 0), 0));

    atom_t* foo = expr(atom_sym("foo"), atom_sym("a"), 0);
    ck_assert(validate_atom(space, foo));
    atom_free(foo);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_check_type);
    tcase_add_test(test_case, test_validate_atom);
}

TEST_MAIN(init_test);

