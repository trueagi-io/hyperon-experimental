#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"
#include "int_gnd.h"

void setup(void) {
}

void teardown(void) {
}

START_TEST (test_sym)
{
    char name[] = "test";
    atom_t* atom = atom_sym(name);
    name[0] = 'r';
    
    char* actual = stratom(atom);
    ck_assert_str_eq(actual, "test");

    free(actual);
    atom_free(atom);
}
END_TEST

START_TEST (test_expr)
{
    atom_t* atom = expr(atom_sym("test"), atom_var("var"), atom_sym("five"), atom_gnd(int_new(42)), 0);

    char* actual = stratom(atom);
    ck_assert_str_eq(actual, "(test $var five 42)");

    free(actual);
    atom_free(atom);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_sym);
    tcase_add_test(test_case, test_expr);
}

TEST_MAIN(init_test);
