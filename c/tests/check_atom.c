#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"
#include "int_gnd.h"

void setup(void) {
}

void teardown(void) {
}

#include "stdio.h"

void print_str(const char *str, void *context) {
    printf("%s", str);
};

void print_bindings(const bindings_t* bindings, void *context) {
    bindings_to_str(bindings, &print_str, NULL);
};

START_TEST (test_bindings_set)
{    
    bindings_t* bindings_a = bindings_new();
    var_atom_t var_atom_a = {.var = "a", .atom = atom_sym("A")};
    bindings_add_var_binding(bindings_a, &var_atom_a);

    bindings_t* bindings_b = bindings_new();
    var_atom_t var_atom_b = {.var = "b", .atom = atom_sym("B")};
    bindings_add_var_binding(bindings_b, &var_atom_b);

    bindings_set_t* set_1 = bindings_merge_v2(bindings_a, bindings_b);

    bindings_t* bindings_c = bindings_new();
    var_atom_t var_atom_c = {.var = "c", .atom = atom_sym("C")};
    bindings_add_var_binding(bindings_c, &var_atom_c);

    bindings_set_t* set_2 = bindings_set_from_bindings(bindings_c);
    bindings_set_t* result_set = bindings_set_merge(set_1, set_2);

    bindings_set_iterate(result_set, &print_bindings, NULL);

    bindings_free(bindings_a);
    bindings_free(bindings_b);
    bindings_free(bindings_c);
    bindings_set_free(set_1);
    bindings_set_free(set_2);
    bindings_set_free(result_set);
}
END_TEST

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
    tcase_add_test(test_case, test_bindings_set);
    tcase_add_test(test_case, test_sym);
    tcase_add_test(test_case, test_expr);
}

TEST_MAIN(init_test);
