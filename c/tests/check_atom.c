#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"
#include "int_gnd.h"

void setup(void) {
}

void teardown(void) {
}

#include "stdio.h"

void bindings_to_buf(bindings_t* bindings, void *context) {
    char* dst_buf = context;
    bindings_to_str(bindings, dst_buf, BUF_SIZE);
};

void clone_one_bindings(bindings_t* bindings, void *context) {
    if (*((bindings_t**)context) != NULL) abort();
    *((bindings_t**)context) = bindings_clone(bindings);
}

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
    bindings_set_merge_into(set_1, set_2);

    atom_t* a_var = atom_var("a");
    atom_t* a_prime_var = atom_var("a_prime");
    atom_t* d_var = atom_var("d");
    atom_t* d_sym = atom_sym("D");
    bindings_set_add_var_equality(set_1, a_var, a_prime_var);
    bindings_set_add_var_binding(set_1, d_var, d_sym);
    atom_free(a_var);
    atom_free(a_prime_var);
    atom_free(d_var);
    atom_free(d_sym);

    bindings_t* bindings_expected = bindings_new();
    var_atom_t var_atom_a_expected = {.var = "a", .atom = atom_sym("A")};
    var_atom_t var_atom_a_prime_expected = {.var = "a_prime", .atom = atom_sym("A")};
    var_atom_t var_atom_b_expected = {.var = "b", .atom = atom_sym("B")};
    var_atom_t var_atom_c_expected = {.var = "c", .atom = atom_sym("C")};
    var_atom_t var_atom_d_expected = {.var = "d", .atom = atom_sym("D")};
    bindings_add_var_binding(bindings_expected, &var_atom_a_expected);
    bindings_add_var_binding(bindings_expected, &var_atom_a_prime_expected);
    bindings_add_var_binding(bindings_expected, &var_atom_b_expected);
    bindings_add_var_binding(bindings_expected, &var_atom_c_expected);
    bindings_add_var_binding(bindings_expected, &var_atom_d_expected);

    bindings_t* result_bindings = NULL;
    bindings_set_iterate(set_1, &clone_one_bindings, &result_bindings);
    ck_assert(bindings_eq(result_bindings, bindings_expected));

    char str_buf[BUF_SIZE];
    bindings_set_iterate(set_1, &bindings_to_buf, &str_buf);
    //printf("%s\n\n", str_buf);
    ck_assert(strlen(str_buf) == 49); //It's a pain to test every combinitory string, but they are all the same length

    //Don't need to free bindings_a, because ownership was consumed into set_1 by bindings_merge_v2
    bindings_free(bindings_b);
    //Don't need to free bindings_c, because ownership was consumed into set_2 by bindings_set_from_bindings
    bindings_free(bindings_expected);
    bindings_free(result_bindings);

    bindings_set_free(set_1);
    bindings_set_free(set_2);
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
    tcase_set_timeout(test_case, 300); //300s = 5min.  To test for memory leaks
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_bindings_set);
    tcase_add_test(test_case, test_sym);
    tcase_add_test(test_case, test_expr);
}

TEST_MAIN(init_test);
