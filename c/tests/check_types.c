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

typedef struct _atoms_t {
    atom_t** items;
    size_t size;
} atoms_t;

void check_atoms(atom_array_t act_atoms, void* context) {
    int i = 0;
    atom_t const** exp_atoms = context;

    while (i < act_atoms.size && exp_atoms[i]) {
        atom_t const* expected = exp_atoms[i];
        atom_t const* actual = act_atoms.items[i];
        char* expected_str = stratom(expected);
        char* actual_str = stratom(actual);
        ck_assert_msg(atom_eq(expected, actual),
                "expected atom [%u]: '%s', is not equal to actual atom [%u]: '%s'",
                i, expected_str, i, actual_str);
        free(expected_str);
        free(actual_str);
        ++i;
    }
    ck_assert_msg(i == act_atoms.size && !exp_atoms[i], "actual size: %u, expected size: %u", act_atoms.size, i);
}

START_TEST (test_get_atom_types)
{
    grounding_space_t* space = grounding_space_new();
    grounding_space_add(space, expr(atom_sym(":"), atom_sym("a"), expr(atom_sym("->"), atom_sym("C"), atom_sym("D"), 0), 0));
    grounding_space_add(space, expr(atom_sym(":"), atom_sym("b"), atom_sym("B"), 0));
    grounding_space_add(space, expr(atom_sym(":"), atom_sym("c"), atom_sym("C"), 0));

    atom_t* D = atom_sym("D");
    atom_t* a = atom_sym("a");
    atom_t* a_type = expr(atom_sym("->"), atom_sym("C"), atom_sym("D"), 0);
    atom_t* call_a_c = expr(atom_sym("a"), atom_sym("c"), 0);
    atom_t* call_a_b = expr(atom_sym("a"), atom_sym("b"), 0);

    atom_t const* call_a_c_types[] = { D, 0 };
    get_atom_types(space, call_a_c, &check_atoms, &call_a_c_types);
    atom_t const* call_a_b_types[] = { 0 };
    get_atom_types(space, call_a_b, &check_atoms, &call_a_b_types);
    atom_t const* a_types[] = { a_type, 0 };
    get_atom_types(space, a, &check_atoms, &a_types);

    atom_free(call_a_b);
    atom_free(call_a_c);
    atom_free(a_type);
    atom_free(a);
    atom_free(D);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_check_type);
    tcase_add_test(test_case, test_validate_atom);
    tcase_add_test(test_case, test_get_atom_types);
}

TEST_MAIN(init_test);

