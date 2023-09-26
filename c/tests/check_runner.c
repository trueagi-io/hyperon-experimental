
#include <stdio.h>
#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"

void setup(void) {
}

void teardown(void) {
}

void copy_atom_vec(const atom_vec_t* atoms, void* context) {
    atom_vec_t** dst_ptr = (atom_vec_t**)context;
    if (*dst_ptr != NULL) {
        abort();
    }
    *dst_ptr = malloc(sizeof(atom_vec_t));
    **dst_ptr = atom_vec_clone(atoms);
}

START_TEST (test_incremental_runner)
{
    metta_t runner = metta_new();
    metta_load_module(&runner, "stdlib");

    runner_state_t runner_state = metta_start_run(&runner);

    sexpr_parser_t parser = sexpr_parser_new("!(+ 1 (+ 2 (+ 3 4)))");

    int step_count = 0;
    char atom_str_buf[64];
    atom_str_buf[0] = 0;
    while (!runner_state_is_complete(&runner_state)) {
        metta_run_step(&runner, &parser, &runner_state);

        atom_vec_t* results = NULL;
        runner_state_current_results(&runner_state, &copy_atom_vec, &results);

        if (results != NULL) {
            if (atom_vec_len(results) > 0) {
                atom_ref_t result_atom = atom_vec_get(results, 0);
                size_t len = atom_to_str(&result_atom, atom_str_buf, 64);
            }
            atom_vec_free(*results);
            free(results);
        }

        step_count++;
    }
    ck_assert_str_eq(atom_str_buf, "10");

    sexpr_parser_free(parser);

    runner_state_free(runner_state);
    metta_free(runner);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_set_timeout(test_case, 300); //300s = 5min.  To test for memory leaks
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_incremental_runner);
}

TEST_MAIN(init_test);
