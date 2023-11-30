
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

// A Convenience function to execute some MeTTa code, and check the resulting atom matches the
// expected text when rendered into a string
bool run_metta_and_compare_result(metta_t* runner, const char* metta_src, const char* expected_result) {

    char atom_str_buf[256];
    bool result = false;
    sexpr_parser_t parser = sexpr_parser_new(metta_src);
    atom_vec_t* results = NULL;
    metta_run(runner, parser, &copy_atom_vec, &results);
    if (results != NULL) {
        if (atom_vec_len(results) > 0) {
            atom_ref_t result_atom = atom_vec_get(results, 0);
            size_t len = atom_to_str(&result_atom, atom_str_buf, 256);

            result = strcmp(atom_str_buf, expected_result) == 0;
        }
        atom_vec_free(*results);
        free(results);
    }

    return result;
}

START_TEST (test_incremental_runner)
{
    metta_t runner = new_test_metta();

    sexpr_parser_t parser = sexpr_parser_new("!(+ 1 (+ 2 (+ 3 4)))");
    runner_state_t runner_state = runner_state_new_with_parser(&runner, parser);

    int step_count = 0;
    char atom_str_buf[64];
    atom_str_buf[0] = 0;
    while (!runner_state_is_complete(&runner_state)) {
        runner_state_step(&runner_state);

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

    runner_state_free(runner_state);
    metta_free(runner);
}
END_TEST

START_TEST (test_runner_errors)
{
    metta_t runner = new_test_metta();

    sexpr_parser_t parser = sexpr_parser_new("!(+ 1 (+ 2 (+ 3 4))");
    atom_vec_t* results = NULL;
    metta_run(&runner, parser, &copy_atom_vec, &results);

    //We have a parse error, so the callback should never be called
    ck_assert(results == NULL);
    ck_assert_str_eq(metta_err_str(&runner), "Unexpected end of expression");

    metta_free(runner);
}
END_TEST

void deleteme_metta_init(metta_t* metta, void* context) {} //LP-TODO-NEXT: Delete this when I rework runner-creation API to take ModIds instead of an init function

size_t path_for_name(const void *payload, const char *parent_dir, const char *mod_name, char *dst_buf, uintptr_t buf_size) {
    const char* suffix = ".ctestmod";
    size_t parent_dir_len = strlen(parent_dir);
    size_t mod_name_len = strlen(mod_name);
    size_t suffix_len = strlen(suffix);
    size_t new_len = parent_dir_len + mod_name_len + suffix_len + 1; //An extra byte for the '/'

    if (new_len+1 > buf_size) { // +1 for the terminator char
        return 0;
    }

    //Construct the new path.  String manipulation is a pain in C
    char* write_ptr = dst_buf;
    strncpy(write_ptr, parent_dir, parent_dir_len);
    write_ptr += parent_dir_len;
    *write_ptr = '/';
    write_ptr += 1;
    strncpy(write_ptr, mod_name, mod_name_len);
    write_ptr += mod_name_len;
    strncpy(write_ptr, suffix, suffix_len);
    dst_buf[new_len] = 0;

    return new_len+1;
}

bool try_path(const void *payload, const char *path, const char *mod_name) {

    //In a real implementation, we would actually check the file-system object (file, dir, etc.) to
    // validate it was a valid module in a format this code can understand.  But this is a test, so we
    // will assume it is valid if the filename is "ctest-mod.ctestmod", and otherwise it isn't valid
    const char* test_str = "ctest-mod.ctestmod";
    size_t test_str_len = strlen(test_str);
    size_t path_len = strlen(path);
    if (test_str_len > path_len) {
        return false;
    }

    return strncmp(path+(path_len-test_str_len), test_str, test_str_len) == 0;
}

module_descriptor_t descriptor(const void *payload, const char *path, const char *mod_name) {
    ck_assert(strcmp(mod_name, "ctest-mod")==0);
    return module_descriptor_new("ctest-mod");
}

void loader(const void *payload, const char *path, struct run_context_t *context, struct module_descriptor_t descriptor) {

    space_t space = space_new_grounding_space();
    run_context_init_self_module(context, descriptor, space, NULL);

    sexpr_parser_t parser = sexpr_parser_new("test-atom");
    run_context_push_parser(context, parser);
}

// Module Format API Declaration for test format
static mod_loader_api_t const TEST_FMT_API= {
    .path_for_name = &path_for_name,
    .try_path = &try_path,
    .descriptor = &descriptor,
    .loader = &loader,
};

START_TEST (test_custom_module_format)
{
    env_builder_t env_builder = env_builder_start();
    env_builder_set_is_test(&env_builder, true);
    env_builder_push_fs_module_format(&env_builder, &TEST_FMT_API, NULL);

    //Start by initializing a runner using an environment with our custom module format
    space_t space = space_new_grounding_space();
    metta_t runner = metta_new_with_space_environment_and_stdlib(&space, env_builder, &deleteme_metta_init, NULL);
    space_free(space);

    //Load a module using our custom format, and verify it was loaded sucessfully
    ck_assert(run_metta_and_compare_result(&runner, "!(import! ctest-mod loaded-test)", "()"));

    //Test that we can match an atom in the module loaded with the custom format
    ck_assert(run_metta_and_compare_result(&runner, "!(match &loaded-test test-atom found!)", "found!"));

    //Try and load a module that our format will reject, and validate we get an error
    ck_assert(run_metta_and_compare_result(&runner, "!(import! bogus-mod loaded-test)", "(Error (import! bogus-mod loaded-test) Failed to resolve module bogus-mod)"));

    metta_free(runner);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_set_timeout(test_case, 300); //300s = 5min.  To test for memory leaks
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_incremental_runner);
    tcase_add_test(test_case, test_runner_errors);
    tcase_add_test(test_case, test_custom_module_format);
}

TEST_MAIN(init_test);
