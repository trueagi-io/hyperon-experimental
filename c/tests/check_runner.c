
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
            if (!result) {
                fprintf(stderr, "Expected: \"%s\"\nFound:    \"%s\"\n", expected_result, atom_str_buf);
            }
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

ssize_t load(void const* payload, run_context_t* run_context, write_t err) {
    space_t space = space_new_grounding_space();
    run_context_init_self_module(run_context, &space, NULL);
    space_free(space);

    sexpr_parser_t parser = sexpr_parser_new("test-atom");
    run_context_push_parser(run_context, parser);
    return 0;
}

static module_loader_t const TEST_MODULE_LOADER = {
    .load = &load,
    .load_tokens = NULL,
    .to_string = NULL,
    .free = NULL,
};

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

bool try_path(const void *payload, const char *path, const char *mod_name, module_loader_t const ** mod_loader, module_descriptor_t *mod_descriptor) {

    //In a real implementation, we would actually check the file-system object (file, dir, etc.) to
    // validate it was a valid module in a format this code can understand.  But this is a test, so we
    // will assume it is valid if the filename is "ctest-mod.ctestmod", and otherwise it isn't valid
    const char* test_str = "ctest-mod.ctestmod";
    size_t test_str_len = strlen(test_str);
    size_t path_len = strlen(path);
    if (test_str_len > path_len) {
        return false;
    }

    if (strncmp(path+(path_len-test_str_len), test_str, test_str_len) == 0) {
        *mod_descriptor = module_descriptor_new(mod_name);
        *mod_loader = &TEST_MODULE_LOADER;
        return true;
    } else {
        return false;
    }
}

static fs_module_format_t const TEST_MODULE_FORMAT = {
    .path_for_name = &path_for_name,
    .try_path = &try_path,
    .free = NULL,
};

START_TEST (test_custom_module_format)
{
    env_builder_t env_builder = env_builder_start();
    env_builder_set_is_test(&env_builder, true);
    env_builder_push_fs_module_format(&env_builder, &TEST_MODULE_FORMAT );

    //Start by initializing a runner using an environment with our custom module format
    space_t space = space_new_grounding_space();
    metta_t runner = metta_new_with_stdlib_loader(NULL, &space, env_builder);
    space_free(space);

    //Load a module using our custom format, and verify it was loaded sucessfully
    ck_assert(run_metta_and_compare_result(&runner, "!(import! &loaded-test ctest-mod)", "()"));

    //Test that we can match an atom in the module loaded with the custom format
    ck_assert(run_metta_and_compare_result(&runner, "!(match &loaded-test test-atom found!)", "found!"));

    //Try and load a module that our format will reject, and validate we get an error
    ck_assert(run_metta_and_compare_result(&runner, "!(import! &new-space bogus-mod)", "(Error (import! &new-space bogus-mod) Failed to resolve module top:bogus-mod)"));

    metta_free(runner);
}
END_TEST

ssize_t custom_stdlib_loader(void const* loader, run_context_t *run_context, write_t err) {

    //Init our new module
    space_t space = space_new_grounding_space();
    run_context_init_self_module(run_context, &space, NULL);
    space_free(space);

    //"import * from corelib" (This is optional, and some implementations might not want it)
    module_id_t corelib_mod_id = run_context_load_module(run_context, "top:corelib");
    run_context_import_dependency(run_context, corelib_mod_id);

    //Load a custom atom using the MeTTa syntax
    sexpr_parser_t parser = sexpr_parser_new("test-atom");
    run_context_push_parser(run_context, parser);

    return 0;
}

START_TEST (test_custom_stdlib)
{
    //Start by initializing a runner using our custom stdlib loader
    space_t space = space_new_grounding_space();
    module_loader_t* stdlib_loader = calloc(sizeof(module_loader_t), 1);
    stdlib_loader->load = custom_stdlib_loader;
    stdlib_loader->free = free;
    metta_t runner = metta_new_with_stdlib_loader(stdlib_loader, &space, env_builder_use_test_env());
    space_free(space);

    //Test that we can match an atom loaded from the custom stdlib function
    ck_assert(run_metta_and_compare_result(&runner, "!(match &self test-atom found!)", "found!"));

    //Test that an operation using the core stdlib works too
    ck_assert(run_metta_and_compare_result(&runner, "!(+ 1 (+ 2 (+ 3 4)))", "10"));

    metta_free(runner);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_set_timeout(test_case, 300); //300s = 5min.  To test for memory leaks
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_incremental_runner);
    tcase_add_test(test_case, test_runner_errors);
    tcase_add_test(test_case, test_custom_module_format);
    tcase_add_test(test_case, test_custom_stdlib);
}

TEST_MAIN(init_test);
