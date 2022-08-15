#include "test.h"

#include <stdlib.h>

#include <hyperon/hyperon.h>

static Suite * capi_suite(void (*init_test)(TCase* test_case)) {
    Suite *s;
    TCase *tc_core;

    s = suite_create("Suite");

    tc_core = tcase_create("Core");
    init_test(tc_core);
    suite_add_tcase(s, tc_core);

    return s;
}

int test_main(void (*init_test)(TCase* test_case)) {
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = capi_suite(init_test);
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
