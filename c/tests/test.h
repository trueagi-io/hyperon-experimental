#ifndef TEST_H
#define TEST_H

#include <check.h>

#define TEST_MAIN(init_test) \
    int main(void) { \
        return test_main(init_test); \
    }

int test_main(void (*init_test)(TCase* test_case));

#endif /* TEST_H */
