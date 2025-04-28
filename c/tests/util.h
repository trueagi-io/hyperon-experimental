#ifndef UTIL_H
#define UTIL_H

#include <hyperon/hyperon.h>

#define BUF_SIZE 4096

// required by MSVC compiler
#define ssize_t ptrdiff_t

void str_to_buf(const char *str, void *context);

char* stratom(atom_t const* atom);
atom_t expr(atom_t atom, ...);

// A function that initializes a MeTTa runner with a test environment, similar to `metta_new_rust()`
metta_t new_test_metta(void);

#endif /* UTIL_H */
