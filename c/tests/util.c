#include <hyperon/hyperon.h>

#include <stdarg.h>
#include <string.h>

#include "util.h"

char* stratom(atom_t const* atom) {
    size_t len = atom_to_str(atom, NULL, 0);
    char* buffer = malloc(len+1);
    atom_to_str(atom, buffer, len+1);
    return buffer;
}

#define MAXARGS 64
atom_t expr(atom_t atom, ...) {
    va_list ap;
    atom_t children[MAXARGS];
    int argno = 0;

    va_start(ap, atom);
    //NOTE: It's illagal to pass NULL as the list terminator, but the compiler can't check it
    // because va_list is un-typed.  Therefore, we'll be on the safe side, and stop looping
    // if the first ptr-sized chunk of the atom is NULL.
    while ((*(void**)&atom != NULL) && !atom_is_null(&atom) && argno < MAXARGS) {
        children[argno++] = atom;
        atom = va_arg(ap, atom_t);
    }
    va_end(ap);
    return atom_expr(children, argno);
}

metta_t new_test_metta(void) {
    space_t space = space_new_grounding_space();
    metta_t metta = metta_new_with_stdlib_loader(NULL, &space, env_builder_use_test_env());
    space_free(space);
    return metta;
}
