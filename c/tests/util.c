#include <stdarg.h>
#include <string.h>

#include "util.h"

void return_string(char const* value, void* context) {
    char** buffer = context;
    size_t length = strlen(value);
    *buffer = malloc(length + 1);
    strncpy(*buffer, value, length);
    (*buffer)[length] = 0;
}

char* stratom(atom_t const* atom) {
    char* buffer;
    atom_to_str(atom, return_string, &buffer);
    return buffer;
}

#define MAXARGS 64

atom_t* expr(atom_t* atom, ...) {
    va_list ap;
    atom_t* children[MAXARGS];
    int argno = 0;

    va_start(ap, atom);
    while (atom != 0 && argno < MAXARGS) {
        children[argno++] = atom;
        atom = va_arg(ap, atom_t*);
    }
    va_end(ap);
    return atom_expr(children, argno);
}
