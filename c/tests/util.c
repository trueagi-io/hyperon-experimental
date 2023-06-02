#include <stdarg.h>
#include <string.h>

#include "util.h"

char* stratom(atom_t const* atom) {
    char temp_buffer[2048];
    size_t len = atom_to_str(atom, temp_buffer, 2048);
    char* buffer = malloc(len+1);
    strncpy(buffer, temp_buffer, len+1);
    buffer[len] = 0;
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
