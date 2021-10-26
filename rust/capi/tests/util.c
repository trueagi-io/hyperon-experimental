#include <stdarg.h>

#include "util.h"

static char buffer[4096];

char const* stratom(atom_t const* atom) {
	atom_to_str(atom, buffer, sizeof(buffer)/sizeof(buffer[0]));
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
