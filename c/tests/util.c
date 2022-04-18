#include <stdarg.h>
#include <string.h>

#include "util.h"

static char buffer[4096];
const size_t buffer_size = sizeof(buffer)/sizeof(buffer[0]);

void return_string(char const* value, void* context) {
	strncpy(buffer, value, buffer_size - 1);
	buffer[buffer_size - 1] = 0;
}

char const* stratom(atom_t const* atom) {
	c_str_callback_t callback = { return_string };
	atom_to_str(atom, &callback);
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
