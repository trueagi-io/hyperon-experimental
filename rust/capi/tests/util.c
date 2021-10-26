#include "util.h"

static char buffer[4096];

char const* stratom(atom_t const* atom) {
	atom_to_str(atom, buffer, sizeof(buffer)/sizeof(buffer[0]));
	return buffer;
}

