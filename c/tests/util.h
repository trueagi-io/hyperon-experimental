#ifndef UTIL_H
#define UTIL_H

#include <hyperon/hyperon.h>

char* stratom(atom_t const* atom);
atom_t* expr(atom_t* atom, ...);

#endif /* UTIL_H */
