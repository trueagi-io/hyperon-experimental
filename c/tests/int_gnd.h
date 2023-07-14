#ifndef INT_GND_H
#define INT_GND_H

#include <hyperon/hyperon.h>

typedef struct _int_gnd_t {
    gnd_api_t const* api;
    atom_t typ;
    int n;
} int_gnd_t;

gnd_t* int_new(int n);
atom_t int_atom_from_str(char const* str, void* context);

#endif /* INT_GND_H */
