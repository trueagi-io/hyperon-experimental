#ifndef INT_GND_H
#define INT_GND_H

#include <hyperon.h>

typedef struct _int_gnd_t {
	gnd_api_t const* api;
	int n;
} int_gnd_t;

gnd_t* int_new(int n);

#endif /* INT_GND_H */
