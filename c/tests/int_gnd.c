#include <stdio.h>

#include "int_gnd.h"

bool int_eq(struct gnd_t const* _a, struct gnd_t const* _b);
gnd_t* int_clone(const gnd_t* _self);
uintptr_t int_display(const struct gnd_t*, char*, uintptr_t);
void int_free(struct gnd_t*);

gnd_api_t const INT_GND_API = { 0, &int_eq, &int_clone, &int_display, &int_free };

gnd_t* int_new(int n) {
    int_gnd_t* self = malloc(sizeof(int_gnd_t));
    self->api = &INT_GND_API;
    self->typ = atom_sym("int");
    self->n = n;
    return (gnd_t*) self;
}

atom_t* int_atom_from_str(char const* str, void* context) {
    int i;
    sscanf(str, "%u", &i);
    return atom_gnd(int_new(i));
}

bool int_eq(struct gnd_t const* _a, struct gnd_t const* _b) {
    int_gnd_t *a = (int_gnd_t*)_a;
    int_gnd_t *b = (int_gnd_t*)_b;
    return a->n == b->n;
}

gnd_t* int_clone(const gnd_t* _self) {
    int_gnd_t* self = (int_gnd_t*)_self;
    gnd_t *copy = int_new(self->n);
    return copy;
}

uintptr_t int_display(const struct gnd_t* _self, char* buffer, uintptr_t max_size) {
    int_gnd_t* self = (int_gnd_t*)_self;
    return snprintf(buffer, max_size, "%d", self->n);
}

void int_free(struct gnd_t* self) {
    free(self);
}
