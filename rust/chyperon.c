#include <stdio.h>

#include "hyperon.h"

typedef struct _int_gnd_t {
	gnd_api_t const* api;
	int n;
} int_gnd_t;

bool int_eq(struct gnd_t const* _a, struct gnd_t const* _b);
gnd_t* int_clone(const gnd_t* _self);
uintptr_t int_display(const struct gnd_t*, char*, uintptr_t);
void int_free(struct gnd_t*);

gnd_api_t const INT_GND_API = { 0, &int_eq, &int_clone, &int_display, &int_free };

gnd_t* int_new(int n) {
	int_gnd_t* self = malloc(sizeof(int_gnd_t));
	self->api = &INT_GND_API;
	self->n = n;
	printf("int_new: %p\n", self);
	return (gnd_t*) self;
}

bool int_eq(struct gnd_t const* _a, struct gnd_t const* _b) {
	int_gnd_t *a = (int_gnd_t*)_a;
	int_gnd_t *b = (int_gnd_t*)_b;
	return a->n == b->n;
}

gnd_t* int_clone(const gnd_t* _self) {
	int_gnd_t* self = (int_gnd_t*)_self;
	gnd_t *copy = int_new(self->n);
	printf("int_clone: %p\n", copy);
	return copy;
}

uintptr_t int_display(const struct gnd_t* _self, char* buffer, uintptr_t max_size) {
	int_gnd_t* self = (int_gnd_t*)_self;
	return snprintf(buffer, max_size, "%d", self->n);
}

void int_free(struct gnd_t* self) {
	printf("int_free: %p\n", self);
	free(self);
}

int main() {
	char name[] = "test";
	atom_t* expr[] = {atom_sym(name), atom_var("X"),
		atom_sym("5"), atom_gnd(int_new(4))};
	printf("expr created\n");
	name[1] = 'r';
	int expr_size = sizeof(expr)/sizeof(expr[0]);
	atom_t* atom = atom_expr(expr, expr_size);
	for (int i = 0; i < expr_size; ++i) {
		free_atom(expr[i]);
	}
	printf("%s\n", atom_to_str(atom));
	free_atom(atom);
	return 0;
}
