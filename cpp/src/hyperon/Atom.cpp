#include <cstring>
#include <algorithm>

#include "Atom.hpp"

auto const& get_string_from_rust = [](char const* cstr, void* context) -> void {
	std::string* cppstr = static_cast<std::string*>(context);
	cppstr->assign(cstr);
};

std::string Atom::to_string() const {
	size_t size = atom_to_str(catom, 0, 0);
	char str[size];
	atom_to_str(catom, str, size);
	return std::string(str);
}

std::string Atom::get_name() const {
	std::string name;
	atom_get_name(catom, get_string_from_rust, &name);
	return name;
}

atom_t* ExprAtom::into_catom(std::vector<Atom*> &&children) {
	atom_t* _children[children.size()];
	std::transform(children.begin(), children.end(), _children,
		[](Atom* atom) -> atom_t* { return atom_copy(atom->catom); });
	return atom_expr(_children, children.size());
}

extern "C" {
	const char *cpp_execute(const struct gnd_t* _gnd, struct vec_atom_t* ops, struct vec_atom_t* data);
	bool cpp_eq(const struct gnd_t* _a, const struct gnd_t* _b);
	struct gnd_t *cpp_clone(const struct gnd_t* _gnd);
	uintptr_t cpp_display(const struct gnd_t* _gnd, char* buffer, uintptr_t size);
	void cpp_free(struct gnd_t* _gnd);
}

gnd_api_t const CPP_GND_API = { &cpp_execute, &cpp_eq, &cpp_clone, &cpp_display, &cpp_free };

static char buffer[4096];
typedef GroundedAtom::cpp_gnd_t cpp_gnd_t;

const char *cpp_execute(const struct gnd_t* _gnd, struct vec_atom_t* _ops, struct vec_atom_t* _data) {
	cpp_gnd_t const* gnd = static_cast<cpp_gnd_t const*>(_gnd);
	VecAtom ops(_ops), data(_data);
	std::string err = gnd->self->execute(ops, data);
	size_t size = std::min(err.size(), sizeof(buffer) - 1);
	memcpy(buffer, err.c_str(), size);
	buffer[size] = 0;
	return buffer;
}

bool cpp_eq(const struct gnd_t* _a, const struct gnd_t* _b) {
	cpp_gnd_t const* a = static_cast<cpp_gnd_t const*>(_a);
	cpp_gnd_t const* b = static_cast<cpp_gnd_t const*>(_b);
	return *(a->self) == *(b->self);
}

struct gnd_t *cpp_clone(const struct gnd_t* _gnd) {
	cpp_gnd_t const* gnd = static_cast<cpp_gnd_t const*>(_gnd);
	return &(gnd->self->clone()->gnd);
}

uintptr_t cpp_display(const struct gnd_t* _gnd, char* buffer, uintptr_t size) {
	cpp_gnd_t const* gnd = static_cast<cpp_gnd_t const*>(_gnd);
	std::string str = gnd->self->to_string();
	if (buffer) {
		if (size >= str.size() + 1) {
			memcpy(buffer, str.c_str(), str.size() + 1);
		} else if (size >= 4) {
			memcpy(buffer, str.c_str(), size - 4);
			memcpy(buffer + size - 4, "...", 4);
		} else if (size > 0) {
			buffer[0] = 0;	
		}
	}
	return str.size() + 1;
}

void cpp_free(struct gnd_t* _gnd) {
	cpp_gnd_t* gnd = static_cast<cpp_gnd_t*>(_gnd);
	delete gnd->self;
}

atom_t* Grounded::into_catom(GroundedAtom* atom) {
	gnd_t* gnd = static_cast<gnd_t*>(&(atom->gnd));
	atom_t* catom = atom_gnd(gnd);
	return catom;
}

