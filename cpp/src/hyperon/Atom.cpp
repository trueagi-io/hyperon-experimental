#include "Atom.hpp"

Atom Atom::expr(std::vector<Atom> &&children) {
	atom_t* _children[children.size()];
	std::transform(children.begin(), children.end(), _children,
		[](Atom& atom) -> atom_t* { return atom.extract_catom(); });
	return Atom(atom_expr(_children, children.size()));
}

std::string Atom::to_string() const {
	size_t size = atom_to_str(catom, 0, 0);
	char str[size];
	atom_to_str(catom, str, size);
	return std::string(str);
}
