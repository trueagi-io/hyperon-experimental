#ifndef ATOM_HPP
#define ATOM_HPP

#include <algorithm>
#include <string>
#include <vector>

#include <hyperon/hyperon.h>

class Atom {

private:

	atom_t* catom;

	Atom(atom_t* catom) : catom(catom) {}

	atom_t* extract_catom() {
		atom_t* extracted = catom;
		catom = nullptr;
		return extracted;
	}

public:
	Atom(Atom const& other) : catom(atom_copy(other.catom)) { }

	virtual ~Atom() {
		if (catom) {
			atom_free(catom);
		}
	}

	bool operator==(Atom const& other) const {
		return atom_eq(catom, other.catom);
	}

	std::string to_string() const;

	static Atom sym(std::string const& name) {
		return Atom(atom_sym(name.c_str()));
	}

	static Atom var(std::string const& name) {
		return Atom(atom_var(name.c_str()));
	}

	static Atom expr(std::vector<Atom> &&children);
};

#endif /* ATOM_HPP */
