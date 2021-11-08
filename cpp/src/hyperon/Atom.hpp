#ifndef ATOM_HPP
#define ATOM_HPP

#include <string>
#include <vector>

#include <hyperon/hyperon.h>

// Atom

class ExprAtom;

class Atom {
protected:
	friend class VecAtom;
	friend class ExprAtom;

	// TODO: smart pointer can be used to make this pointer copyable
	atom_t* catom;

	Atom(atom_t* catom) : catom(catom) {}

	atom_t* extract_catom() {
		atom_t* extracted = catom;
		catom = nullptr;
		return extracted;
	}

	virtual std::string get_name() const;

public:
	Atom(Atom&& other) : catom(nullptr) {
		std::swap(this->catom, other.catom);
	}
	Atom(Atom& atom) = delete;
	Atom(Atom const& atom) = delete;

	virtual ~Atom() {
		if (catom) {
			atom_free(catom);
		}
	}

	atom_type_t get_type() const {
		return atom_get_type(catom);
	}

	bool operator==(Atom const& other) const {
		return atom_eq(catom, other.catom);
	}

	std::string to_string() const;
};

class SymbolAtom : public Atom {
public:
	SymbolAtom(std::string name) : Atom(atom_sym(name.c_str())) {}
	static SymbolAtom sym(std::string name) { return SymbolAtom(name); }
	std::string get_name() const { return Atom::get_name(); }
};

class VariableAtom : public Atom {
public:
	VariableAtom(std::string name) : Atom(atom_var(name.c_str())) {}
	static VariableAtom var(std::string name) { return VariableAtom(name); }
	std::string get_name() const { return Atom::get_name(); }
};

class ExprAtom : public Atom {
public:
	ExprAtom(std::vector<Atom*>&& children) : Atom(ExprAtom::into_catom(std::move(children))) {}
	static ExprAtom expr(std::vector<Atom*>&& children) { return ExprAtom(std::move(children)); }

private:
	static atom_t* into_catom(std::vector<Atom*>&& children);
};

// TODO: make naming in Rust and C++ conformant
class GroundedAtom;
class Grounded : public Atom {
public:
	Grounded(GroundedAtom* value) : Atom(Grounded::into_catom(value)) {}
	static Grounded gnd(GroundedAtom* value) { return Grounded(value); }
private:
	atom_t* into_catom(GroundedAtom* atom);
};

// TODO: rename to something more meaningful
class VecAtom;
extern const gnd_api_t CPP_GND_API;

class GroundedAtom {
public:
	struct cpp_gnd_t : gnd_t {
		GroundedAtom* self;
	};

	GroundedAtom() {
		gnd.api = &CPP_GND_API;
		gnd.self = this;
	}
	virtual ~GroundedAtom() {}
	virtual std::string execute(VecAtom& ops, VecAtom& data) {
		return to_string() + " is not executable";
	}
	virtual bool operator==(GroundedAtom const& other) const = 0;
	virtual GroundedAtom* clone() const = 0;
	virtual std::string to_string() const = 0;

	cpp_gnd_t gnd;
};

class VecAtom {
private:
	vec_atom_t* vec;

public:
	// TODO: make this constructor private
	VecAtom(vec_atom_t* vec) : vec(vec) { }

	virtual ~VecAtom() { }

	Atom pop() {
		return Atom(vec_pop(vec));
	}

	void push(Atom& atom) {
		vec_push(vec, atom.extract_catom());
	}
};

#endif /* ATOM_HPP */
