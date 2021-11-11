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
	static Atom* from_catom(atom_t* catom);

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
protected:
	friend class Atom;
	SymbolAtom(atom_t* catom) : Atom(catom) {}
public:
	SymbolAtom(std::string name) : Atom(atom_sym(name.c_str())) {}
	static SymbolAtom sym(std::string name) { return SymbolAtom(name); }
	std::string get_name() const { return Atom::get_name(); }
};

class VariableAtom : public Atom {
protected:
	friend class Atom;
	VariableAtom(atom_t* catom) : Atom(catom) {}
public:
	VariableAtom(std::string name) : Atom(atom_var(name.c_str())) {}
	static VariableAtom var(std::string name) { return VariableAtom(name); }
	std::string get_name() const { return Atom::get_name(); }
};

class ExprAtom : public Atom {
protected:
	friend class Atom;
	ExprAtom(atom_t* catom) : Atom(catom) {}
public:
	ExprAtom(std::vector<Atom*>&& children) : Atom(ExprAtom::into_catom(std::move(children))) {}
	static ExprAtom expr(std::vector<Atom*>&& children) { return ExprAtom(std::move(children)); }

private:
	static atom_t* into_catom(std::vector<Atom*>&& children);
};

// TODO: make naming in Rust and C++ conformant
class GroundedAtom;
struct cpp_gnd_t : gnd_t {
	GroundedAtom* self;
};

class Grounded : public Atom {
protected:
	friend class Atom;
	Grounded(atom_t* catom) : Atom(catom) {}
public:
	Grounded(GroundedAtom* value) : Atom(Grounded::into_catom(value)) {}
	static Grounded gnd(GroundedAtom* value) { return Grounded(value); }
	GroundedAtom* get_object() { 
		// TODO: support case of Rust object returned from C API
		gnd_t* _obj = atom_get_object(catom);
		return static_cast<cpp_gnd_t*>(_obj)->self;
	}
private:
	atom_t* into_catom(GroundedAtom* atom);
};

// TODO: rename to something more meaningful
class VecAtom;
extern const gnd_api_t CPP_GND_API;

class GroundedAtom {
public:
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
protected:
	vec_atom_t* vec;

	VecAtom(vec_atom_t* vec) : vec(vec) { }

public:
	virtual ~VecAtom() { }

	Atom* pop() {
		return Atom::from_catom(vec_pop(vec));
	}

	void push(Atom& atom) {
		vec_push(vec, atom_copy(atom.catom));
	}
};

class VecAtomCpp : public VecAtom {
public:
	VecAtomCpp() : VecAtom(vec_atom_new()) { }
	virtual ~VecAtomCpp() {
		vec_atom_free(vec);
	}
};

#endif /* ATOM_HPP */
