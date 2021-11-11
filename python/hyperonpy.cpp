#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <hyperon/hyperon.h>

namespace py = pybind11;

struct CAtom {
	CAtom(atom_t* catom) : catom(catom) {}
	atom_t* catom;
};

struct CVecAtom {
	CVecAtom(vec_atom_t* cvec) : cvec(cvec) {}
	vec_atom_t* cvec;
};

struct CGroundingSpace {
	CGroundingSpace(grounding_space_t* cspace) : cspace(cspace) {}
	grounding_space_t* cspace;
};

auto const& copy_to_string = [](char const* cstr, void* context) -> void {
	std::string* cppstr = static_cast<std::string*>(context);
	cppstr->assign(cstr);
};

extern "C" {
	const char *py_execute(const struct gnd_t* _gnd, struct vec_atom_t* ops, struct vec_atom_t* data);
	bool py_eq(const struct gnd_t* _a, const struct gnd_t* _b);
	struct gnd_t *py_clone(const struct gnd_t* _gnd);
	uintptr_t py_display(const struct gnd_t* _gnd, char* buffer, uintptr_t size);
	void py_free(struct gnd_t* _gnd);
}

const gnd_api_t PY_GROUNDED_API = { &py_execute, &py_eq, &py_clone, &py_display, &py_free };

struct GroundedObject : gnd_t {
	GroundedObject(py::object pyobj) : self(this), pyobj(pyobj) {
		this->api = &PY_GROUNDED_API;
		pyobj.inc_ref();
	}
	~GroundedObject() {
		pyobj.dec_ref();
	}
	GroundedObject* self;
	py::object pyobj;
};

const char *py_execute(const struct gnd_t* _gnd, struct vec_atom_t* ops, struct vec_atom_t* data) {
	return nullptr;	
}

bool py_eq(const struct gnd_t* _a, const struct gnd_t* _b) {
	py::object a = static_cast<GroundedObject const*>(_a)->pyobj;
	py::object b = static_cast<GroundedObject const*>(_b)->pyobj;
	return a.equal(b);
}

struct gnd_t *py_clone(const struct gnd_t* _cgnd) {
	py::object pyobj = static_cast<GroundedObject const*>(_cgnd)->pyobj;
	py::object copy = pyobj.attr("copy")();
	return new GroundedObject(copy);
}

uintptr_t py_display(const struct gnd_t* _cgnd, char* buffer, uintptr_t size) {
	py::object pyobj = static_cast<GroundedObject const*>(_cgnd)->pyobj;
	std::string str = py::str(pyobj).cast<std::string>();
	strncpy(buffer, str.c_str(), size - 1);
	buffer[size - 1] = 0;
	return str.size();
}

void py_free(struct gnd_t* _cgnd) {
	delete _cgnd;
}

PYBIND11_MODULE(hyperonpy, m) {
	m.doc() = "Python API of the Hyperon library";

	py::enum_<atom_type_t>(m, "AtomType")
		.value("SYMBOL", atom_type_t::SYMBOL)
		.value("VARIABLE", atom_type_t::VARIABLE)
		.value("EXPR", atom_type_t::EXPR)
		.value("GROUNDED", atom_type_t::GROUNDED)
		.export_values();

	py::class_<CAtom>(m, "CAtom");

    m.def("atom_sym", [](char const* name) { return CAtom(atom_sym(name)); }, "Create symbol atom");
    m.def("atom_var", [](char const* name) { return CAtom(atom_var(name)); }, "Create variable atom");
    m.def("atom_expr", [](py::list _children) {
    		size_t size = py::len(_children);
    		atom_t* children[size];
    		int idx = 0;
    		for (auto atom : _children) {
    			// Copying atom is required because atom_expr() moves children
    			// catoms inside new expression atom.
    			children[idx++] = atom_copy(atom.cast<CAtom&>().catom);
    		}
    		return CAtom(atom_expr(children, size));
    	}, "Create expression atom");
    m.def("atom_gnd", [](py::object object) { return CAtom(atom_gnd(new GroundedObject(object))); }, "Create grounded atom");
    m.def("atom_free", [](CAtom atom) { atom_free(atom.catom); }, "Free C atom");

    m.def("atom_eq", [](CAtom a, CAtom b) -> bool { return atom_eq(a.catom, b.catom); }, "Test if two atoms are equal");
    m.def("atom_to_str", [](CAtom atom) {
			std::string str;
    		atom_to_str(atom.catom, copy_to_string, &str);
    		return str;
    	}, "Convert atom to human readable string");
    m.def("atom_get_type", [](CAtom atom) { return atom_get_type(atom.catom); }, "Get type of the atom");
    m.def("atom_get_name", [](CAtom atom) {
			std::string str;
    		atom_get_name(atom.catom, copy_to_string, &str);
    		return str;
    	}, "Get name of the Symbol or Variable atom");
	m.def("atom_get_object", [](CAtom atom) {
			return static_cast<GroundedObject const*>(atom_get_object(atom.catom))->pyobj;
		}, "Get object of the grounded atom");

	py::class_<CVecAtom>(m, "CVecAtom");
	m.def("vec_atom_new", []() { return CVecAtom(vec_atom_new()); }, "New vector of atoms");
	m.def("vec_atom_free", [](CVecAtom vec) { vec_atom_free(vec.cvec); }, "Free vector of atoms");
	m.def("vec_atom_push", [](CVecAtom vec, CAtom atom) { vec_atom_push(vec.cvec, atom_copy(atom.catom)); }, "Push atom into vector");
	m.def("vec_atom_pop", [](CVecAtom vec) { return CAtom(vec_atom_pop(vec.cvec)); }, "Push atom into vector");

	py::class_<CGroundingSpace>(m, "CGroundingSpace");
	m.def("grounding_space_new", []() { return CGroundingSpace(grounding_space_new()); }, "New grounding space instance");
	m.def("grounding_space_free", [](CGroundingSpace space) { grounding_space_free(space.cspace); }, "Free grounding space");
	m.def("grounding_space_add", [](CGroundingSpace space, CAtom atom) { grounding_space_add(space.cspace, atom_copy(atom.catom)); }, "Add atom into grounding space");
	m.def("grounding_space_eq", [](CGroundingSpace a, CGroundingSpace b) { return grounding_space_eq(a.cspace, b.cspace); }, "Check if two grounding spaces are equal");
}

