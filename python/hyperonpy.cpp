#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <hyperon/hyperon.h>

namespace py = pybind11;

template <typename T>
struct CPtr {
	using type = T;
	CPtr(T* ptr) : ptr(ptr) {}
	T* ptr;
};

using CAtom = CPtr<atom_t>;
using CVecAtom = CPtr<vec_atom_t>;
using CGroundingSpace = CPtr<grounding_space_t>;
using CSExprSpace = CPtr<sexpr_space_t>;

void copy_to_string(char const* cstr, void* context) {
	std::string* cppstr = static_cast<std::string*>(context);
	cppstr->assign(cstr);
}

void copy_atoms_to_list(atom_t const* const* atoms, size_t size, void* context) {
	py::list& list = *(py::list*) context;
	for (size_t i = 0; i < size; ++i) {
		list.append(CAtom(atom_copy(atoms[i])));
	}
}

extern "C" {
	const char *py_execute(const struct gnd_t* _gnd, struct vec_atom_t* ops, struct vec_atom_t* data);
	bool py_eq(const struct gnd_t* _a, const struct gnd_t* _b);
	struct gnd_t *py_clone(const struct gnd_t* _gnd);
	size_t py_display(const struct gnd_t* _gnd, char* buffer, size_t size);
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

py::object inc_ref(py::object obj) {
	obj.inc_ref();
	return obj;
}

const char *py_execute(const struct gnd_t* _cgnd, struct vec_atom_t* _ops, struct vec_atom_t* _data) {
	// Increment module reference counter otherwise SIGSEGV happens on exit
	static py::object hyperon = inc_ref(py::module_::import("hyperon"));
	static py::object BaseVecAtom = hyperon.attr("BaseVecAtom");
	py::object pyobj = static_cast<GroundedObject const*>(_cgnd)->pyobj;
	pyobj.attr("execute")(BaseVecAtom(CVecAtom(_ops)), BaseVecAtom(CVecAtom(_data)));
	// TODO: implement returning error
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

size_t py_display(const struct gnd_t* _cgnd, char* buffer, size_t size) {
	py::object pyobj = static_cast<GroundedObject const*>(_cgnd)->pyobj;
	std::string str = py::str(pyobj).cast<std::string>();
	strncpy(buffer, str.c_str(), size - 1);
	buffer[size - 1] = 0;
	return str.size();
}

void py_free(struct gnd_t* _cgnd) {
	delete _cgnd;
}

struct CConstr {

	py::object pyconstr;

	CConstr(py::object pyconstr) : pyconstr(pyconstr) {
		pyconstr.inc_ref();
	}

	static void free(void* ptr) {
		CConstr* self = static_cast<CConstr*>(ptr);
		self->pyconstr.dec_ref();
		delete self;
	}

	static atom_t* apply(char const* token, void* context) {
		CConstr* self = static_cast<CConstr*>(context);
		py::object atom = self->pyconstr(token);
		return atom_copy(atom.attr("catom").cast<CAtom>().ptr);
	}
};

PYBIND11_MODULE(hyperonpy, m) {
	m.doc() = "Python API of the Hyperon library";

	// TODO: integrate Rust logs with Python logger
	m.def("init_logger", &init_logger, "Initialize Hyperon library logger");

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
    			children[idx++] = atom_copy(atom.cast<CAtom&>().ptr);
    		}
    		return CAtom(atom_expr(children, size));
    	}, "Create expression atom");
    m.def("atom_gnd", [](py::object object) { return CAtom(atom_gnd(new GroundedObject(object))); }, "Create grounded atom");
    m.def("atom_free", [](CAtom atom) { atom_free(atom.ptr); }, "Free C atom");

    m.def("atom_eq", [](CAtom a, CAtom b) -> bool { return atom_eq(a.ptr, b.ptr); }, "Test if two atoms are equal");
    m.def("atom_to_str", [](CAtom atom) {
			std::string str;
    		atom_to_str(atom.ptr, &copy_to_string, &str);
    		return str;
    	}, "Convert atom to human readable string");
    m.def("atom_get_type", [](CAtom atom) { return atom_get_type(atom.ptr); }, "Get type of the atom");
    m.def("atom_get_name", [](CAtom atom) {
			std::string str;
    		atom_get_name(atom.ptr, &copy_to_string, &str);
    		return str;
    	}, "Get name of the Symbol or Variable atom");
	m.def("atom_get_object", [](CAtom atom) {
			return static_cast<GroundedObject const*>(atom_get_object(atom.ptr))->pyobj;
		}, "Get object of the grounded atom");

	py::class_<CVecAtom>(m, "CVecAtom");
	m.def("vec_atom_new", []() { return CVecAtom(vec_atom_new()); }, "New vector of atoms");
	m.def("vec_atom_free", [](CVecAtom vec) { vec_atom_free(vec.ptr); }, "Free vector of atoms");
	m.def("vec_atom_push", [](CVecAtom vec, CAtom atom) { vec_atom_push(vec.ptr, atom_copy(atom.ptr)); }, "Push atom into vector");
	m.def("vec_atom_pop", [](CVecAtom vec) { return CAtom(vec_atom_pop(vec.ptr)); }, "Push atom into vector");

	py::class_<CGroundingSpace>(m, "CGroundingSpace");
	m.def("grounding_space_new", []() { return CGroundingSpace(grounding_space_new()); }, "New grounding space instance");
	m.def("grounding_space_free", [](CGroundingSpace space) { grounding_space_free(space.ptr); }, "Free grounding space");
	m.def("grounding_space_add", [](CGroundingSpace space, CAtom atom) { grounding_space_add(space.ptr, atom_copy(atom.ptr)); }, "Add atom into grounding space");
	m.def("grounding_space_eq", [](CGroundingSpace a, CGroundingSpace b) { return grounding_space_eq(a.ptr, b.ptr); }, "Check if two grounding spaces are equal");
	m.def("grounding_space_len", [](CGroundingSpace space) { return grounding_space_len(space.ptr); }, "Return number of atoms in grounding space");
	m.def("grounding_space_get", [](CGroundingSpace space, size_t idx) { return CAtom(grounding_space_get(space.ptr, idx)); }, "Get atom by index from grounding space");
	m.def("grounding_space_query", [](CGroundingSpace space, CAtom pattern) {
			py::list results;
			grounding_space_query(space.ptr, pattern.ptr,
					[](binding_t const* cbindings, size_t size, void* context) {
						py::list& results = *(py::list*)context;
						py::dict pybindings;
						for (size_t i = 0; i < size; ++i) {
							pybindings[cbindings[i].var] = CAtom(atom_copy(cbindings[i].atom));
						}
						results.append(pybindings);
					}, &results);
			return results;
		}, "Query atoms from grounding space by pattern");



	m.def("grounding_space_subst", [](CGroundingSpace space, CAtom pattern, CAtom templ) {
			py::list results;
			grounding_space_subst(space.ptr, pattern.ptr, templ.ptr, &copy_atoms_to_list, &results);
			return results;
		}, "Get bindings for pattern and apply to template");

	py::class_<CSExprSpace>(m, "CSExprSpace");
	m.def("sexpr_space_new", []() { return CSExprSpace(sexpr_space_new()); }, "New sexpr space");
	m.def("sexpr_space_free", [](CSExprSpace space) { sexpr_space_free(space.ptr); }, "Free sexpr space");
	m.def("sexpr_space_register_token", [](CSExprSpace space, char const* regex, py::object constr) {
			droppable_t context = { new CConstr(constr), CConstr::free };
			sexpr_space_register_token(space.ptr, regex, &CConstr::apply, context);
		}, "Register sexpr space token");
	m.def("sexpr_space_add_str", [](CSExprSpace space, char const* str) { sexpr_space_add_str(space.ptr, str); }, "Add text to the sexpr space");
	m.def("sexpr_space_into_grounding_space", [](CSExprSpace tspace, CGroundingSpace gspace) { sexpr_space_into_grounding_space(tspace.ptr, gspace.ptr); }, "Add content of the sexpr space to the grounding space");

	m.def("interpret", [](CGroundingSpace space, CAtom expr) { 
			py::list results;
			interpret(space.ptr, expr.ptr, &copy_atoms_to_list, &results);
			return results;
		}, "Run interpreter on expression and return result");
}

