#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <hyperon/hyperon.h>

namespace py = pybind11;

template<class T, size_t N>
constexpr size_t lenghtof(T (&)[N]) { return N; }

template <typename T>
struct CPtr {
    using type = T;
    CPtr(T* ptr) : ptr(ptr) {}
    T* ptr;
};

using CAtom = CPtr<atom_t>;
using CVecAtom = CPtr<vec_atom_t>;
using CGroundingSpace = CPtr<grounding_space_t>;
using CTokenizer = CPtr<tokenizer_t>;
using CSExprSpace = CPtr<sexpr_space_t>;
using CStepResult = CPtr<step_result_t>;

static void copy_to_string(char const* cstr, void* context) {
    std::string* cppstr = static_cast<std::string*>(context);
    cppstr->assign(cstr);
}

static void copy_atoms(atom_array_t atoms, void* context) {
    py::list* list = static_cast<py::list*>(context);
    for (size_t i = 0; i < atoms.size; ++i) {
        list->append(CAtom(atom_clone(atoms.items[i])));
    }
}

py::object get_attr_or_fail(py::handle const& pyobj, char const* attr) {
    if (py::hasattr(pyobj, attr)) {
        return pyobj.attr(attr)();
    } else {
        std::string message = "Python object doesn't have a \"";
        message += attr;
        message += "\" attribute";
        throw std::runtime_error(message);
    }
}

extern "C" {
    exec_error_t *py_execute(const struct gnd_t* _gnd, struct vec_atom_t* args, struct vec_atom_t* ret);
    bool py_eq(const struct gnd_t* _a, const struct gnd_t* _b);
    struct gnd_t *py_clone(const struct gnd_t* _gnd);
    size_t py_display(const struct gnd_t* _gnd, char* buffer, size_t size);
    void py_free(struct gnd_t* _gnd);
}

const gnd_api_t PY_EXECUTABLE_API = { &py_execute, &py_eq, &py_clone, &py_display, &py_free };
const gnd_api_t PY_VALUE_API = { nullptr, &py_eq, &py_clone, &py_display, &py_free };

struct GroundedObject : gnd_t {
    GroundedObject(py::object pyobj, atom_t* typ) : pyobj(pyobj) {
        if (py::hasattr(pyobj, "execute")) {
            this->api = &PY_EXECUTABLE_API;
        } else {
            this->api = &PY_VALUE_API;
        }
        this->typ = typ;
    }
    virtual ~GroundedObject() {
        atom_free(this->typ);
    }
    py::object pyobj;
};

py::object inc_ref(py::object obj) {
    obj.inc_ref();
    return obj;
}

exec_error_t *py_execute(const struct gnd_t* _cgnd, struct vec_atom_t* _args, struct vec_atom_t* ret) {
    // Increment module reference counter otherwise SIGSEGV happens on exit
    static py::object hyperon = inc_ref(py::module_::import("hyperon"));
    static py::object call_execute_on_grounded_atom = hyperon.attr("call_execute_on_grounded_atom");
    static py::handle NoReduceError = hyperon.attr("NoReduceError");
    py::object pyobj = static_cast<GroundedObject const*>(_cgnd)->pyobj;
    CAtom pytyp = static_cast<GroundedObject const*>(_cgnd)->typ;
    try {
        py::list args;
        for (size_t i = 0; i < vec_atom_size(_args); ++i) {
            args.append(CAtom(atom_clone(vec_atom_get(_args, i))));
        }
        py::list result = call_execute_on_grounded_atom(pyobj, pytyp, args);
        for (auto& atom:  result) {
            vec_atom_push(ret, atom_clone(atom.attr("catom").cast<CAtom>().ptr));
        }
        return nullptr;
    } catch (py::error_already_set &e) {
        if (e.matches(NoReduceError)) {
            return exec_error_no_reduce();
        } else {
            char message[4096];
            snprintf(message, lenghtof(message), "Exception caught:\n%s", e.what());
            return exec_error_runtime(message);
        }
    }
}

bool py_eq(const struct gnd_t* _a, const struct gnd_t* _b) {
    py::object a = static_cast<GroundedObject const*>(_a)->pyobj;
    py::object b = static_cast<GroundedObject const*>(_b)->pyobj;
    return a.equal(b);
}

struct gnd_t *py_clone(const struct gnd_t* _cgnd) {
    GroundedObject const* cgnd = static_cast<GroundedObject const*>(_cgnd);
    py::object pyobj = cgnd->pyobj;
    py::object copy = pyobj.attr("copy")();
    atom_t* typ = atom_clone(cgnd->typ);
    return new GroundedObject(copy, typ);
}

size_t py_display(const struct gnd_t* _cgnd, char* buffer, size_t size) {
    py::object pyobj = static_cast<GroundedObject const*>(_cgnd)->pyobj;
    std::string str = py::str(pyobj).cast<std::string>();
    strncpy(buffer, str.c_str(), size - 1);
    buffer[size - 1] = 0;
    return str.size();
}

void py_free(struct gnd_t* _cgnd) {
    delete static_cast<GroundedObject const*>(_cgnd);
}

struct CConstr {

    py::object pyconstr;

    CConstr(py::object pyconstr) : pyconstr(pyconstr) { }

    static void free(void* ptr) {
        CConstr* self = static_cast<CConstr*>(ptr);
        delete self;
    }

    static atom_t* apply(char const* token, void* context) {
        CConstr* self = static_cast<CConstr*>(context);
        py::object atom = self->pyconstr(token);
        return atom_clone(atom.attr("catom").cast<CAtom>().ptr);
    }
};

struct CSExprParser {

    std::string text;
    sexpr_parser_t* ptr;

    CSExprParser(std::string text) : text(text) {
        ptr = sexpr_parser_new(this->text.c_str());
    }

    virtual ~CSExprParser() {
        sexpr_parser_free(ptr);
    }

    py::object parse(CTokenizer tokenizer) {
        atom_t* atom = sexpr_parser_parse(this->ptr, tokenizer.ptr);
        return atom ? py::cast(CAtom(atom)) : py::none();
    }
};

struct CAtomType {};

PYBIND11_MODULE(hyperonpy, m) {
    m.doc() = "Python API of the Hyperon library";

    // TODO: integrate Rust logs with Python logger
    m.def("init_logger", &init_logger, "Initialize Hyperon library logger");

    py::enum_<atom_type_t>(m, "AtomKind")
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
            for (auto& atom : _children) {
                // Copying atom is required because atom_expr() moves children
                // catoms inside new expression atom.
                children[idx++] = atom_clone(atom.cast<CAtom&>().ptr);
            }
            return CAtom(atom_expr(children, size));
        }, "Create expression atom");
    m.def("atom_gnd", [](py::object object, CAtom ctyp) {
            atom_t* typ = atom_clone(ctyp.ptr);
            return CAtom(atom_gnd(new GroundedObject(object, typ)));
            }, "Create grounded atom");
    m.def("atom_free", [](CAtom atom) { atom_free(atom.ptr); }, "Free C atom");

    m.def("atom_eq", [](CAtom a, CAtom b) -> bool { return atom_eq(a.ptr, b.ptr); }, "Test if two atoms are equal");
    m.def("atom_to_str", [](CAtom atom) {
            std::string str;
            atom_to_str(atom.ptr, copy_to_string, &str);
            return str;
        }, "Convert atom to human readable string");
    m.def("atom_get_type", [](CAtom atom) { return atom_get_type(atom.ptr); }, "Get type of the atom");
    m.def("atom_get_name", [](CAtom atom) {
            std::string str;
            atom_get_name(atom.ptr, copy_to_string, &str);
            return str;
        }, "Get name of the Symbol or Variable atom");
    m.def("atom_get_object", [](CAtom atom) {
            return static_cast<GroundedObject const*>(atom_get_object(atom.ptr))->pyobj;
        }, "Get object of the grounded atom");
    m.def("atom_get_grounded_type", [](CAtom atom) {
            return CAtom(atom_get_grounded_type(atom.ptr));
        }, "Get object of the grounded atom");
    m.def("atom_get_children", [](CAtom atom) {
            py::list atoms;
            atom_get_children(atom.ptr, copy_atoms, &atoms);
            return atoms;
        }, "Get children atoms of the expression");
    m.def("atoms_are_equivalent", [](CAtom first, CAtom second) {
            return atoms_are_equivalent(first.ptr, second.ptr); },
            "Check atom for equivalence");

    py::class_<CVecAtom>(m, "CVecAtom");
    m.def("vec_atom_new", []() { return CVecAtom(vec_atom_new()); }, "New vector of atoms");
    m.def("vec_atom_free", [](CVecAtom vec) { vec_atom_free(vec.ptr); }, "Free vector of atoms");
    m.def("vec_atom_size", [](CVecAtom vec) { return vec_atom_size(vec.ptr); }, "Return size of the vector");
    m.def("vec_atom_push", [](CVecAtom vec, CAtom atom) { vec_atom_push(vec.ptr, atom_clone(atom.ptr)); }, "Push atom into vector");
    m.def("vec_atom_pop", [](CVecAtom vec) { return CAtom(vec_atom_pop(vec.ptr)); }, "Push atom into vector");

    py::class_<CGroundingSpace>(m, "CGroundingSpace");
    m.def("grounding_space_new", []() { return CGroundingSpace(grounding_space_new()); }, "New grounding space instance");
    m.def("grounding_space_free", [](CGroundingSpace space) { grounding_space_free(space.ptr); }, "Free grounding space");
    m.def("grounding_space_add", [](CGroundingSpace space, CAtom atom) { grounding_space_add(space.ptr, atom_clone(atom.ptr)); }, "Add atom into grounding space");
    m.def("grounding_space_remove", [](CGroundingSpace space, CAtom atom) { return grounding_space_remove(space.ptr, atom.ptr); }, "Remove atom from grounding space");
    m.def("grounding_space_replace", [](CGroundingSpace space, CAtom from, CAtom to) { return grounding_space_replace(space.ptr, from.ptr, atom_clone(to.ptr)); }, "Replace atom from grounding space");
    m.def("grounding_space_eq", [](CGroundingSpace a, CGroundingSpace b) { return grounding_space_eq(a.ptr, b.ptr); }, "Check if two grounding spaces are equal");
    m.def("grounding_space_len", [](CGroundingSpace space) { return grounding_space_len(space.ptr); }, "Return number of atoms in grounding space");
    m.def("grounding_space_get", [](CGroundingSpace space, size_t idx) { return CAtom(grounding_space_get(space.ptr, idx)); }, "Get atom by index from grounding space");
    m.def("grounding_space_query", [](CGroundingSpace space, CAtom pattern) {
            py::list results;
            grounding_space_query(space.ptr, pattern.ptr,
                    [](binding_array_t cbindings, void* context) {
                        py::list& results = *(py::list*)context;
                        py::dict pybindings;
                        for (size_t i = 0; i < cbindings.size; ++i) {
                            pybindings[cbindings.items[i].var] = CAtom(atom_clone(cbindings.items[i].atom));
                        }
                        results.append(pybindings);
                    }, &results);
            return results;
        }, "Query atoms from grounding space by pattern");



    m.def("grounding_space_subst", [](CGroundingSpace space, CAtom pattern, CAtom templ) {
            py::list atoms;
            grounding_space_subst(space.ptr, pattern.ptr, templ.ptr, copy_atoms, &atoms);
            return atoms;
        }, "Get bindings for pattern and apply to template");

    py::class_<CTokenizer>(m, "CTokenizer");
    m.def("tokenizer_new", []() { return CTokenizer(tokenizer_new()); }, "New tokenizer");
    m.def("tokenizer_free", [](CTokenizer tokenizer) { tokenizer_free(tokenizer.ptr); }, "Free tokenizer");
    m.def("tokenizer_clone", [](CTokenizer tokenizer) { tokenizer_clone(tokenizer.ptr); }, "Clone tokenizer");
    m.def("tokenizer_register_token", [](CTokenizer tokenizer, char const* regex, py::object constr) {
            droppable_t context = { new CConstr(constr), CConstr::free };
            tokenizer_register_token(tokenizer.ptr, regex, &CConstr::apply, context);
        }, "Register token");

    py::class_<CSExprParser>(m, "CSExprParser")
        .def(py::init<std::string>())
        .def("parse", &CSExprParser::parse,  "Return next parser atom or None");

    py::class_<CSExprSpace>(m, "CSExprSpace");
    m.def("sexpr_space_new", [](CTokenizer tokenizer) { return CSExprSpace(sexpr_space_new(tokenizer_clone(tokenizer.ptr))); }, "New sexpr space");
    m.def("sexpr_space_free", [](CSExprSpace space) { sexpr_space_free(space.ptr); }, "Free sexpr space");
    m.def("sexpr_space_add_str", [](CSExprSpace space, char const* str) { sexpr_space_add_str(space.ptr, str); }, "Add text to the sexpr space");
    m.def("sexpr_space_into_grounding_space", [](CSExprSpace tspace, CGroundingSpace gspace) { sexpr_space_into_grounding_space(tspace.ptr, gspace.ptr); }, "Add content of the sexpr space to the grounding space");

    py::class_<CStepResult>(m, "CStepResult")
        .def("__str__", [](CStepResult step) {
            std::string str;
            step_to_str(step.ptr, copy_to_string, &str);
            return str;
        }, "Convert step to human readable string");
    m.def("interpret_init", [](CGroundingSpace space, CAtom expr) {
            return CStepResult(interpret_init(space.ptr, expr.ptr));
        }, "Initialize interpreter of the expression");
    m.def("interpret_step", [](CStepResult step) {
            return CStepResult(interpret_step(step.ptr));
        }, "Do next step of the interpretataion");
    m.def("step_has_next", [](CStepResult step) {
            return step_has_next(step.ptr);
        }, "Check whether next step of interpretation is posible");
    m.def("step_get_result", [](CStepResult step) {
            py::list atoms;
            step_get_result(step.ptr, copy_atoms, &atoms);
            return atoms;
        }, "Return result of the interpretation");

#define ADD_TYPE(t, d) .def_property_readonly_static(#t, [](py::object) { return CAtom(atom_clone(ATOM_TYPE_ ## t)); }, d " atom type")

    py::class_<CAtomType>(m, "CAtomType")
        ADD_TYPE(UNDEFINED, "Undefined")
        ADD_TYPE(TYPE, "Type")
        ADD_TYPE(ATOM, "Generic")
        ADD_TYPE(SYMBOL, "Symbol")
        ADD_TYPE(VARIABLE, "Variable")
        ADD_TYPE(EXPRESSION, "Expression")
        ADD_TYPE(GROUNDED, "Grounded");
    m.def("check_type", [](CGroundingSpace space, CAtom atom, CAtom type) { 
            return check_type(space.ptr, atom.ptr, type.ptr);
        }, "Check if atom is an instance of the passed type");
    m.def("validate_atom", [](CGroundingSpace space, CAtom atom) {
            return validate_atom(space.ptr, atom.ptr);
        }, "Validate expression arguments correspond to the operation type");
    m.def("get_atom_types", [](CGroundingSpace space, CAtom atom) {
            py::list atoms;
            get_atom_types(space.ptr, atom.ptr, copy_atoms, &atoms);
            return atoms;
        }, "Get types of the given atom");
}

