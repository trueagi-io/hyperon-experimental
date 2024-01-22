#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <nonstd/optional.hpp>

#include <hyperon/hyperon.h>

namespace py = pybind11;

namespace PYBIND11_NAMESPACE { namespace detail {
    template <typename T>
    struct type_caster<nonstd::optional<T>> : optional_caster<nonstd::optional<T>> {};
}}

template<class T, size_t N>
constexpr size_t lenghtof(T (&)[N]) { return N; }

template <typename T>
struct CPtr {
    CPtr(T* ptr) : ptr(ptr) {}
    T* ptr;
};

template <typename T>
struct CStruct {
    CStruct(T obj) : obj(obj) {}
    T obj;
    T* ptr () { return &(this->obj); }
};

using CAtom = CStruct<atom_t>;
using CVecAtom = CStruct<atom_vec_t>;
using CBindings = CStruct<bindings_t>;
using CBindingsSet = CStruct<bindings_set_t>;
using CSpace = CStruct<space_t>;
using CTokenizer = CStruct<tokenizer_t>;
using CSyntaxNode = CStruct<syntax_node_t>;
using CStepResult = CStruct<step_result_t>;
using CRunnerState = CStruct<runner_state_t>;
using CMetta = CStruct<metta_t>;
using EnvBuilder = CStruct<env_builder_t>;

// Returns a string, created by executing a function that writes string data into a buffer
typedef size_t (*write_to_buf_func_t)(void*, char*, size_t);
std::string func_to_string(write_to_buf_func_t func, void* arg) {
    //First try with a 1K stack buffer, because that will work in the vast majority of cases
    char dst_buf[1024];
    size_t len = func(arg, dst_buf, 1024);
    if (len < 1024) {
        return std::string(dst_buf);
    } else {
        char* data = new char[len+1];
        func(arg, data, len+1);
        std::string new_string = std::string(data);
        return new_string;
    }
}

// Similar to func_to_string, but for functions that don't take any args
typedef size_t (*write_to_buf_no_arg_func_t)(char*, size_t);
std::string func_to_string_no_arg(write_to_buf_no_arg_func_t func) {
    //First try with a 1K stack buffer, because that will work in the vast majority of cases
    char dst_buf[1024];
    size_t len = func(dst_buf, 1024);
    if (len < 1024) {
        return std::string(dst_buf);
    } else {
        char* data = new char[len+1];
        func(data, len+1);
        std::string new_string = std::string(data);
        return new_string;
    }
}

// Similar to func_to_string, but for functions that that take two args
typedef size_t (*write_to_buf_two_arg_func_t)(void*, void*, char*, size_t);
std::string func_to_string_two_args(write_to_buf_two_arg_func_t func, void* arg1, void* arg2) {
    //First try with a 1K stack buffer, because that will work in the vast majority of cases
    char dst_buf[1024];
    size_t len = func(arg1, arg2, dst_buf, 1024);
    if (len < 1024) {
        return std::string(dst_buf);
    } else {
        char* data = new char[len+1];
        func(arg1, arg2, data, len+1);
        std::string new_string = std::string(data);
        return new_string;
    }
}

static void copy_atoms(const atom_vec_t* atoms, void* context) {
    py::list* list = static_cast<py::list*>(context);
    for (size_t i = 0; i < atom_vec_len(atoms); ++i) {
        atom_ref_t atom = atom_vec_get(atoms, i);
        list->append(CAtom(atom_clone(&atom)));
    }
}

static void copy_atom_to_dict(atom_ref_t var, atom_ref_t atom, void* context) {
    py::dict& pybindings = *static_cast<py::dict*>(context);
    std::string var_name = func_to_string((write_to_buf_func_t)&atom_get_name, &var);
    pybindings[var_name.c_str()] = CAtom(atom_clone(&atom));
}

static void copy_lists_of_atom(const atom_vec_t* atoms, void* context) {
    py::list* list_of_lists = static_cast<py::list*>(context);
    py::list list;
    copy_atoms(atoms, &list);
    list_of_lists->append(list);
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
    exec_error_t py_execute(const struct gnd_t* _gnd, const struct atom_vec_t* args, struct atom_vec_t* ret);
    bindings_set_t py_match_(const struct gnd_t *_gnd, const atom_ref_t *_atom);
    bool py_eq(const struct gnd_t* _a, const struct gnd_t* _b);
    struct gnd_t *py_clone(const struct gnd_t* _gnd);
    size_t py_display(const struct gnd_t* _gnd, char* buffer, size_t size);
    void py_free(struct gnd_t* _gnd);
}

const gnd_api_t PY_EXECUTABLE_MATCHABLE_API = { &py_execute, &py_match_, &py_eq, &py_clone, &py_display, &py_free };
const gnd_api_t PY_EXECUTABLE_API = { &py_execute, nullptr, &py_eq, &py_clone, &py_display, &py_free };
const gnd_api_t PY_MATCHABLE_API = { nullptr, &py_match_, &py_eq, &py_clone, &py_display, &py_free };
const gnd_api_t PY_VALUE_API = { nullptr, nullptr, &py_eq, &py_clone, &py_display, &py_free };

struct GroundedObject : gnd_t {
    GroundedObject(py::object pyobj, atom_t typ) : pyobj(pyobj) {
        if (py::hasattr(pyobj, "execute") && py::hasattr(pyobj, "match_")) {
            this->api = &PY_EXECUTABLE_MATCHABLE_API;
        } else if (py::hasattr(pyobj, "execute")) {
            this->api = &PY_EXECUTABLE_API;
        } else if (py::hasattr(pyobj, "match_")) {
            this->api = &PY_MATCHABLE_API;
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

exec_error_t py_execute(const struct gnd_t* _cgnd, const struct atom_vec_t* _args, struct atom_vec_t* ret) {
    py::object hyperon = py::module_::import("hyperon.atoms");
    py::function call_execute_on_grounded_atom = hyperon.attr("_priv_call_execute_on_grounded_atom");
    py::handle NoReduceError = hyperon.attr("NoReduceError");
    py::object pyobj = static_cast<GroundedObject const*>(_cgnd)->pyobj;
    CAtom pytyp = static_cast<GroundedObject const*>(_cgnd)->typ;
    try {
        py::list args;
        for (size_t i = 0; i < atom_vec_len(_args); ++i) {
            atom_ref_t arg_atom_ref = atom_vec_get(_args, i);
            args.append(CAtom(atom_clone(&arg_atom_ref)));
        }
        py::list result = call_execute_on_grounded_atom(pyobj, pytyp, args);
        for (py::handle atom:  result) {
            if (!py::hasattr(atom, "catom")) {
                return exec_error_runtime("Grounded operation which is defined using unwrap=False should return atom instead of Python type");
            }
            atom_vec_push(ret, atom_clone(atom.attr("catom").cast<CAtom>().ptr()));
        }
        return exec_error_no_err();
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

bindings_set_t py_match_(const struct gnd_t *_gnd, const atom_ref_t *_atom) {
    py::object hyperon = py::module_::import("hyperon.atoms");
    py::function call_match_on_grounded_atom = hyperon.attr("_priv_call_match_on_grounded_atom");

    py::object pyobj = static_cast<GroundedObject const *>(_gnd)->pyobj;
    CAtom catom = atom_clone(_atom);
    py::list results = call_match_on_grounded_atom(pyobj, catom);

    struct bindings_set_t result_set = bindings_set_empty();
    for (py::handle result: results) {
        py::dict pybindings = result.cast<py::dict>();

        struct bindings_t cbindings = bindings_new();
        for (auto var_atom : pybindings) {
            const std::string var  = var_atom.first.cast<py::str>();
            atom_t atom = atom_clone(var_atom.second.attr("catom").cast<CAtom>().ptr());
            bindings_add_var_binding(&cbindings, atom_var(var.c_str()), atom);
        }

        bindings_set_push(&result_set, cbindings);
    }

    return result_set;
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
    atom_t typ = atom_clone(&cgnd->typ);
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

extern "C" {
    bindings_set_t py_space_query(const struct space_params_t *params, const atom_ref_t *atom);
    atom_vec_t *py_space_subst(const struct space_params_t *params, const atom_ref_t *pattern, const atom_ref_t *tmpl);
    void py_space_add(const struct space_params_t *params, atom_t atom);
    bool py_space_remove(const struct space_params_t *params, const atom_ref_t *atom);
    bool py_space_replace(const struct space_params_t *params, const atom_ref_t *from, atom_t to);
    ssize_t py_space_atom_count(const struct space_params_t *params);
    void *py_space_new_atom_iter_state(const struct space_params_t *params);
    atom_ref_t py_space_iter_next_atom(const struct space_params_t *params, void *state);
    void py_space_free_atom_iter_state(const struct space_params_t *params, void *state);
    void py_space_free_payload(void *payload);
}

const space_api_t PY_SPACE_NO_SUBST_API = {
    &py_space_query,
    NULL, //TODO: &py_space_subst
    &py_space_add,
    &py_space_remove,
    &py_space_replace,
    &py_space_atom_count,
    &py_space_new_atom_iter_state,
    &py_space_iter_next_atom,
    &py_space_free_atom_iter_state,
    &py_space_free_payload };

struct PySpace {
    PySpace(py::object pyobj) : pyobj(pyobj) {
    }
    virtual ~PySpace() {
    }
    py::object pyobj;
};

bindings_set_t py_space_query(const struct space_params_t *params, const atom_ref_t *query_atom) {
    py::object hyperon = py::module_::import("hyperon.base");
    py::function call_query_on_python_space = hyperon.attr("_priv_call_query_on_python_space");
    py::object pyobj = static_cast<PySpace const *>(params->payload)->pyobj;
    CAtom catom = atom_clone(query_atom);
    py::object result = call_query_on_python_space(pyobj, catom);
    CBindingsSet set = result.attr("c_set").cast<CBindingsSet>();
    return bindings_set_clone(set.ptr());
}

//TODO, currently Python spaces use the default subst implementation
// atom_vec_t *py_space_subst(const struct space_params_t *params, const struct atom_t *pattern, const struct atom_t *tmpl) {
//     //TODO
// }

void py_space_add(const struct space_params_t *params, atom_t atom) {
    py::object hyperon = py::module_::import("hyperon.base");
    py::function call_add_on_python_space = hyperon.attr("_priv_call_add_on_python_space");
    py::object pyobj = static_cast<PySpace const *>(params->payload)->pyobj;
    atom_t notify_atom = atom_clone(&atom);
    CAtom catom = atom;
    call_add_on_python_space(pyobj, catom);

    //TODO: Create a mechanism so the Python code can do the notification manually, and bypass this
    // automatic notification code
    space_event_t event = space_event_new_add(notify_atom);
    space_params_notify_all_observers(params, &event);
    space_event_free(event);
}

bool py_space_remove(const struct space_params_t *params, const atom_ref_t *atom) {
    py::object hyperon = py::module_::import("hyperon.base");
    py::function call_remove_on_python_space = hyperon.attr("_priv_call_remove_on_python_space");
    py::object pyobj = static_cast<PySpace const *>(params->payload)->pyobj;
    atom_t notify_atom = atom_clone(atom);
    CAtom catom = atom_clone(atom);
    py::object result = call_remove_on_python_space(pyobj, catom);
    if (result.cast<bool>()) {
        //TODO: See comment about manual notification above
        space_event_t event = space_event_new_remove(notify_atom);
        space_params_notify_all_observers(params, &event);
        space_event_free(event);
        return true;
    } else {
        atom_free(notify_atom);
        return false;
    }
}

bool py_space_replace(const struct space_params_t *params, const atom_ref_t *from, atom_t to) {
    py::object hyperon = py::module_::import("hyperon.base");
    py::function call_replace_on_python_space = hyperon.attr("_priv_call_replace_on_python_space");
    py::object pyobj = static_cast<PySpace const *>(params->payload)->pyobj;
    atom_t notify_from = atom_clone(from);
    atom_t notify_to = atom_clone(&to);
    CAtom catom_from = atom_clone(from);
    CAtom catom_to = to;
    py::object result = call_replace_on_python_space(pyobj, catom_from, catom_to);
    if (result.cast<bool>()) {
        //TODO: See comment about manual notification above
        space_event_t event = space_event_new_replace(notify_from, notify_to);
        space_params_notify_all_observers(params, &event);
        space_event_free(event);
        return true;
    } else {
        atom_free(notify_from);
        atom_free(notify_to);
        return false;
    }
}

ssize_t py_space_atom_count(const struct space_params_t *params) {
    py::object hyperon = py::module_::import("hyperon.base");
    py::function call_atom_count_on_python_space = hyperon.attr("_priv_call_atom_count_on_python_space");
    py::object pyobj = static_cast<PySpace const *>(params->payload)->pyobj;
    py::int_ result = call_atom_count_on_python_space(pyobj);
    return result.cast<ssize_t>();
}

void *py_space_new_atom_iter_state(const struct space_params_t *params) {
    py::object hyperon = py::module_::import("hyperon.base");
    py::function call_new_iter_state_on_python_space = hyperon.attr("_priv_call_new_iter_state_on_python_space");
    py::object pyobj = static_cast<PySpace const *>(params->payload)->pyobj;
    py::object result = call_new_iter_state_on_python_space(pyobj);
    if (result.is_none()) {
        return NULL;
    } else {
        py::function iter_init_fn = result.attr("__iter__");
        iter_init_fn();
        py::object* iter_buf = new py::object(result);
        return (void*)iter_buf;
    }
}

atom_ref_t py_space_iter_next_atom(const struct space_params_t *params, void *state) {
    py::object* iter_buf = (py::object*)state;
    py::function next_fn = iter_buf->attr("__next__");
    try {
        py::object atom = next_fn();
        return atom_ref(atom.attr("catom").cast<CAtom>().ptr());
    } catch (pybind11::error_already_set &e) {
        if (e.matches(PyExc_StopIteration)) {
            return atom_ref_null();
        } else {
            throw;
        }
    }
}

void py_space_free_atom_iter_state(const struct space_params_t *params, void *state) {
    py::object* iter_buf = (py::object*)state;
    delete iter_buf;
}

void py_space_free_payload(void *payload) {
    delete static_cast<PySpace const*>(payload);
}

void copy_to_list_callback(atom_ref_t var, atom_ref_t atom, void* context){

    pybind11::list& var_atom_list = *( (pybind11::list*)(context) );

    var_atom_list.append(
            std::make_pair(
                func_to_string((write_to_buf_func_t)&atom_get_name, &var),
                CAtom(atom)));
}

void atom_copy_to_list_callback(atom_ref_t atom, void* context){
    pybind11::list& atoms_list = *( (pybind11::list*)(context) );
    atoms_list.append(CAtom(atom_clone(&atom)));
}

void bindings_copy_to_list_callback(bindings_t* bindings, void* context){
    pybind11::list& bindings_list = *( (pybind11::list*)(context) );
    bindings_list.append(CBindings(bindings_clone(bindings)));
}

void syntax_node_copy_to_list_callback(const syntax_node_t* node, void *context) {
    pybind11::list& nodes_list = *( (pybind11::list*)(context) );
    if (syntax_node_is_leaf(node)) {
        nodes_list.append(CSyntaxNode(syntax_node_clone(node)));
    }
};

void run_python_loader_callback(metta_t* metta, void* context) {
    py::object runner_mod = py::module_::import("hyperon.runner");
    py::object metta_class = runner_mod.attr("MeTTa");
    py::function load_metta_py_stdlib = metta_class.attr("_priv_load_metta_py_stdlib");
    metta_t cloned_metta = metta_clone_handle(metta);
    py::object py_metta = metta_class(CMetta(cloned_metta));
    load_metta_py_stdlib(py_metta);
}

struct CConstr {

    py::function pyconstr;

    CConstr(py::function pyconstr) : pyconstr(pyconstr) { }

    static void free(void* ptr) {
        CConstr* self = static_cast<CConstr*>(ptr);
        delete self;
    }

    static atom_t apply(char const* token, void* context) {
        CConstr* self = static_cast<CConstr*>(context);
        py::object atom = self->pyconstr(token);
        return atom_clone(atom.attr("catom").cast<CAtom>().ptr());
    }
};

static token_api_t TOKEN_API = { .construct_atom = &CConstr::apply, .free_context = &CConstr::free };

struct CSExprParser {

    std::string text;
    sexpr_parser_t parser;

    CSExprParser(std::string text) : text(text) {
        parser = sexpr_parser_new(this->text.c_str());
    }

    virtual ~CSExprParser() {
        sexpr_parser_free(parser);
    }

    sexpr_parser_t* ptr () { return &(this->parser); }

    py::object parse(CTokenizer tokenizer) {
        atom_t atom = sexpr_parser_parse(&this->parser, tokenizer.ptr());
        return !atom_is_null(&atom) ? py::cast(CAtom(atom)) : py::none();
    }

    py::object err_str() {
        const char* err_str = sexpr_parser_err_str(&this->parser);
        return err_str != NULL ? py::cast(std::string(err_str)) : py::none();
    }

    py::object parse_to_syntax_tree() {
        syntax_node_t root_node = sexpr_parser_parse_to_syntax_tree(&this->parser);
        return !syntax_node_is_null(&root_node) ? py::cast(CSyntaxNode(root_node)) : py::none();
    }
};

struct CAtomType {};
struct CAtoms {};

PYBIND11_MODULE(hyperonpy, m) {
    m.doc() = "Python API of the Hyperon library";

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
            atom_t children[size];
            int idx = 0;
            for (py::handle atom : _children) {
                // Copying atom is required because atom_expr() moves children
                // catoms inside new expression atom.
                children[idx++] = atom_clone(atom.cast<CAtom&>().ptr());
            }
            return CAtom(atom_expr(children, size));
        }, "Create expression atom");
    m.def("atom_gnd", [](py::object object, CAtom ctyp) {
            if (py::hasattr(object, "cspace")) {
                //TODO: We should make static constant type atoms, so we don't need to allocate and then
                // free them, just to test a constant
                atom_t undefined = ATOM_TYPE_UNDEFINED();
                if (!atom_eq(ctyp.ptr(), &undefined)) {
                    throw std::runtime_error("Grounded Space Atoms can't have a custom type");
                }
                atom_free(undefined);
                space_t* space = object.attr("cspace").cast<CSpace&>().ptr();
                return CAtom(atom_gnd_for_space(space));
            } else {
                atom_t typ = atom_clone(ctyp.ptr());
                return CAtom(atom_gnd(new GroundedObject(object, typ)));
            }
            }, "Create grounded atom");
    m.def("atom_free", [](CAtom atom) { atom_free(atom.obj); }, "Free C atom");

    m.def("atom_eq", [](CAtom& a, CAtom& b) -> bool { return atom_eq(a.ptr(), b.ptr()); }, "Test if two atoms are equal");
    m.def("atom_is_error", [](CAtom& atom) -> bool { return atom_is_error(atom.ptr()); }, "Returns True if an atom is a MeTTa error expression");
    m.def("atom_error_message", [](CAtom& atom) {
            return func_to_string((write_to_buf_func_t)&atom_error_message, atom.ptr());
        }, "Renders the error message from an error expression atom");
    m.def("atom_to_str", [](CAtom& atom) {
            return func_to_string((write_to_buf_func_t)&atom_to_str, atom.ptr());
        }, "Convert atom to human readable string");
    m.def("atom_get_type", [](CAtom& atom) { return atom_get_type(atom.ptr()); }, "Get type of the atom");
    m.def("atom_get_name", [](CAtom& atom) {
            return func_to_string((write_to_buf_func_t)&atom_get_name, atom.ptr());
        }, "Get name of the Symbol or Variable atom");
    m.def("atom_get_space", [](CAtom& atom) {
            return CSpace(atom_get_space(atom.ptr()));
        }, "Get the space inside of a Grounded atom wrapping a space");
    m.def("atom_get_object", [](CAtom& atom) {
            return static_cast<GroundedObject const*>(atom_get_object(atom.ptr()))->pyobj;
        }, "Get object of the grounded atom");
    m.def("atom_get_grounded_type", [](CAtom& atom) {
            return CAtom(atom_get_grounded_type(atom.ptr()));
        }, "Get object of the grounded atom");
    m.def("atom_get_children", [](CAtom& atom) {
            py::list atoms;
            atom_get_children(atom.ptr(), copy_atoms, &atoms);
            return atoms;
        }, "Get children atoms of the expression");
    m.def("atom_iterate", [](CAtom& atom) -> pybind11::list {
            pybind11::list atoms_list;
            atom_iterate(atom.ptr(), atom_copy_to_list_callback, &atoms_list);
            return atoms_list;
        }, "Returns iterator to traverse child atoms recursively, depth first");
    m.def("atom_match_atom", [](CAtom& a, CAtom& b) -> CBindingsSet {
            return CBindingsSet(atom_match_atom(a.ptr(), b.ptr()));
        }, "Matches one atom against another, establishing Bindings between variables");
    m.def("atoms_are_equivalent", [](CAtom& first, CAtom& second) {
            return atoms_are_equivalent(first.ptr(), second.ptr());
        }, "Check atom for equivalence");

    py::class_<CVecAtom>(m, "CVecAtom");
    m.def("atom_vec_from_list", [](pybind11::list pylist) {
        atom_vec_t new_vec = atom_vec_new();
        for(py::handle pyobj : pylist) {
            py::handle atom_pyhandle = pyobj.attr("catom");
            CAtom atom = atom_pyhandle.cast<CAtom>();
            atom_vec_push(&new_vec, atom_clone(atom.ptr()));
        }
        return CVecAtom(new_vec);
    }, "Create a vector of atoms from a Python list");
    m.def("atom_vec_new", []() { return CVecAtom(atom_vec_new()); }, "New vector of atoms");
    m.def("atom_vec_free", [](CVecAtom& vec) { atom_vec_free(vec.obj); }, "Free vector of atoms");
    m.def("atom_vec_len", [](CVecAtom& vec) { return atom_vec_len(vec.ptr()); }, "Return size of the vector");
    m.def("atom_vec_push", [](CVecAtom& vec, CAtom atom) { atom_vec_push(vec.ptr(), atom_clone(atom.ptr())); }, "Push atom into vector");
    m.def("atom_vec_pop", [](CVecAtom& vec) { return CAtom(atom_vec_pop(vec.ptr())); }, "Push atom into vector");

    py::class_<CBindings>(m, "CBindings");
    m.def("bindings_new", []() { return CBindings(bindings_new()); }, "New bindings");
    m.def("bindings_free", [](CBindings bindings) { bindings_free(bindings.obj);}, "Free bindings" );
    m.def("bindings_clone", [](CBindings bindings) { return CBindings(bindings_clone(bindings.ptr())); }, "Deep copy of bindings");
    m.def("bindings_merge", [](CBindings self, CBindings other) {
        return CBindingsSet(bindings_merge(bindings_clone(self.ptr()), other.ptr()));
    }, "Merges bindings into a BindingsSet, allowing for conflicting bindings to split");
    m.def("bindings_eq", [](CBindings left, CBindings right){ return bindings_eq(left.ptr(), right.ptr());}, "Compares bindings"  );
    m.def("bindings_add_var_binding",
          [](CBindings bindings, char const* varName, CAtom atom) {
              return bindings_add_var_binding(bindings.ptr(), atom_var(varName), atom_clone(atom.ptr()));
          },
          "Links variable to atom" );
    m.def("bindings_is_empty", [](CBindings bindings){ return bindings_is_empty(bindings.ptr());}, "Returns true if bindings is empty");

    m.def("bindings_narrow_vars", [](CBindings bindings, CVecAtom& vars) {
            bindings_narrow_vars(bindings.ptr(), vars.ptr());
        }, "Remove vars from Bindings, except those specified" );

    m.def("bindings_resolve", [](CBindings bindings, char const* varName) -> nonstd::optional<CAtom> {
            auto const res = bindings_resolve(bindings.ptr(), varName);
            return atom_is_null(&res) ? nonstd::nullopt : nonstd::optional<CAtom>(CAtom(res));
        }, "Resolve" );

    m.def("bindings_to_str", [](CBindings bindings) {
        return func_to_string((write_to_buf_func_t)&bindings_to_str, bindings.ptr());
    }, "Convert bindings to human readable string");

    m.def("bindings_list", [](CBindings bindings) -> pybind11::list {
        pybind11::list var_atom_list;
        bindings_traverse(
                bindings.ptr(),
                copy_to_list_callback,
                &var_atom_list);

        return var_atom_list;
    }, "Returns iterator to traverse bindings");

    py::class_<CBindingsSet>(m, "CBindingsSet");
    m.def("bindings_set_empty", []() { return CBindingsSet(bindings_set_empty()); }, "New BindingsSet with no Bindings");
    m.def("bindings_set_single", []() { return CBindingsSet(bindings_set_single()); }, "New BindingsSet with one new Bindings");
    m.def("bindings_set_free", [](CBindingsSet& set) { bindings_set_free(set.obj); }, "Free BindingsSet");
    m.def("bindings_set_eq", [](CBindingsSet& set, CBindingsSet& other) { return bindings_set_eq(set.ptr(), other.ptr()); }, "Free BindingsSet");
    //TODO: I think we need better words for these concepts.  "empty" & "single" are placeholders for now.
    //https://github.com/trueagi-io/hyperon-experimental/issues/281
    m.def("bindings_set_is_empty", [](CBindingsSet& set) {
        return bindings_set_is_empty(set.ptr());
    }, "Returns true if BindingsSet contains no Bindings object (fully constrained)");
    m.def("bindings_set_is_single", [](CBindingsSet& set) {
        return bindings_set_is_single(set.ptr());
    }, "Returns true if BindingsSet contains no variable bindings (unconstrained)");
    m.def("bindings_set_to_str", [](CBindingsSet& set) {
        return func_to_string((write_to_buf_func_t)&bindings_set_to_str, (void*)set.ptr());
    }, "Convert BindingsSet to human readable string");
    m.def("bindings_set_clone", [](CBindingsSet& set) { return CBindingsSet(bindings_set_clone(set.ptr())); }, "Deep copy of BindingsSet");
    m.def("bindings_set_from_bindings", [](CBindings bindings) { bindings_t cloned_bindings = bindings_clone(bindings.ptr()); return CBindingsSet(bindings_set_from_bindings(cloned_bindings)); }, "New BindingsSet from existing Bindings");
    m.def("bindings_set_push", [](CBindingsSet& set, CBindings bindings) { bindings_t cloned_bindings = bindings_clone(bindings.ptr()); bindings_set_push(set.ptr(), cloned_bindings); }, "Adds the Bindings to the BindingsSet");
    m.def("bindings_set_add_var_binding", [](CBindingsSet& set, CAtom var, CAtom value) {
        bindings_set_add_var_binding(set.ptr(), var.ptr(), value.ptr());
    }, "Asserts a binding between a variable and an atom for every Bindings in the BindingsSet" );
    m.def("bindings_set_add_var_equality", [](CBindingsSet& set, CAtom var_a, CAtom var_b) {
        bindings_set_add_var_equality(set.ptr(), var_a.ptr(), var_b.ptr());
    }, "Asserts a binding between two variables for every Bindings in the BindingsSet" );
    m.def("bindings_set_merge_into", [](CBindingsSet& set, CBindingsSet& other) {
        bindings_set_merge_into(set.ptr(), other.ptr());
    }, "Merges the contents of the `other` BindingsSet into the `set` BindingsSet" );
    m.def("bindings_set_list", [](CBindingsSet& set) -> pybind11::list {
        pybind11::list bindings_list;
        bindings_set_iterate(
                set.ptr(),
                bindings_copy_to_list_callback,
                &bindings_list);
        return bindings_list;
    }, "Returns iterator to traverse Bindings within BindingsSet");
    m.def("bindings_set_unpack", [](CBindingsSet& set) -> pybind11::list {
        py::list results;
        bindings_set_iterate(
            set.ptr(),
            [](bindings_t * cbindings, void* context) {
                py::list& results = *(py::list*)context;
                py::dict pybindings;
                bindings_traverse(cbindings, copy_atom_to_dict, &pybindings );
                results.append(pybindings);
            }, &results);
        return results;
    }, "Unpacks a BindingsSet into a list of dicts");

    py::class_<CSpace>(m, "CSpace");
    m.def("space_new_grounding", []() { return CSpace(space_new_grounding_space()); }, "New grounding space instance");
    m.def("space_new_custom", [](py::object object) {
        return CSpace( space_new(&PY_SPACE_NO_SUBST_API, new PySpace(object)));
        }, "Create new custom space implemented in Python");
    m.def("space_free", [](CSpace space) { space_free(space.obj); }, "Free space");
    m.def("space_get_payload", [](CSpace space) {
        PySpace* py_space = (PySpace*)space_get_payload(space.ptr());
        return py_space->pyobj;
        }, "Accessor for the payload of a space implemented in Python");
    m.def("space_add", [](CSpace space, CAtom atom) { space_add(space.ptr(), atom_clone(atom.ptr())); }, "Add atom into space");
    m.def("space_remove", [](CSpace space, CAtom& atom) { return space_remove(space.ptr(), atom.ptr()); }, "Remove atom from space");
    m.def("space_replace", [](CSpace space, CAtom& from, CAtom to) { return space_replace(space.ptr(), from.ptr(), atom_clone(to.ptr())); }, "Replace atom from space");
    m.def("space_eq", [](CSpace a, CSpace b) { return space_eq(a.ptr(), b.ptr()); }, "Check if two spaces are equal");
    m.def("space_atom_count", [](CSpace space) { return space_atom_count(space.ptr()); }, "Return number of atoms in space, or -1 if the space is unable to determine the value");
    m.def("space_list", [](CSpace space) -> nonstd::optional<pybind11::list> {
        pybind11::list atoms_list;
        if (space_iterate(space.ptr(), atom_copy_to_list_callback, &atoms_list)) {
            return atoms_list;
        } else {
            return nonstd::nullopt;
        }
    }, "Returns iterator to traverse atoms within a space");
    m.def("space_query", [](CSpace space, CAtom& pattern) {
            bindings_set_t result_bindings_set = space_query(space.ptr(), pattern.ptr());
            return CBindingsSet(result_bindings_set);
        }, "Query atoms from space by pattern");
    m.def("space_subst", [](CSpace space, CAtom& pattern, CAtom& templ) {
            py::list atoms;
            space_subst(space.ptr(), pattern.ptr(), templ.ptr(), copy_atoms, &atoms);
            return atoms;
        }, "Get bindings for pattern and apply to template");

    py::class_<CTokenizer>(m, "CTokenizer");
    m.def("tokenizer_new", []() { return CTokenizer(tokenizer_new()); }, "New tokenizer");
    m.def("tokenizer_free", [](CTokenizer tokenizer) { tokenizer_free(tokenizer.obj); }, "Free tokenizer");
    m.def("tokenizer_clone", [](CTokenizer tokenizer) { tokenizer_clone(tokenizer.ptr()); }, "Clone tokenizer");
    m.def("tokenizer_register_token", [](CTokenizer tokenizer, char const* regex, py::function constr) {
            tokenizer_register_token(tokenizer.ptr(), regex, &TOKEN_API, new CConstr(constr));
        }, "Register token");

    py::enum_<syntax_node_type_t>(m, "SyntaxNodeType")
        .value("COMMENT", syntax_node_type_t::COMMENT)
        .value("VARIABLE_TOKEN", syntax_node_type_t::VARIABLE_TOKEN)
        .value("STRING_TOKEN", syntax_node_type_t::STRING_TOKEN)
        .value("WORD_TOKEN", syntax_node_type_t::WORD_TOKEN)
        .value("OPEN_PAREN", syntax_node_type_t::OPEN_PAREN)
        .value("CLOSE_PAREN", syntax_node_type_t::CLOSE_PAREN)
        .value("WHITESPACE", syntax_node_type_t::WHITESPACE)
        .value("LEFTOVER_TEXT", syntax_node_type_t::LEFTOVER_TEXT)
        .value("EXPRESSION_GROUP", syntax_node_type_t::EXPRESSION_GROUP)
        .value("ERROR_GROUP", syntax_node_type_t::ERROR_GROUP)
        .export_values();

    py::class_<CSyntaxNode>(m, "CSyntaxNode");
    m.def("syntax_node_free", [](CSyntaxNode node) { syntax_node_free(node.obj); }, "Free a syntax node at the top level of a syntax tree");
    m.def("syntax_node_clone", [](CSyntaxNode& node) { return CSyntaxNode(syntax_node_clone(node.ptr())); }, "Create a deep copy of the syntax node");
    m.def("syntax_node_type", [](CSyntaxNode& node) { return syntax_node_type(node.ptr()); }, "Get type of the syntax node");
    m.def("syntax_node_is_null", [](CSyntaxNode& node) { return syntax_node_is_null(node.ptr()); }, "Returns True if a syntax node is Null");
    m.def("syntax_node_is_leaf", [](CSyntaxNode& node) { return syntax_node_is_leaf(node.ptr()); }, "Returns True if a syntax node is Null");
    m.def("syntax_node_src_range", [](CSyntaxNode& node) -> py::object {
        size_t start, end;
        syntax_node_src_range(node.ptr(), &start, &end);
        return py::make_tuple(start, end);
    }, "Get range in source code offsets for the text represented by the node");
    m.def("syntax_node_unroll", [](CSyntaxNode& node) {
        pybind11::list nodes_list;
        syntax_node_iterate(node.ptr(), syntax_node_copy_to_list_callback, &nodes_list);
        return nodes_list;
    }, "Returns a list of all leaf nodes recursively contained within a SyntaxNode");

    py::class_<CSExprParser>(m, "CSExprParser")
        .def(py::init<std::string>())
        .def("parse", &CSExprParser::parse,  "Return next parsed atom, None, or an error expression")
        .def("sexpr_parser_err_str", &CSExprParser::err_str,  "Return the parse error from the previous parse operation or None")
        .def("parse_to_syntax_tree", &CSExprParser::parse_to_syntax_tree,  "Return next parser atom or None, as a syntax node at the root of a syntax tree");

    py::class_<CStepResult>(m, "CStepResult")
        .def("__str__", [](CStepResult step) {
            return func_to_string((write_to_buf_func_t)&step_to_str, step.ptr());
        }, "Convert step to human readable string");
    m.def("interpret_init", [](CSpace space, CAtom expr) {
            return CStepResult(interpret_init(space.ptr(), expr.ptr()));
        }, "Initialize interpreter of the expression");
    m.def("interpret_step", [](CStepResult step) {
            return CStepResult(interpret_step(step.obj));
        }, "Do next step of the interpretataion");
    m.def("step_has_next", [](CStepResult step) {
            return step_has_next(step.ptr());
        }, "Check whether next step of interpretation is posible");
    m.def("step_get_result", [](CStepResult step) {
            py::list atoms;
            step_get_result(step.obj, copy_atoms, &atoms);
            return atoms;
        }, "Return result of the interpretation");

#define ADD_TYPE(t, d) .def_property_readonly_static(#t, [](py::object) { return CAtom(ATOM_TYPE_ ## t()); }, d " atom type")

    py::class_<CAtomType>(m, "CAtomType")
        ADD_TYPE(UNDEFINED, "Undefined")
        ADD_TYPE(TYPE, "Type")
        ADD_TYPE(ATOM, "Generic")
        ADD_TYPE(SYMBOL, "Symbol")
        ADD_TYPE(VARIABLE, "Variable")
        ADD_TYPE(EXPRESSION, "Expression")
        ADD_TYPE(GROUNDED, "Grounded")
        ADD_TYPE(GROUNDED_SPACE, "Space");
    m.def("check_type", [](CSpace space, CAtom& atom, CAtom& type) {
            return check_type(space.ptr(), atom.ptr(), type.ptr());
        }, "Check if atom is an instance of the passed type");
    m.def("validate_atom", [](CSpace space, CAtom& atom) {
            return validate_atom(space.ptr(), atom.ptr());
        }, "Validate expression arguments correspond to the operation type");
    m.def("get_atom_types", [](CSpace space, CAtom& atom) {
            py::list atoms;
            get_atom_types(space.ptr(), atom.ptr(), copy_atoms, &atoms);
            return atoms;
        }, "Get types of the given atom");

#define ADD_SYMBOL(t, d) .def_property_readonly_static(#t, [](py::object) { return CAtom(t ## _SYMBOL()); }, d " atom type")

    py::class_<CAtoms>(m, "CAtoms")
        ADD_SYMBOL(EMPTY, "Empty");

    py::class_<CMetta>(m, "CMetta");
    m.def("metta_new", [](CSpace space, EnvBuilder env_builder) {
        return CMetta(metta_new_with_space_environment_and_stdlib(space.ptr(), env_builder.obj, &run_python_loader_callback, NULL));
    }, "New MeTTa interpreter instance");
    m.def("metta_free", [](CMetta metta) { metta_free(metta.obj); }, "Free MeTTa interpreter");
    m.def("metta_err_str", [](CMetta& metta) {
        const char* err_str = metta_err_str(metta.ptr());
        return err_str != NULL ? py::cast(std::string(err_str)) : py::none();
    }, "Returns the error string from the last MeTTa operation or None");
    m.def("metta_eq", [](CMetta& a, CMetta& b) { return metta_eq(a.ptr(), b.ptr()); }, "Compares two MeTTa handles");
    m.def("metta_search_path_cnt", [](CMetta& metta) { return metta_search_path_cnt(metta.ptr()); }, "Returns the number of module search paths in the runner's environment");
    m.def("metta_nth_search_path", [](CMetta& metta, size_t idx) {
        return func_to_string_two_args((write_to_buf_two_arg_func_t)&metta_nth_search_path, metta.ptr(), (void*)idx);
    }, "Returns the module search path at the specified index, in the runner's environment");
    m.def("metta_space", [](CMetta& metta) { return CSpace(metta_space(metta.ptr())); }, "Get space of MeTTa interpreter");
    m.def("metta_tokenizer", [](CMetta& metta) { return CTokenizer(metta_tokenizer(metta.ptr())); }, "Get tokenizer of MeTTa interpreter");
    m.def("metta_run", [](CMetta& metta, CSExprParser& parser) {
            py::list lists_of_atom;
            sexpr_parser_t cloned_parser = sexpr_parser_clone(&parser.parser);
            metta_run(metta.ptr(), cloned_parser, copy_lists_of_atom, &lists_of_atom);
            return lists_of_atom;
        }, "Run MeTTa interpreter on an input");
    m.def("metta_evaluate_atom", [](CMetta& metta, CAtom atom) {
            py::list atoms;
            metta_evaluate_atom(metta.ptr(), atom_clone(atom.ptr()), copy_atoms, &atoms);
            return atoms;
        }, "Run MeTTa interpreter on an atom");
    //QUESTION: Should we eliminate the `metta_load_module` function from the native APIs, in favor of
    // allowing the caller to load modules using the `import` operation in MeTTa code?
    m.def("metta_load_module", [](CMetta& metta, std::string text) {
        metta_load_module(metta.ptr(), text.c_str());
    }, "Load MeTTa module");

    py::class_<CRunnerState>(m, "CRunnerState")
        .def("__str__", [](CRunnerState state) {
            return func_to_string((write_to_buf_func_t)&runner_state_to_str, state.ptr());
        }, "Render a RunnerState as a human readable string");
    m.def("runner_state_new_with_parser", [](CMetta& metta, CSExprParser& parser) {
        sexpr_parser_t cloned_parser = sexpr_parser_clone(&parser.parser);
        return CRunnerState(runner_state_new_with_parser(metta.ptr(), cloned_parser));
    }, "Initializes the MeTTa runner state for incremental execution");
    m.def("runner_state_new_with_atoms", [](CMetta& metta, CVecAtom& atoms) {
        return CRunnerState(runner_state_new_with_atoms(metta.ptr(), atoms.ptr()));
    }, "Initializes the MeTTa runner state for incremental execution");
    m.def("runner_state_step", [](CRunnerState& state) { runner_state_step(state.ptr()); }, "Runs one incremental step of the MeTTa interpreter");
    m.def("runner_state_free", [](CRunnerState state) { runner_state_free(state.obj); }, "Frees a Runner State");
    m.def("runner_state_err_str", [](CRunnerState& state) {
        const char* err_str = runner_state_err_str(state.ptr());
        return err_str != NULL ? py::cast(std::string(err_str)) : py::none();
    }, "Returns the error string from the last RunnerState operation or None");
    m.def("runner_state_is_complete", [](CRunnerState& state) { return runner_state_is_complete(state.ptr()); }, "Returns whether a RunnerState is finished");
    m.def("runner_state_current_results", [](CRunnerState& state) {
        py::list lists_of_atom;
        runner_state_current_results(state.ptr(), copy_lists_of_atom, &lists_of_atom);
        return lists_of_atom;
    }, "Returns the in-flight results from a runner state");

    py::class_<EnvBuilder>(m, "EnvBuilder");
    m.def("environment_config_dir", []() {
        return func_to_string_no_arg((write_to_buf_no_arg_func_t)&environment_config_dir);
    }, "Return the config_dir for the common environment");
    m.def("env_builder_start", []() { return EnvBuilder(env_builder_start()); }, "Begin initialization of the environment");
    m.def("env_builder_use_default", []() { return EnvBuilder(env_builder_use_default()); }, "Use the common environment");
    m.def("env_builder_use_test_env", []() { return EnvBuilder(env_builder_use_test_env()); }, "Use an environment for unit testing");
    m.def("env_builder_init_common_env", [](EnvBuilder builder) { return env_builder_init_common_env(builder.obj); }, "Finish initialization of the common environment");
    m.def("env_builder_set_working_dir", [](EnvBuilder& builder, std::string path) { env_builder_set_working_dir(builder.ptr(), path.c_str()); }, "Sets the working dir in the environment");
    m.def("env_builder_set_config_dir", [](EnvBuilder& builder, std::string path) { env_builder_set_config_dir(builder.ptr(), path.c_str()); }, "Sets the config dir in the environment");
    m.def("env_builder_create_config_dir", [](EnvBuilder& builder) { env_builder_create_config_dir(builder.ptr()); }, "Creates the config dir if it doesn't exist");
    m.def("env_builder_disable_config_dir", [](EnvBuilder& builder) { env_builder_disable_config_dir(builder.ptr()); }, "Disables the config dir in the environment");
    m.def("env_builder_set_is_test", [](EnvBuilder& builder, bool is_test) { env_builder_set_is_test(builder.ptr(), is_test); }, "Disables the config dir in the environment");
    m.def("env_builder_add_include_path", [](EnvBuilder& builder, std::string path) { env_builder_add_include_path(builder.ptr(), path.c_str()); }, "Adds an include path to the environment");

    m.def("gnd_to_int", [](CAtom atom) -> py::object {
            long long n;
            if (grounded_number_to_longlong(atom.ptr(), &n)) {
                return py::int_(n);
            } else
                return py::none();
            }, "Convert MeTTa stdlib number to Python int");
    m.def("gnd_to_float", [](CAtom atom) -> py::object {
            double d;
            if (grounded_number_to_double(atom.ptr(), &d))
                return py::float_(d);
            else
                return py::none();
            }, "Convert MeTTa stdlib number to Python float");
    m.def("number_to_gnd", [](py::object n) {
                if (py::isinstance<py::int_>(n)) {
                    return CAtom(longlong_to_grounded_number(n.cast<long long>()));
                }
                if (py::isinstance<py::float_>(n)) {
                    return CAtom(double_to_grounded_number(n.cast<double>()));
                }
                throw std::runtime_error("int of float number is expected as an argument");
            }, "Convert Python number to MeTTa stdlib number");
}

