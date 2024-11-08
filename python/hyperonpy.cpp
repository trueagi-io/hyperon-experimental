#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <nonstd/optional.hpp>
#include <fstream>
#include <iostream>
#include <sstream>

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
struct CConstPtr {
    CConstPtr(const T* ptr) : ptr(ptr) {}
    const T* ptr;
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
using CRunContext = CPtr<run_context_t>;
using CModuleDescriptor = CConstPtr<module_descriptor_t>;
using ModuleId = CStruct<module_id_t>;
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
    serial_result_t py_serialize(const struct gnd_t *_gnd, struct serializer_api_t const* api, void* context);
    bool py_eq(const struct gnd_t* _a, const struct gnd_t* _b);
    struct gnd_t *py_clone(const struct gnd_t* _gnd);
    size_t py_display(const struct gnd_t* _gnd, char* buffer, size_t size);
    void py_free(struct gnd_t* _gnd);
}
extern "C" bindings_set_t py_match_value(const struct gnd_t *_gnd, const atom_ref_t *_atom);

struct GroundedObject : gnd_t {
    GroundedObject(py::object pyobj, atom_t typ) : pyobj(pyobj) {
        // TODO: here static API instance is replaced by allocated one. This
        // increases the memory usage and slows down the code. There are two
        // ways to fix it: (1) add 2^3 static instances of gnd_api_t and
        // choosing between them using 3 nested conditions; (2) make pointers
        // in gnd_api_t non-optional and check whether method is present
        // dynamically in Python. In case (2) default implementation should be
        // chosen in Python.
        gnd_api_t* api = new gnd_api_t{  nullptr, nullptr, nullptr, &py_eq, &py_clone, &py_display, &py_free };
        if (py::hasattr(pyobj, "execute")) {
            api->execute = &py_execute;
        }
        if (py::hasattr(pyobj, "match_")) {
            api->match_ = &py_match_;
        } else {
            api->match_ = &py_match_value;
        }
        if (py::hasattr(pyobj, "serialize")) {
            api->serialize = &py_serialize;
        }
        this->api = api;
        this->typ = typ;
    }
    virtual ~GroundedObject() {
        delete this->api;
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
    py::function _priv_call_execute_on_grounded_atom = hyperon.attr("_priv_call_execute_on_grounded_atom");
    py::handle NoReduceError = hyperon.attr("NoReduceError");
    py::object pyobj = static_cast<GroundedObject const*>(_cgnd)->pyobj;
    CAtom pytyp = static_cast<GroundedObject const*>(_cgnd)->typ;
    try {
        py::list args;
        for (size_t i = 0; i < atom_vec_len(_args); ++i) {
            atom_ref_t arg_atom_ref = atom_vec_get(_args, i);
            args.append(CAtom(atom_clone(&arg_atom_ref)));
        }
        py::list result = _priv_call_execute_on_grounded_atom(pyobj, pytyp, args);
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

//Callback function for use by grounded atoms wrapping python values
bindings_set_t py_match_value(const struct gnd_t *_gnd, const atom_ref_t *_atom) {
    py::object hyperon = py::module_::import("hyperon.atoms");
    py::function compare_value_atom_fn = hyperon.attr("_priv_compare_value_atom");

    py::object pyobj = static_cast<GroundedObject const *>(_gnd)->pyobj;
    CAtom catom = atom_clone(_atom);
    py::bool_ result = compare_value_atom_fn(pyobj, catom);

    if (result) {
        return bindings_set_single();
    } else {
        return bindings_set_empty();
    }
}

bindings_set_t py_match_(const struct gnd_t *_gnd, const atom_ref_t *_atom) {
    py::object hyperon = py::module_::import("hyperon.atoms");
    py::function _priv_call_match_on_grounded_atom = hyperon.attr("_priv_call_match_on_grounded_atom");

    py::object pyobj = static_cast<GroundedObject const *>(_gnd)->pyobj;
    CAtom catom = atom_clone(_atom);
    py::list results = _priv_call_match_on_grounded_atom(pyobj, catom);

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

struct Serializer {
    Serializer() {}
    virtual ~Serializer() {}
    virtual serial_result_t serialize_bool(bool v) {
        return serial_result_t::NOT_SUPPORTED;
    }
    virtual serial_result_t serialize_int(py::int_ v) {
        return serial_result_t::NOT_SUPPORTED;
    }
    virtual serial_result_t serialize_float(py::float_ v) {
        return serial_result_t::NOT_SUPPORTED;
    }
};

struct PySerializer : public Serializer {
    using Serializer::Serializer;

    serial_result_t serialize_bool(bool v) override {
        PYBIND11_OVERRIDE_PURE(serial_result_t, Serializer, serialize_bool, v);
    }

    serial_result_t serialize_int(py::int_ v) override {
        PYBIND11_OVERRIDE_PURE(serial_result_t, Serializer, serialize_int, v);
    }

    serial_result_t serialize_float(py::float_ v) override {
        PYBIND11_OVERRIDE_PURE(serial_result_t, Serializer, serialize_float, v);
    }
};

struct PythonToCSerializer : public Serializer {
    PythonToCSerializer(struct serializer_api_t const* api, void* context) : Serializer(), api(api), context(context) { }
    virtual ~PythonToCSerializer() { }

    serial_result_t serialize_bool(bool v) override {
        return this->api->serialize_bool(this->context, v);
    }
    serial_result_t serialize_int(py::int_ v) override {
        return this->api->serialize_longlong(this->context, v);
    }
    serial_result_t serialize_float(py::float_ v) override {
        return this->api->serialize_double(this->context, v);
    }

    struct serializer_api_t const* api;
    void* context;
};

serial_result_t py_serialize(const struct gnd_t *_gnd, struct serializer_api_t const* api, void* context) {
    py::object hyperon = py::module_::import("hyperon.atoms");
    py::function _priv_call_serialize_on_grounded_atom = hyperon.attr("_priv_call_serialize_on_grounded_atom");

    py::object pyobj = static_cast<GroundedObject const *>(_gnd)->pyobj;
    PythonToCSerializer py_serializer(api, context);
    py::object result = _priv_call_serialize_on_grounded_atom(pyobj, py_serializer);
    return result.cast<serial_result_t>();
}

struct CToPythonSerializer {
    CToPythonSerializer(Serializer& _serializer) : serializer(_serializer) {}
    virtual ~CToPythonSerializer() {}

    static CToPythonSerializer* to_this(void* serializer) {
        return static_cast<CToPythonSerializer*>(serializer);
    }
    static serial_result_t serialize_bool(void* serializer, bool v) {
        return to_this(serializer)->serializer.serialize_bool(v);
    }
    static serial_result_t serialize_longlong(void* serializer, long long v) {
        return to_this(serializer)->serializer.serialize_int(v);
    }
    static serial_result_t serialize_double(void* serializer, double v) {
        return to_this(serializer)->serializer.serialize_float(v);
    }

    Serializer& serializer;
};

const serializer_api_t PY_C_TO_PYTHON_SERIALIZER = {
    &CToPythonSerializer::serialize_bool,
    &CToPythonSerializer::serialize_longlong,
    &CToPythonSerializer::serialize_double
};

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

void copy_pair_of_atoms_to_list_callback(atom_ref_t var, atom_ref_t atom, void* context){
    pybind11::list& var_atom_list = *( (pybind11::list*)(context) );
    var_atom_list.append(std::make_pair(CAtom(var), CAtom(atom)));
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

// A C function that wraps a Python function, so that the python code to load the stdlib can be run inside `metta_new_with_space_environment_and_stdlib()`
void run_python_stdlib_loader(run_context_t* run_context, void* callback_context) {
    py::object runner_mod = py::module_::import("hyperon.runner");
    py::function load_py_stdlib = runner_mod.attr("_priv_load_py_stdlib");
    CRunContext c_run_context = CRunContext(run_context);
    load_py_stdlib(&c_run_context);
}

// A C function that dispatches to a Python function, so that the Python module loader code can be run inside `metta_load_module_direct()`
void run_python_module_loader(run_context_t* run_context, void* callback_context) {
    py::function* py_func = (py::function*)callback_context;
    CRunContext c_run_context = CRunContext(run_context);
    (*py_func)(&c_run_context);
}

size_t path_for_name_mod_fmt_callback(const void* payload, const char* parent_dir, const char* mod_name, char* dst_buf, uintptr_t buf_size) {
    py::object* fmt_interface_obj = (py::object*)payload;
    py::function py_func = fmt_interface_obj->attr("path_for_name");

    py::object result_path_py = py_func(parent_dir, mod_name);
    std::string result_path_string = py::str(result_path_py);

    if (buf_size >= result_path_string.length()+1) {
        strncpy(dst_buf, &result_path_string[0], result_path_string.length());
        dst_buf[result_path_string.length()] = 0;
        return result_path_string.length()+1;
    } else {
        return 0;
    }
}

void* try_path_mod_fmt_callback(const void* payload, const char* path, const char* mod_name) {
    py::object* fmt_interface_obj = (py::object*)payload;
    py::function py_func = fmt_interface_obj->attr("try_path");
    py::object context_obj = py_func(path, mod_name);
    if (context_obj.is_none()) {
        return NULL;
    } else {
        return (void*) new py::object(context_obj);
    }
}

void load_mod_fmt_callback(const void* payload, run_context_t* run_context, void* callback_context) {
    py::object* fmt_interface_obj = (py::object*)payload;
    py::object* callback_context_obj = (py::object*)callback_context;
    py::function py_func = fmt_interface_obj->attr("_load_called_from_c");
    CRunContext c_run_context = CRunContext(run_context);
    try {
        py_func(&c_run_context, callback_context_obj);
    } catch (py::error_already_set &e) {
        char message[4096];
        snprintf(message, lenghtof(message), "Exception caught:\n%s", e.what());
        run_context_raise_error(run_context, message);
    }
}

void free_mod_fmt_context(void* callback_context) {
    py::object* py_context_obj = (py::object*)callback_context;
    delete py_context_obj;
}

// Module Format API Declaration for a module implementation in C
static mod_file_fmt_api_t const C_FMT_API= {
    .path_for_name = &path_for_name_mod_fmt_callback,
    .try_path = &try_path_mod_fmt_callback,
    .load = &load_mod_fmt_callback,
    .free_callback_context = &free_mod_fmt_context,
};

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

    py::enum_<serial_result_t>(m, "SerialResult", "Serializer error code")
        .value("OK", serial_result_t::OK, "Serialization is successfully finished")
        .value("NOT_SUPPORTED", serial_result_t::NOT_SUPPORTED, "Serialization of the type is not supported by serializer");

    py::class_<CAtom>(m, "CAtom");

    m.def("atom_sym", [](char const* name) { return CAtom(atom_sym(name)); }, "Create symbol atom");
    m.def("atom_var", [](char const* name) { return CAtom(atom_var(name)); }, "Create variable atom");
    m.def("atom_var_parse_name", [](char const* name) { return CAtom(atom_var_parse_name(name)); }, "Create variable atom parsing name in format <name>#<id>");
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
    m.def("atom_get_metatype", [](CAtom& atom) { return atom_get_metatype(atom.ptr()); }, "Get type of the atom");
    m.def("atom_get_name", [](CAtom& atom) {
            return func_to_string((write_to_buf_func_t)&atom_get_name, atom.ptr());
        }, "Get name of the Symbol or Variable atom");
    m.def("atom_get_space", [](CAtom& atom) {
            return CSpace(atom_get_space(atom.ptr()));
        }, "Get the space inside of a Grounded atom wrapping a space");
    m.def("atom_get_object", [](CAtom& atom) {
            return static_cast<GroundedObject const*>(atom_get_object(atom.ptr()))->pyobj;
        }, "Get object of the grounded atom");
    m.def("atom_is_cgrounded", [](CAtom& atom) {
            return py::bool_(atom_is_cgrounded(atom.ptr()));
        }, "Check if atom is CGrounded");
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
          [](CBindings bindings, CAtom var, CAtom atom) {
              return bindings_add_var_binding(bindings.ptr(), atom_clone(var.ptr()), atom_clone(atom.ptr()));
          },
          "Links variable to atom" );
    m.def("bindings_is_empty", [](CBindings bindings){ return bindings_is_empty(bindings.ptr());}, "Returns true if bindings is empty");

    m.def("bindings_narrow_vars", [](CBindings bindings, CVecAtom& vars) {
            bindings_narrow_vars(bindings.ptr(), vars.ptr());
        }, "Remove vars from Bindings, except those specified" );

    m.def("bindings_resolve", [](CBindings bindings, CAtom var) -> nonstd::optional<CAtom> {
            auto const res = bindings_resolve(bindings.ptr(), atom_clone(var.ptr()));
            return atom_is_null(&res) ? nonstd::nullopt : nonstd::optional<CAtom>(CAtom(res));
        }, "Resolve" );

    m.def("bindings_to_str", [](CBindings bindings) {
        return func_to_string((write_to_buf_func_t)&bindings_to_str, bindings.ptr());
    }, "Convert bindings to human readable string");

    m.def("bindings_list", [](CBindings bindings) -> pybind11::list {
        pybind11::list var_atom_list;
        bindings_traverse(
                bindings.ptr(),
                copy_pair_of_atoms_to_list_callback,
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
        ADD_TYPE(GROUNDED_SPACE, "Space")
        ADD_TYPE(UNIT, "Unit");
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

#define ADD_ATOM(t, d) .def_property_readonly_static(#t, [](py::object) { return CAtom(t ## _ATOM()); }, d " atom type")

    py::class_<CAtoms>(m, "CAtoms")
        ADD_ATOM(EMPTY, "Empty")
        ADD_ATOM(UNIT, "Unit")
        ADD_ATOM(METTA, "metta");

    py::class_<CRunContext>(m, "CRunContext");
    m.def("run_context_init_self_module", [](CRunContext& run_context, CSpace space, char const* resource_dir) {
        run_context_init_self_module(run_context.ptr, space.ptr(), resource_dir);
    }, "Init module in loader");
    m.def("run_context_load_module", [](CRunContext& run_context, const char* mod_name) {
        return ModuleId(run_context_load_module(run_context.ptr, mod_name));
    }, "Load a module by name");
    m.def("run_context_get_metta", [](CRunContext& run_context) {
        return CMetta(run_context_get_metta(run_context.ptr));
    }, "Returns the MeTTa runner that a RunContext is running within");
    m.def("run_context_get_space", [](CRunContext& run_context) {
        return CSpace(run_context_get_space(run_context.ptr));
    }, "Returns the Space for the currently running module");
    m.def("run_context_get_tokenizer", [](CRunContext& run_context) {
        return CTokenizer(run_context_get_tokenizer(run_context.ptr));
    }, "Returns the Tokenizer for the currently running module");
    m.def("run_context_import_dependency", [](CRunContext& run_context, ModuleId mod_id) {
        run_context_import_dependency(run_context.ptr, mod_id.obj);
    }, "Imports a dependency into a module");

    py::class_<CModuleDescriptor>(m, "CModuleDescriptor");

    py::class_<ModuleId>(m, "ModuleId")
        .def("is_valid", [](ModuleId& id) { return module_id_is_valid(id.ptr()); }, "Returns True if a ModuleId is valid");

    py::class_<CMetta>(m, "CMetta");
    m.def("metta_new", [](CSpace space, EnvBuilder env_builder) {
        return CMetta(metta_new_with_space_environment_and_stdlib(space.ptr(), env_builder.obj, &run_python_stdlib_loader, NULL));
    }, "New MeTTa interpreter instance");
    m.def("metta_free", [](CMetta metta) { metta_free(metta.obj); }, "Free MeTTa interpreter");
    m.def("metta_err_str", [](CMetta& metta) {
        const char* err_str = metta_err_str(metta.ptr());
        return err_str != NULL ? py::cast(std::string(err_str)) : py::none();
    }, "Returns the error string from the last MeTTa operation or None");
    m.def("metta_eq", [](CMetta& a, CMetta& b) { return metta_eq(a.ptr(), b.ptr()); }, "Compares two MeTTa handles");
    m.def("metta_space", [](CMetta& metta) { return CSpace(metta_space(metta.ptr())); }, "Get space of MeTTa runner's top-level module");
    m.def("metta_tokenizer", [](CMetta& metta) { return CTokenizer(metta_tokenizer(metta.ptr())); }, "Get tokenizer of MeTTa runner's top-level module");
    m.def("metta_working_dir", [](CMetta& metta) {
        return func_to_string((write_to_buf_func_t)&metta_working_dir, metta.ptr());
    }, "Returns the working dir from the runner's environment");
    m.def("metta_load_module_direct", [](CMetta& metta, char const* mod_name, py::function* py_func) {
        return ModuleId(metta_load_module_direct(metta.ptr(), mod_name, &run_python_module_loader, (void*)py_func));
    }, "Loads a module into a runner using a function");
    m.def("metta_load_module_at_path", [](CMetta& metta, char const* path, nonstd::optional<char const*> mod_name) {
        char const* name = mod_name.value_or((char const*)NULL);
        return ModuleId(metta_load_module_at_path(metta.ptr(), path, name));
    }, "Loads a module into a runner from a file system resource");
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
    m.def("env_builder_create_config_dir", [](EnvBuilder& builder, bool should_create) { env_builder_create_config_dir(builder.ptr(), should_create); }, "Creates the config dir if it doesn't exist");
    m.def("env_builder_disable_config_dir", [](EnvBuilder& builder) { env_builder_disable_config_dir(builder.ptr()); }, "Disables the config dir in the environment");
    m.def("env_builder_set_is_test", [](EnvBuilder& builder, bool is_test) { env_builder_set_is_test(builder.ptr(), is_test); }, "Disables the config dir in the environment");
    m.def("env_builder_push_include_path", [](EnvBuilder& builder, std::string path) { env_builder_push_include_path(builder.ptr(), path.c_str()); }, "Adds an include path to the environment");
    m.def("env_builder_push_fs_module_format", [](EnvBuilder& builder, py::object interface, uint64_t fmt_id) {
        //TODO. We end up leaking this object, but it's a non-issue in practice because environments usually live the life of the program.
        // To fix this, give the Python MeTTa object built from this EnvBuilder a reference to the `interface` object, rather than allocating it here
        py::object* py_impl = new py::object(interface);
        env_builder_push_fs_module_format(builder.ptr(), &C_FMT_API, (void*)py_impl, fmt_id);
    }, "Adds a new module format to the environment");

    m.def("log_error", [](std::string msg) { log_error(msg.c_str()); }, "Logs an error through the MeTTa logger");
    m.def("log_warn", [](std::string msg) { log_warn(msg.c_str()); }, "Logs a warning through the MeTTa logger");
    m.def("log_info", [](std::string msg) { log_info(msg.c_str()); }, "Logs an info message through the MeTTa logger");

    py::class_<Serializer, PySerializer>(m, "Serializer", "An abstract class to implement a custom serializer")
        .def(py::init<>(), "Constructor")
        .def("serialize_bool", &Serializer::serialize_bool, "Serialize bool value")
        .def("serialize_int", &Serializer::serialize_int, "Serialize int value")
        .def("serialize_float", &Serializer::serialize_float, "Serialize float value");
    py::class_<PythonToCSerializer>(m, "PythonToCSerializer", "Python serializer which is backed by C serializer")
        .def("serialize_bool", &Serializer::serialize_bool, "Serialize bool value")
        .def("serialize_int", &Serializer::serialize_int, "Serialize int value")
        .def("serialize_float", &Serializer::serialize_float, "Serialize float value");
    m.def("atom_gnd_serialize", [](CAtom atom, Serializer& _serializer) -> serial_result_t {
                CToPythonSerializer serializer(_serializer);
                return atom_gnd_serialize(atom.ptr(), &PY_C_TO_PYTHON_SERIALIZER, &serializer);
            }, "Serializes a grounded atom using the given serializer");

    m.def("load_ascii", [](std::string name, CSpace space) {
        py::object hyperon = py::module_::import("hyperon.atoms");
        py::function ValueObject = hyperon.attr("ValueObject");
        std::ifstream f;
        f.open(name);
        if(!f.is_open()) {
            throw std::runtime_error("load_ascii: file not found");
        }
        int count = 0;
        std::vector<std::vector<atom_t>> stack;
        std::vector<atom_t> children;
        std::string str = "";
        int depth = 0;
        bool isescape = false;
        while(f) {
            char c = f.get();
            bool isspace = std::isspace(c);
            if(isescape) {
                str += c;
                isescape = false;
                continue;
            }
            if((isspace or c == '(' or c == ')')) {
                if(c == ')' and depth == 0) {
                    // ignore for now --> exception
                    continue;
                }
                if(str.size() > 0) {
                    atom_t atom;
                    if(str[0] >= '0' and str[0] <= '9' or str[0] == '-') {
                        try {
                            long double ld = std::stold(str);
                            long long ll = std::stoll(str);
                            py::object obj;
                            // separate assignments are needed to get different types
                            if(ll == ld) {
                                obj = ValueObject(ll);
                            } else {
                                obj = ValueObject(ld);
                            }
                            atom = atom_gnd(new GroundedObject(obj,
                                            atom_sym("Number")));
                        }
                        catch(...) {
                            atom = atom_sym(str.c_str());
                        }
                    } else {
                        atom = atom_sym(str.c_str());
                    }
                    str.clear();
                    if(depth == 0) {
                        space_add(space.ptr(), atom);
                        continue;
                    }
                    children.push_back(atom);
                }
                if(isspace) {
                    continue;
                }
                if(c == '(') {
                    stack.push_back(std::move(children));
                    depth++;
                } else { //if(c == ')') {
                    atom_t expr = atom_expr(children.data(), children.size());
                    children = std::move(stack.back());
                    stack.pop_back();
                    depth--;
                    if(depth == 0) {
                        space_add(space.ptr(), expr);
                    } else {
                        children.push_back(expr);
                    }
                }
            } else {
                if(c == '\\') {
                    isescape = true;
                } else {
                    str += c;
                }
            }
        }
        return true;
    }, "Load metta space ignoring tokenization and execution");
}
