#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <hyperon/hyperon.hpp>

namespace py = pybind11;

class PyGroundedAtomHolder : public GroundedAtom {
private:
	py::object pygnd;

public:
	PyGroundedAtomHolder(py::object pygnd) : pygnd(pygnd) {
		pygnd.inc_ref();
	}

	virtual ~PyGroundedAtomHolder() {
		pygnd.dec_ref();
	}

	std::string execute(VecAtom& ops, VecAtom& data) override {
		return py::cast<GroundedAtom&>(pygnd).execute(ops, data);
	}

	bool operator==(GroundedAtom const& _other) const override {
		PyGroundedAtomHolder const* holder = dynamic_cast<PyGroundedAtomHolder const*>(&_other);
		GroundedAtom const& other = holder ? py::cast<GroundedAtom const&>(holder->pygnd) : _other;
		return py::cast<GroundedAtom&>(pygnd).operator==(other);
	}

	GroundedAtom* clone() const override {
		GroundedAtom const& self = py::cast<GroundedAtom const&>(pygnd);
		GroundedAtom* copy = self.clone();
		return copy;
	}

    std::string to_string() const override {
		return py::cast<GroundedAtom const&>(pygnd).to_string();
    }
};

class PyGroundedAtomTrampoline : public GroundedAtom {
public:
	using GroundedAtom::GroundedAtom;

	std::string execute(VecAtom& ops, VecAtom& data) override {
        // workaround for a pybind11 issue https://github.com/pybind/pybind11/issues/2033
        // see https://stackoverflow.com/a/59331026/14016260 for explanation
        py::object dummy0 = py::cast(&ops);
        py::object dummy1 = py::cast(&data);
        PYBIND11_OVERRIDE(std::string, GroundedAtom, execute, ops, data);
	}

	bool operator==(GroundedAtom const& other) const override {
        PYBIND11_OVERRIDE_PURE_NAME(bool, GroundedAtom, "__eq__", operator==, other);
	}

	GroundedAtom* clone() const override {
        //PYBIND11_OVERRIDE_PURE_NAME(GroundedAtom*, GroundedAtom, "copy", clone,);
    	py::gil_scoped_acquire gil;  // Acquire the GIL while in this scope.
    	// Try to look up the overridden method on the Python side.
    	py::function override = py::get_override(this, "copy");
    	if (override) {  // method is found
        	auto obj = override();  // Call the Python function.
        	return new PyGroundedAtomHolder(obj);
    	}
        py::pybind11_fail("Tried to call pure virtual function GroundedAtom::clone()");
	}

    std::string to_string() const override {
        PYBIND11_OVERRIDE_PURE_NAME(std::string, GroundedAtom, "__repr__", to_string,);
    }
};

PYBIND11_MODULE(hyperonpy, m) {
	m.doc() = "Python API of the Hyperon library";

	py::class_<Atom> atom(m, "Atom");

	py::enum_<atom_type_t>(atom, "Type")
		.value("SYMBOL", atom_type_t::SYMBOL)
		.value("VARIABLE", atom_type_t::VARIABLE)
		.value("EXPR", atom_type_t::EXPR)
		.value("GROUNDED", atom_type_t::GROUNDED)
		.export_values();

	atom.def("__eq__", &Atom::operator==)
		.def("__repr__", &Atom::to_string)
		.def("get_type", &Atom::get_type);

	py::class_<SymbolAtom, Atom>(m, "SymbolAtom")
		.def("get_symbol", &SymbolAtom::get_name);
	py::class_<ExprAtom, Atom>(m, "ExprAtom");
	py::class_<VariableAtom, Atom>(m, "VariableAtom")
		.def("get_name", &VariableAtom::get_name);
	py::class_<Grounded, Atom>(m, "Grounded");

    m.def("S", &SymbolAtom::sym, "Create symbol atom");
    m.def("V", &VariableAtom::var, "Create variable atom");
    m.def("E", &ExprAtom::expr, "Create expression atom");
    m.def("G", [](py::object gnd) -> Grounded {
    			return Grounded::gnd(new PyGroundedAtomHolder(gnd));
    		}, "Create grounded atom");

	py::class_<GroundedAtom, PyGroundedAtomTrampoline>(m, "GroundedAtom")
		.def(py::init<>())
		.def("execute", &GroundedAtom::execute)
		.def("copy", &GroundedAtom::clone)
		.def("__eq__", &GroundedAtom::operator==)
		.def("__repr__", &GroundedAtom::to_string);

	py::class_<VecAtom>(m, "VecAtom")
		.def("pop", &VecAtom::pop)
		.def("push", &VecAtom::push);
}

