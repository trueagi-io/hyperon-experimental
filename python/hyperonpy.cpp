#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <hyperon/hyperon.hpp>

namespace py = pybind11;

PYBIND11_MODULE(hyperonpy, m) {
	m.doc() = "Python API of the Hyperon library";

	py::class_<Atom>(m, "Atom")
		.def("__eq__", &Atom::operator==)
		.def("__repr__", &Atom::to_string);

    m.def("S", &Atom::sym, "Create symbol atom");
    m.def("V", &Atom::var, "Create variable atom");
    m.def("E", &Atom::expr, "Create expression atom");
}

