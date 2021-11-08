from hyperonpy import (
        Atom,
        GroundedAtom,
        S,
        V,
        E as _E,
        G)

def E(*args):
    return _E(list(args))

def ValueAtom(value):
    return G(Value(value))

class Value(GroundedAtom):

    def __init__(self, value):
        GroundedAtom.__init__(self)
        self.value = value

    def __eq__(self, other):
        if isinstance(other, Value):
            return self.value == other.value
        return False

    def __repr__(self):
        return repr(self.value)

    def copy(self):
        return Value(self.value)
