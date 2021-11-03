from hyperonpy import (
        Atom,
        S,
        V,
        E as _E)

def E(*args):
    return _E(list(args))
