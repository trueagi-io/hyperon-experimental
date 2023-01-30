from hyperon.atoms import *
from hyperon.ext import register_atoms

import numpy as np

class NumpyValue(ValueObject):

    def __eq__(self, other):
        return isinstance(other, NumpyValue) and\
               (self.content.shape == other.content.shape) and\
               (self.content == other.content).all()

def _np_atom_type(npobj):
    return E(S('NPArray'), E(*[ValueAtom(s, 'Number') for s in npobj.shape]))

def wrapnpop(func):
    def wrapper(*args):
        a = [arg.get_object().value for arg in args]
        res = func(*a)
        typ = _np_atom_type(res)
        return [G(NumpyValue(res), typ)]
    return wrapper

@register_atoms
def numme_atoms():

    def nmvector_op(*args):
        npobj = np.array([a.get_object().value for a in args])
        typ = _np_atom_type(npobj)
        return [G(NumpyValue(npobj), typ)]

    def nmarray_op(data):
        def _to_array(expr):
            arr = []
            for a in expr.get_children():
                v = None
                match a.get_type():
                    case AtomKind.EXPR:
                        v = _to_array(a)
                    case AtomKind.GROUNDED:
                        v = a.get_object().value
                    case _:
                        raise Exception("Unexpected element for np.array: " + str(a))
                arr += [v]
            return arr
        arr = _to_array(data)
        npobj = np.array(arr)
        typ = _np_atom_type(npobj)
        return [G(NumpyValue(npobj), typ)]

    # FIXME: we don't add types for operations, because numpy operations types
    # are too loose
    nmVectorAtom = OperationAtom('np.vector', nmvector_op, unwrap=False)
    nmArrayAtom = OperationAtom('np.array', nmarray_op, unwrap=False)
    nmAddAtom = OperationAtom('np.add', wrapnpop(np.add), unwrap=False)
    nmSubAtom = OperationAtom('np.sub', wrapnpop(np.subtract), unwrap=False)
    nmMulAtom = OperationAtom('np.mul', wrapnpop(np.multiply), unwrap=False)
    nmDivAtom = OperationAtom('np.div', wrapnpop(np.divide), unwrap=False)
    nmMMulAtom = OperationAtom('np.matmul', wrapnpop(np.matmul), unwrap=False)

    return {
        r"np\.vector": nmVectorAtom,
        r"np\.array": nmArrayAtom,
        r"np\.add": nmAddAtom,
        r"np\.sub": nmSubAtom,
        r"np\.mul": nmMulAtom,
        r"np\.matmul": nmMMulAtom,
        r"np\.div": nmDivAtom
    }
