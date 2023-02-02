from hyperon.atoms import *
from hyperon.ext import register_atoms

import numpy as np

class NumpyValue(ValueObject):

    def __eq__(self, other):
        return isinstance(other, NumpyValue) and\
               (self.content.shape == other.content.shape) and\
               (self.content == other.content).all()


class PatternValue(ValueObject):

    pass


class PatternOperation(OperationObject):

    def __init__(self, name, op, unwrap=False, rec=False):
        super().__init__(name, op, unwrap)
        self.rec = rec

    def execute(self, *args, res_typ=AtomType.UNDEFINED):
        if self.rec:
            args = args[0].get_children()
            args = [self.execute(arg)[0]\
                if isinstance(arg, ExpressionAtom) else arg for arg in args]
        # If there is a variable or PatternValue in arguments, create PatternValue
        # instead of executing the operation
        for arg in args:
            if isinstance(arg, GroundedAtom) and\
               isinstance(arg.get_object(), PatternValue) or\
               isinstance(arg, VariableAtom):
                return [G(PatternValue([self, args]))]
        return super().execute(*args, res_typ=res_typ)


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

    # FIXME: we don't add types for operations, because numpy operations types
    # are too loose
    nmVectorAtom = G(PatternOperation('np.vector', wrapnpop(lambda *args: np.array(args)), unwrap=False))
    nmArrayAtom = G(PatternOperation('np.array', wrapnpop(lambda *args: np.array(args)), unwrap=False, rec=True))
    nmAddAtom = G(PatternOperation('np.add', wrapnpop(np.add), unwrap=False))
    nmSubAtom = G(PatternOperation('np.sub', wrapnpop(np.subtract), unwrap=False))
    nmMulAtom = G(PatternOperation('np.mul', wrapnpop(np.multiply), unwrap=False))
    nmDivAtom = G(PatternOperation('np.div', wrapnpop(np.divide), unwrap=False))
    nmMMulAtom = G(PatternOperation('np.matmul', wrapnpop(np.matmul), unwrap=False))

    return {
        r"np\.vector": nmVectorAtom,
        r"np\.array": nmArrayAtom,
        r"np\.add": nmAddAtom,
        r"np\.sub": nmSubAtom,
        r"np\.mul": nmMulAtom,
        r"np\.matmul": nmMMulAtom,
        r"np\.div": nmDivAtom
    }
