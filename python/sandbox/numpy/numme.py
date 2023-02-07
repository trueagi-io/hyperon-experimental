from hyperon.atoms import *
from hyperon.ext import register_atoms

import numpy as np

class NumpyValue(MatchableObject):

    def __eq__(self, other):
        return isinstance(other, NumpyValue) and\
               (self.content.shape == other.content.shape) and\
               (self.content == other.content).all()

    def match_(self, other):
        sh = self.content.shape
        bindings = {}
        if isinstance(other, GroundedAtom):
            other = other.get_object()
        # Match by equality with another NumpyValue
        if isinstance(other, NumpyValue):
            return [{}] if other == self else []
        # if isinstance(other, PatternValue):
        #     other = other.to_expr()
        if isinstance(other, ExpressionAtom):
            ch = other.get_children()
            # TODO: constructors and operations
            if len(ch) != sh[0]:
                return []
            for i in range(len(ch)):
                res = self.content[i]
                typ = _np_atom_type(res)
                res = NumpyValue(res)
                if isinstance(ch[i], VariableAtom):
                    bindings[ch[i].get_name()] = G(res, typ)
                elif isinstance(ch[i], ExpressionAtom):
                    bind_add = res.match_(ch[i])
                    if bind_add == []:
                        return []
                    bindings.update(bind_add[0])
        return [] if len(bindings) == 0 else [bindings]


class PatternValue(MatchableObject):

    def match_(self, other):
        if isinstance(other, GroundedAtom):
            other = other.get_object().content
        if not isinstance(other, PatternValue):
            return other.match_(self)
        # TODO: match to patterns
        return []


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
