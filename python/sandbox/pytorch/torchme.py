from hyperon.atoms import *
from hyperon.ext import register_atoms
import torch


class TensorValue(MatchableObject):

    def __eq__(self, other):
        return isinstance(other, TensorValue) and\
               (self.content.shape == other.content.shape) and\
               (self.content == other.content).all()

    def match_(self, other):
        sh = self.content.shape
        bindings = {}
        if isinstance(other, GroundedAtom):
            other = other.get_object()
        # Match by equality with another TensorValue
        if isinstance(other, TensorValue):
            return [{}] if other == self else []

        if isinstance(other, ExpressionAtom):
            ch = other.get_children()
            # TODO: constructors and operations
            if len(ch) != sh[0]:
                return []
            for i in range(len(ch)):
                res = self.content[i]
                typ = _tensor_atom_type(res)
                res = TensorValue(res)
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

def _tensor_atom_type(npobj):
    return E(S('Tensor'), E(*[ValueAtom(s, 'Number') for s in npobj.shape]))


def wrapnpop(func):
    def wrapper(*args):
        a = [arg.get_object().value for arg in args]
        res = func(*a)
        typ = _tensor_atom_type(res)
        return [G(TensorValue(res), typ)]
    return wrapper


def create_tensor_from_data(*args):
    # Check if the argument list (or tuple) contains tensors with same shape
    if all(isinstance(arg, torch.Tensor) for arg in args):
        if all(arg.shape == args[0].shape for arg in args):
            t = torch.stack(args)
        else:
            raise ValueError("Chunks of data should have the same shape to stack tensor.")
    else:
        t = torch.tensor(args)
    return t


@register_atoms
def torchme_atoms():
    tmEmptyTensorAtom = G(PatternOperation('torch.empty', wrapnpop(lambda *args: torch.empty(args)), unwrap=False, rec=True))
    tmTensorAtom = G(PatternOperation('torch.tensor', wrapnpop(create_tensor_from_data), unwrap=False, rec=True))
    tmZerosTensorAtom = G(PatternOperation('torch.zeros', wrapnpop(lambda *args: torch.zeros(args)), unwrap=False, rec=True))
    tmOnesTensorAtom = G(PatternOperation('torch.ones', wrapnpop(lambda *args: torch.ones(args)), unwrap=False, rec=True))
    #tmManualSeedAtom = G(PatternOperation('torch.manual_seed', wrapnpop(lambda x: torch.manual_seed(x)), unwrap=False, rec=False))
    tmRandomTensorAtom = G(PatternOperation('torch.rand', wrapnpop(lambda *args: torch.rand(args)), unwrap=False, rec=True))
    return {
        r"torch\.empty": tmEmptyTensorAtom,
        r"torch\.tensor": tmTensorAtom,
        r"torch\.zeros": tmZerosTensorAtom,
        r"torch\.ones": tmOnesTensorAtom,
        #r"torch\.manual_seed": tmManualSeedAtom,
        r"torch\.rand": tmRandomTensorAtom
    }

