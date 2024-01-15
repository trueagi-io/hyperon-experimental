import importlib
import numbers

import torch
from hyperon.atoms import *
from hyperon.ext import register_atoms


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
            raise ValueError("Chunks of data should have the same shape to stack a tensor.")
    else:
        t = torch.tensor(args)
    return t


def tm_add(*args):
    nargs = len(args)
    if nargs > 2:
        if isinstance(args[2], numbers.Number):
            t = torch.add(args[0], args[1], alpha=args[2])
        else:
            raise ValueError(f"The third parameter for the torch.add() should be a scalar value, but got {type(args[2])} instead")

    else:
        t = torch.add(*args)

    return t


def tm_sub(*args):
    nargs = len(args)
    if nargs > 2:
        if isinstance(args[2], numbers.Number):
            t = torch.add(args[0], args[1], alpha=args[2])
        else:
            raise ValueError(f"The third parameter for the torch.sub() should be a scalar value, but got {type(args[2])} instead")

    else:
        t = torch.sub(*args)

    return t


def import_module(*args):
    torch_module_name = args[0].get_name()
    pymodule_name = args[1].get_name()
    pymodule = importlib.import_module(pymodule_name)
    module_class = getattr(pymodule, torch_module_name)

    if len(args) > 2:
        a = []
        for arg in args[2:]:
            if isinstance(arg, GroundedAtom):
                if isinstance(arg.get_object(), GroundedObject):
                    a.append(arg.get_object().content)
                else:
                    a.append(arg.get_object().value)
            elif isinstance(arg, SymbolAtom):
                if arg.get_name() == 'None':
                    a.append(None)
                else:
                    a.append(arg.get_name())

        module_instance = module_class(*a)
    else:
        module_instance = module_class()

    return [G(GroundedObject(module_instance))]


def to_device(*args):
    torch_object = None
    device = None
    if isinstance(args[0], GroundedAtom):
        if isinstance(args[0].get_object(), GroundedObject):
            torch_object = args[0].get_object().content
        else:
            torch_object = args[0].get_object().value

    if isinstance(args[1], SymbolAtom):
        device = args[1].get_name()

    torch_object.to(device=device)

    return [G(GroundedObject(torch_object))]


def run_trainer(*args):
    trainer = args[0]
    nepochs = args[1]
    for t in range(nepochs):
        print(f"Epoch {t + 1}\n-------------------------------")
        trainer.train()
        trainer.test()
    return

@register_atoms
def torchme_atoms():
    tmEmptyTensorAtom = G(PatternOperation('torch.empty', wrapnpop(lambda *args: torch.empty(args)), unwrap=False, rec=True))
    tmTensorAtom = G(PatternOperation('torch.tensor', wrapnpop(create_tensor_from_data), unwrap=False, rec=True))
    tmZerosTensorAtom = G(PatternOperation('torch.zeros', wrapnpop(lambda *args: torch.zeros(args)), unwrap=False, rec=True))
    tmOnesTensorAtom = G(PatternOperation('torch.ones', wrapnpop(lambda *args: torch.ones(args)), unwrap=False, rec=True))
    tmManualSeedAtom = G(OperationObject('torch.manual_seed', lambda x: torch.manual_seed(x), unwrap=True))
    tmRandomTensorAtom = G(PatternOperation('torch.rand', wrapnpop(lambda *args: torch.rand(args)), unwrap=False, rec=True))
    tmAddAtom = G(PatternOperation('torch.add', wrapnpop(tm_add), unwrap=False, rec=True))
    tmAbsAtom = G(PatternOperation('torch.abs', wrapnpop(torch.abs), unwrap=False))
    tmSubAtom = G(PatternOperation('torch.sub', wrapnpop(tm_sub), unwrap=False, rec=True))
    tmMulAtom = G(PatternOperation('torch.mul', wrapnpop(torch.mul), unwrap=False, rec=True))
    tmDivAtom = G(PatternOperation('torch.div', wrapnpop(torch.div), unwrap=False, rec=True))
    tmMatMulAtom = G(PatternOperation('torch.matmul', wrapnpop(torch.matmul), unwrap=False, rec=True))
    tmMeanAtom = G(PatternOperation('torch.mean', wrapnpop(torch.mean), unwrap=False))


    tmImportModelAtom = G(OperationObject('torch.import_module', import_module, unwrap=False))

    tmReqGradStatusAtom = G(OperationObject('torch.requires_grad_status', lambda x: x.requires_grad, unwrap=True))
    tmReqGradAtom = G(OperationObject('torch.requires_grad', lambda x, b: x.requires_grad_(b), unwrap=True))
    tmBackwardAtom = G(OperationObject('torch.backward', lambda x: x.backward(), unwrap=True))
    tmToDeviceAtom = G(OperationObject('torch.to_device', to_device, unwrap=False))
    tmGetModelParamsAtom = G(OperationObject('torch.get_model_params', lambda x: x.parameters(), unwrap=True))
    tmRunTrainerAtom = G(OperationObject('torch.run_trainer', run_trainer, unwrap=True))

    return {
        r"torch\.empty": tmEmptyTensorAtom,
        r"torch\.tensor": tmTensorAtom,
        r"torch\.zeros": tmZerosTensorAtom,
        r"torch\.ones": tmOnesTensorAtom,
        r"torch\.manual_seed": tmManualSeedAtom,
        r"torch\.rand": tmRandomTensorAtom,
        r"torch\.add": tmAddAtom,
        r"torch\.abs": tmAbsAtom,
        r"torch\.sub": tmSubAtom,
        r"torch\.mul": tmMulAtom,
        r"torch\.div": tmDivAtom,
        r"torch\.matmul": tmMatMulAtom,
        r"torch\.mean": tmMeanAtom,


        r"torch\.import_module": tmImportModelAtom,
        r"torch\.requires_grad_status": tmReqGradStatusAtom,
        r"torch\.requires_grad": tmReqGradAtom,
        r"torch\.backward": tmBackwardAtom,
        r"torch\.to_device": tmToDeviceAtom,
        r"torch\.get_model_params": tmGetModelParamsAtom,
        r"torch\.run_trainer": tmRunTrainerAtom
    }

