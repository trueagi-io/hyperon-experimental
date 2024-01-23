import importlib
import numbers

import torch
from hyperon.atoms import *
from hyperon.ext import register_atoms
import os
import json
from pydoc import locate
import re

'''
Most of the functions defined in the torch module for working with tensors are implemented in C. 
Unfortunately, the standard inspect module doesn't allow to get signatures of built-in functions.
Thus, to automatically obtain function signatures, we need to parse the function documentation
using the 'parse_torch_func_signatures.py' script.
Parsed signatures are saved in the .json file.
Modify the file path according to its location in your environment
'''
TORCH_FUNC_SIGNATURES_PATH = 'torch_func_signatures.json'

if not os.path.isfile(TORCH_FUNC_SIGNATURES_PATH):
    raise FileNotFoundError(f'{TORCH_FUNC_SIGNATURES_PATH} does not exist')


class TensorValue(MatchableObject):

    def __eq__(self, other):
        return isinstance(other, TensorValue) and \
            (self.content.shape == other.content.shape) and \
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
            if not isinstance(args[0], GroundedAtom):
                args = args[0].get_children()
                args = [self.execute(arg)[0] \
                        if isinstance(arg, ExpressionAtom) else arg for arg in args]
        # If there is a variable or PatternValue in arguments, create PatternValue
        # instead of executing the operation
        for arg in args:
            if isinstance(arg, GroundedAtom) and \
                    isinstance(arg.get_object(), PatternValue) or \
                    isinstance(arg, VariableAtom):
                return [G(PatternValue([self, args]))]
        return super().execute(*args, res_typ=res_typ)


def _tensor_atom_type(npobj):
    return E(S('Tensor'), E(*[ValueAtom(s, 'Number') for s in npobj.shape]))


def instantiate_module(*args):
    torch_module_name = args[0].get_name()
    pymodule_name = args[1].get_name()
    pymodule = importlib.import_module(pymodule_name)
    module_class = getattr(pymodule, torch_module_name)

    if len(args) > 2:
        a = []
        kw = []
        for arg in args[2:]:
            if isinstance(arg, GroundedAtom):
                if isinstance(arg.get_object(), GroundedObject):
                    obj_cont = arg.get_object().content
                    if isinstance(obj_cont, Kwargs):
                        kw = obj_cont.content
                    else:
                        a.append(arg.get_object().content)
                else:
                    a.append(arg.get_object().value)
            elif isinstance(arg, SymbolAtom):
                if arg.get_name() == 'None':
                    a.append(None)
                else:
                    a.append(arg.get_name())
        if len(kw) > 0:
            module_instance = module_class(**kw)
        else:
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


class Kwargs(MatchableObject):
    def __init__(self, content=None, id=None):
        super().__init__(content, id)
        if content is None:
            self.content = {}

    def __len__(self):
        return len(self.content)

    def match_(self, other):
        new_bindings_set = BindingsSet.empty()
        p = other.get_children()
        if isinstance(p[0], SymbolAtom):
            key = p[0].get_name()
            var = p[1]
            if key in self.content:
                val = ValueAtom(self.content[key])
                bindings = Bindings()
                bindings.add_var_binding(var, val)
                new_bindings_set.push(bindings)

        return new_bindings_set


def pairs_to_kwargs(pairs):
    kwargs = Kwargs()
    pairs_children = pairs.get_children()
    for pair in pairs_children:
        p = pair.iterate()

        if isinstance(p[0], SymbolAtom):
            key = p[0].get_name()
            if isinstance(p[1], GroundedAtom):
                kwargs.content[key] = p[1].get_object().value
            elif isinstance(p[1], SymbolAtom):
                v = p[1].get_name()
                if v == 'None':
                    kwargs.content[key] = None
                else:
                    kwargs.content[key] = v

    return [G(GroundedObject(kwargs))]


def is_complex(s):
    complex_pattern = "([-]?\d+(\.\d+)?[+-]\d+(\.\d+)?[jJ])|(\d+(\.\d+)?[jJ])"
    pattern = re.compile(complex_pattern)
    try:
        if re.match(pattern, s):
            return True
        else:
            return False
    except ValueError:
        return False

def is_float(s):
    try:
        float(s)
        return '.' in s
    except ValueError:
        return False

def torch_function_decorator(func_name, ret_type, args_doc, kwargs_doc):
    def torch_function_wrapper(*_args):
        kwargs_to_feed = {}
        func = getattr(torch, func_name)
        if len(_args) == 1 and isinstance(_args[0], GroundedAtom):
            arg = _args[0].get_object().content
            if isinstance(arg, Kwargs):
                kwargs_to_feed = arg.content
            else:
                kwargs_to_feed[args_doc[0]] = arg
        else:
            args=[]
            for arg in _args:
                if isinstance(arg, SymbolAtom):
                    a = arg.get_name()
                    if is_complex(a):
                        a = complex(a)
                    elif is_float(a):
                        a = float(a)
                    args.append(a)
                else:
                    args.append(arg.get_object().value)
            # args = [arg.get_object().value for arg in _args]

            if len(args_doc) == 1 and len(args) > 1:
                args = [args]
            kwargs_list = args_doc + kwargs_doc
            nargs_doc = len(kwargs_list)

            for i, val in enumerate(args):
                kwargs_to_feed[args_doc[i]] = val

        if 'dtype' in kwargs_to_feed:
            if isinstance(kwargs_to_feed['dtype'], str) and 'torch.' in kwargs_to_feed['dtype']:
                kwargs_to_feed['dtype'] = locate(f"{kwargs_to_feed['dtype']}")

        if func_name == 'tensor':
            # Check if the argument list (or tuple) contains tensors with same shape
            if all(isinstance(arg, torch.Tensor) for arg in args):
                if len(args) == 1:
                    res = args[0].clone().detach()
                elif all(arg.shape == args[0].shape for arg in args):
                    res = torch.stack(args)
                else:
                    raise ValueError("Chunks of data should have the same shape to stack a tensor.")
            else:
                res = torch.tensor(args)

        else:
            res = func(**kwargs_to_feed)

        if isinstance(res, tuple):
            result = []
            for r in res:
                if isinstance(r, torch.Tensor):
                    typ = _tensor_atom_type(r)
                    result.append(G(TensorValue(r), typ))
            return result
        elif ret_type in ['Tensor', 'LongTensor']:
            typ = _tensor_atom_type(res)
            return [G(TensorValue(res), typ)]
        elif ret_type in ['bool', '(bool)', 'int']:
            return [ValueAtom(res)]
        elif ret_type == '(Tensor, Tensor[])':
            return []
        elif ret_type == 'List of Tensors':
            return []
        elif ret_type == 'seq':
            return []
        elif ret_type == 'LongTensor or tuple of LongTensors':
            return []
        elif ret_type == 'dtype':
            return []

    return torch_function_wrapper, False, True


@register_atoms
def torchme_atoms():
    with open(TORCH_FUNC_SIGNATURES_PATH, 'r') as file:
        torch_func_signatures = json.load(file)
    atoms_to_reg = {}
    for tfs in torch_func_signatures:
        func_name = tfs['func_name']
        ret_type = tfs['ret_type']
        args = []
        kwargs = []
        for key, value in tfs['signature'].items():
            if value['type'] in ['Arguments', 'Args']:
                args.append(key)
            elif value['type'] in ['Keyword args', 'Keyword arguments']:
                kwargs.append(key)

        wrapped_func, unwrap, rec = torch_function_decorator(func_name, ret_type, args, kwargs)
        atoms_to_reg[f'torch.{func_name}'] = G(PatternOperation(f'torch.{func_name}',
                                                                wrapped_func,
                                                                unwrap=unwrap,
                                                                rec=rec))

    tmKwargsAtom = G(PatternOperation('kwargs', pairs_to_kwargs))
    atoms_to_reg.update({'kwargs': tmKwargsAtom})
    tmManualSeedAtom = G(OperationObject('torch.manual_seed', lambda x: torch.manual_seed(x), unwrap=True))
    atoms_to_reg.update({'torch.manual_seed': tmManualSeedAtom})

    tmInstantiateModuleAtom = G(OperationObject('torch.instantiate_module', instantiate_module, unwrap=False))
    atoms_to_reg.update({'torch.instantiate_module': tmInstantiateModuleAtom})

    tmReqGradStatusAtom = G(OperationObject('torch.requires_grad_status', lambda x: x.requires_grad, unwrap=True))
    atoms_to_reg.update({'torch.requires_grad_status': tmReqGradStatusAtom})

    tmReqGradAtom = G(OperationObject('torch.requires_grad', lambda x, b: x.requires_grad_(b), unwrap=True))
    atoms_to_reg.update({'torch.requires_grad': tmReqGradAtom})
    tmBackwardAtom = G(OperationObject('torch.backward', lambda x: x.backward(), unwrap=True))
    atoms_to_reg.update({'torch.backward': tmBackwardAtom})
    tmToDeviceAtom = G(OperationObject('torch.to_device', to_device, unwrap=False))
    atoms_to_reg.update({'torch.to_device':tmToDeviceAtom})
    tmGetModelParamsAtom = G(OperationObject('torch.get_model_params', lambda x: x.parameters(), unwrap=True))
    atoms_to_reg.update({'torch.get_model_params':tmGetModelParamsAtom})
    tmRunTrainerAtom = G(OperationObject('torch.run_trainer', run_trainer, unwrap=True))
    atoms_to_reg.update({'torch.run_trainer':tmRunTrainerAtom})

    return atoms_to_reg
