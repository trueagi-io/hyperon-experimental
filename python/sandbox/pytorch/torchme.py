import json
import os
import re
from pydoc import locate

import torch
from hyperon.ext import register_tokens
from hyperon.atoms import *
from kwargsme import *
from parsing_exceptions import *

'''
Most of the functions defined in the torch module for working with tensors are implemented in C. 
Unfortunately, the standard inspect module doesn't allow to get signatures of built-in functions.
Thus, to automatically obtain function signatures, we need to parse the function documentation
using the 'parse_torch_func_signatures.py' script.
Parsed signatures are saved in the .json file.
Modify the file path according to its location in your environment
'''
TORCH_FUNC_SIGNATURES_PATH = 'torch_func_signatures.json'

torch_dtype = ['torch.float', 'torch.float16', 'torch.uint8', 'torch.float32', 'torch.float64', 'torch.double', 'torch.half',
               'torch.complex64', 'torch.cfloat', 'torch.cdouble', 'torch.complex128', 'torch.bfloat16', 'torch.int8',
               'torch.int16', 'torch.short', 'torch.int32', 'torch.int', 'torch.int64', 'torch.long', 'torch.bool']


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


def check_for_numbers_tuple(expression):
    pattern = r'^\((-?[0-9]+\.?[0-9]*\s)*-?[0-9]+\.?[0-9]*\)$'
    return bool(re.match(pattern, expression))


def check_list_structure(args):
    if not args:
        return False

    if not isinstance(args[0], GroundedAtom):
        return False

    for a in args[1:]:
        if not isinstance(a, ExpressionAtom):
            return False
    return True


class PatternOperation(OperationObject):

    def __init__(self, name, op, unwrap=False, rec=False):
        super().__init__(name, op, unwrap)
        self.rec = rec

    def execute(self, *args, res_typ=AtomType.UNDEFINED):
        if self.rec:
            if not isinstance(args[0], GroundedAtom):
                args = args[0].get_children()
                args_to_feed = []
                if check_list_structure(args):
                    args_to_feed.append(args[0])
                    for a in args[1:]:
                        if check_for_numbers_tuple(a.__repr__()):
                            a = a.get_children()
                            if all(isinstance(c, GroundedAtom) for c in a):
                                args_to_feed.append(a)
                        return super().execute(*args_to_feed, res_typ=res_typ)
                else:
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


def is_string_int(s):
    try:
        int(s)
        return True
    except ValueError:
        return False


def is_string_float(s):
    try:
        float(s)
        return '.' in s
    except ValueError:
        return False


def create_tensor(kwargs):
    if 'data' in kwargs:
        arg = kwargs['data']
        if isinstance(arg, torch.Tensor):
            return arg.clone().detach()

        if all(isinstance(a, torch.Tensor) for a in arg):
            if len(arg) == 1:
                return arg[0].clone().detach()
            if all(a.shape == arg[0].shape for a in arg):
                return torch.stack(arg)
            raise ValueError("Chunks of data should have the same shape to stack a tensor.")
        else:
            return torch.tensor(**kwargs)

    return torch.tensor([])


def get_output_grounded_atom(res):
    return G(TensorValue(res), _tensor_atom_type(res)) if isinstance(res, torch.Tensor) else ValueAtom(res)


def parse_res(res):
    if isinstance(res, torch.Tensor) or not isinstance(res, tuple):
        return get_output_grounded_atom(res)
    else:
        return [get_output_grounded_atom(r) for r in res]


def torch_function_decorator(func_name, ret_type, args_doc, module_name):
    def torch_function_wrapper(*_args):
        kwargs_to_feed = {}
        if module_name == 'torch':
            func = getattr(torch, func_name)
        elif module_name == 'Tensor':
            if isinstance(_args[0], torch.Tensor):
                func = getattr(_args[0], func_name)
                if len(_args) > 1:
                    _args = _args[1:]
                else:
                    _args=[]
        else:
            return G([])

        if len(_args) == 1 and isinstance(_args[0], GroundedAtom):
            arg = _args[0].get_object().content
            if isinstance(arg, Kwargs):
                kwargs_to_feed = arg.content
            else:
                kwargs_to_feed[args_doc[0]] = arg
        else:
            args = []
            for arg in _args:
                if isinstance(arg, SymbolAtom):
                    a = arg.get_name()
                    if is_complex(a):
                        a = complex(a)
                    elif is_string_float(a):
                        a = float(a)
                    args.append(a)
                elif isinstance(arg, list):
                    a_list = [a.get_object().value for a in arg]
                    args.append(a_list)
                elif isinstance(arg, GroundedAtom):
                    args.append(arg.get_object().value)
                else:
                    args.append(arg)

            if len(args_doc) == 1 and len(args) > 1:
                args = [args]

            for i, val in enumerate(args):
                kwargs_to_feed[args_doc[i]] = val

        if func_name == "result_type":
            types = (torch.Tensor, int, float, bool, complex)
            tensor1, tensor2 = kwargs_to_feed["tensor1"], kwargs_to_feed["tensor2"]
            type_tensor1, type_tensor2 = [isinstance(t, types) for t in [tensor1, tensor2]]

            if type_tensor1 and type_tensor2:
                name = "other"
                if isinstance(tensor1, torch.Tensor):
                    kwargs_to_feed["tensor"] = tensor1
                else:
                    kwargs_to_feed["scalar"] = tensor1
                    name = "scalar"
                kwargs_to_feed[name] = tensor2
            else:
                kwargs_to_feed["scalar1"] = tensor1
                kwargs_to_feed["scalar2"] = tensor2

            kwargs_to_feed.pop("tensor1")
            kwargs_to_feed.pop("tensor2")

        if 'indices_or_sections' in kwargs_to_feed:
            if isinstance(kwargs_to_feed['indices_or_sections'], int):
                kwargs_to_feed['sections'] = kwargs_to_feed['indices_or_sections']
            else:
                kwargs_to_feed['indices'] = kwargs_to_feed['indices_or_sections']
            kwargs_to_feed.pop('indices_or_sections')

        if 'dtype' in kwargs_to_feed:
            if isinstance(kwargs_to_feed['dtype'], str) and 'torch.' in kwargs_to_feed['dtype']:
                kwargs_to_feed['dtype'] = locate(f"{kwargs_to_feed['dtype']}")

        if func_name == 'tensor':
            res = create_tensor(kwargs_to_feed)
        else:
            res = func(**kwargs_to_feed)

        if isinstance(res, tuple):
            return parse_res(res)
        elif ret_type in ['Tensor', 'LongTensor', 'bool', '(bool)', 'int', 'tuple or int', 'seq', 'dtype', 'Size', 'torch.Size or int', None]:
            return [get_output_grounded_atom(res)]

    return torch_function_wrapper


@register_tokens
def call_torchme_atoms():

    with open(TORCH_FUNC_SIGNATURES_PATH, 'r') as file:
        torch_func_signatures = json.load(file)

    def new_torch_atom(token):
        # Here are some exceptions to automatic parsing results
        if token in torch_dtype:
            return S(token)
        if token in parsing_exceptions:
            return G(OperationObject(token, parsing_exceptions[token][0], parsing_exceptions[token][1]))

        func_name = token[6:]
        tfs = [t for t in torch_func_signatures if t.get('func_name') == func_name]
        if not tfs:
            raise ValueError(f"Can't find signature for function torch.{func_name}")
        tfs = tfs[0]
        ret_type = tfs['ret_type']
        args = []
        # kwargs = []
        for key, value in tfs['signature'].items():
            if value['type'] in ['Arguments', 'Args']:
                args.append(key)
            # elif value['type'] in ['Keyword args', 'Keyword arguments']:
            #     kwargs.append(key)

        wrapped_func = torch_function_decorator(func_name, ret_type, args, tfs['module'])

        if tfs['module'] == 'Tensor':
            return G(OperationObject(f'torch:{func_name}',
                                     wrapped_func,
                                     unwrap=True))
        elif tfs['module'] == 'torch':
            return G(PatternOperation(f'torch:{func_name}',
                                      wrapped_func,
                                      unwrap=False,
                                      rec=True))
        else:
            return G([])

    return {
        r'torch\:[^\s^\.]+': new_torch_atom,
        r"kwargs": lambda _: G(PatternOperation('kwargs', pairs_to_kwargs))
    }

