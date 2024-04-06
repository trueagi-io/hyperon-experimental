from hyperon.atoms import OperationAtom, OperationObject, GroundedAtom, ValueAtom, ExpressionAtom, SymbolAtom, ValueObject
from hyperon.ext import register_atoms
import os
import sys

def groundedatom_to_python_object(a):
    obj = a.get_object()
    if isinstance(obj, ValueObject):
        obj = obj.value
    if isinstance(obj, OperationObject):
        obj = obj.content
    # At this point it is already python object
    if isinstance(obj, PythonCaller):
        obj = obj.obj
    return obj

def tuple_to_keyvalue(a):
    ac = a.get_children()
    if len(ac) != 2:
        raise Exception("Syntax error in tuple_to_keyvalue")
    return str(ac[0]), groundedatom_to_python_object(ac[1])

def atoms_to_args(*atoms):
    args = []
    kwargs = {}
    for a in atoms:
        if isinstance(a, GroundedAtom):
            args.append(groundedatom_to_python_object(a))
        elif isinstance(a, ExpressionAtom):
            k,v = tuple_to_keyvalue(a)
            kwargs[k] = v
        else:
            raise Exception(f"Unexpected error: {a},  {type(a)}")
    return args, kwargs

class PythonCaller:
    def __init__(self, obj):
        self.obj = obj

    def __call__(self, *atoms):
        args, kwargs = atoms_to_args(*atoms)
        return [ValueAtom(PythonCaller(self.obj(*args, **kwargs)))]

def _import_and_create_operationatom(metta, import_str, obj):

    # we only need these 3 lines to import from the current directory
    # TODO fix it somehow differently
    current_directory = os.getcwd()
    if current_directory not in sys.path:
        sys.path.append(current_directory)

    local_scope = {}
    exec(import_str, {}, local_scope)
    oatom = OperationAtom(obj, PythonCaller(local_scope[obj]), unwrap = False)
    metta.register_atom(obj, oatom)


def import_from(metta, lib, i, obj):
    if str(i) != "import":
        raise Exception("bad import syntax")
    lib = str(lib)
    obj = str(obj)
    _import_and_create_operationatom(metta, f"from {lib} import {obj}", obj)
    return []

def import_as(metta, lib, a, obj):
    if str(a) != "as":
        raise Exception("bad import syntax")
    lib = str(lib)
    obj = str(obj)
    _import_and_create_operationatom(metta, f"import {lib} as {obj}", obj)
    return []

def call_with_dot(*atoms):
    if len(atoms) < 2:
        raise Exception("Syntax error")
    obj = groundedatom_to_python_object(atoms[0])
    method = str(atoms[1])
    atoms = atoms[2:]
    args, kwargs = atoms_to_args(*atoms)
    rez = getattr(obj, method)(*args, **kwargs)
    return [ValueAtom(PythonCaller(rez))]

def __unwrap(obj):
    return obj.obj

@register_atoms(pass_metta=True)
def my_atoms(metta):
    return {'import_from': OperationAtom('import_from', lambda *args: import_from(metta, *args), unwrap = False),
            'import_as':   OperationAtom('import_as',   lambda *args: import_as  (metta, *args), unwrap = False)}

@register_atoms()
def my_atoms2():
    return {'__unwrap': OperationAtom('__unwrap', __unwrap),
            "call_dot": OperationAtom("call_dot", call_with_dot, unwrap = False)}

# The functions which are not required for import, but nice for examples

# convert nested tuples to nested python tuples
def _ptuple(*atoms):
    rez = []
    for a in atoms:
        if isinstance(a, GroundedAtom):
            rez.append(groundedatom_to_python_object(a))
        elif isinstance(a, ExpressionAtom):
            rez.append(_ptuple(*a.get_children()))
    return tuple(rez)

def ptuple(*atoms):
    return [ValueAtom(_ptuple(*atoms))]

# convert pair of tuples to python dictionary
def pdict(*atoms):
    return [ValueAtom(dict([tuple_to_keyvalue(a) for a in atoms]))]

# chain python objects with |  (syntactic sugar for langchain)
def chain(*atoms):
    objects = [groundedatom_to_python_object(a) for a in atoms]
    result = objects[0]
    for obj in objects[1:]:
        result = result | obj
    return [ValueAtom(PythonCaller(result))]

@register_atoms()
def my_atoms3():
    return {"ptuple": OperationAtom("ptuple", ptuple, unwrap = False),
            "pdict":  OperationAtom("pdict",  pdict,  unwrap = False),
            "chain":  OperationAtom("chain",  chain,  unwrap = False)}
