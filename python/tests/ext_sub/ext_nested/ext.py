from hyperon.atoms import OperationAtom, ValueAtom
from hyperon.ext import *

g_object = None
def set_global(v):
    global g_object
    g_object = v

@register_atoms
def my_dict_atoms():
    return {
        '&my-dict': ValueAtom({'A': 5, 6: 'B'}),
        'get-by-key': OperationAtom('get-by-key', lambda d, k: d[k])
        }

@register_atoms
def my_glob_atoms():
    return {
        'set-global!': OperationAtom("set-global!", set_global),
        'get-global': OperationAtom("get-global", lambda: g_object),
        }

@register_tokens(pass_metta=True)
def my_get_runner(metta):
    return {
        '&runner': lambda _: ValueAtom(metta)
    }
