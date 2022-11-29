from hyperon.atoms import OperationAtom, ValueAtom
from hyperon.ext import *

@register_atoms
def my_dict_atoms():
    return {
        '&my-dict': ValueAtom({'A': 5, 6: 'B'}),
        'get-by-key': OperationAtom('get-by-key', lambda d, k: d[k])
        }

@register_tokens(pass_metta=True)
def my_get_runner(metta):
    return {
        '&runner': lambda _: ValueAtom(metta)
    }
