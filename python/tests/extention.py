from hyperon.atoms import OperationAtom, ValueAtom
from hyperon.ext import add_atoms

@add_atoms
def my_dict_atoms():
    return {
        '&my-dict': ValueAtom({'A': 5, 6: 'B'}),
        'get-by-key': OperationAtom('get-by-key', lambda d, k: d[k])
        }
