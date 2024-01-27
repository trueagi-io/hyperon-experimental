from hyperon.atoms import OperationAtom
from hyperon.ext import register_atoms
from bhv.np import NumPyBoolBHV as BHV, NumPyBoolPermutation as Perm


@register_atoms
def my_atoms():
    return {
        'bhv-new': OperationAtom('bhv-new', BHV.rand),
        'bhv-majority': OperationAtom('bhv-majority', lambda *args: BHV.majority(list(args))),
        'bhv-bind': OperationAtom('bhv-bind', lambda a,b: a^b),
        'bhv-std-apart-relative': OperationAtom('bhv-std-apart-relative', lambda a,b: a.std_apart(b, relative = True)),
        'bhv-is-related': OperationAtom('bhv-is-related', lambda a,b: a.related(b)),
        'bhv-new-perm': OperationAtom('bhv-perm', Perm.random),
        'bhv-apply-perm': OperationAtom('bhv-apply-perm', lambda a, b: a(b))
    }
