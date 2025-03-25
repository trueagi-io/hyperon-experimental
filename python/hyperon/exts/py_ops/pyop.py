from hyperon import *
from hyperon.ext import register_atoms

@register_atoms
def arithm_ops():
    # The whole idea of overloading these operations is for them
    # to operate not necessarily on Numbers
    subAtom = OperationAtom('-', lambda a, b: a - b) #, ['Number', 'Number', 'Number'])
    mulAtom = OperationAtom('*', lambda a, b: a * b) #, ['Number', 'Number', 'Number'])
    addAtom = OperationAtom('+', lambda a, b: a + b) #, ['Number', 'Number', 'Number'])
    divAtom = OperationAtom('/', lambda a, b: a / b) #, ['Number', 'Number', 'Number'])
    modAtom = OperationAtom('%', lambda a, b: a % b) #, ['Number', 'Number', 'Number'])
    return {
        r"\+": addAtom,
        r"-": subAtom,
        r"\*": mulAtom,
        r"/": divAtom,
        r"%": modAtom
    }

@register_atoms
def bool_ops():
    equalAtom = OperationAtom('==', lambda a, b: [ValueAtom(a == b, 'Bool')],
                              ['$t', '$t', 'Bool'], unwrap=False)
    greaterAtom = OperationAtom('>', lambda a, b: a > b) #, ['Number', 'Number', 'Bool'])
    lessAtom = OperationAtom('<', lambda a, b: a < b) #, ['Number', 'Number', 'Bool'])
    greaterEqAtom = OperationAtom('>=', lambda a, b: a >= b) #, ['Number', 'Number', 'Bool'])
    lessEqAtom = OperationAtom('<=', lambda a, b: a <= b) #, ['Number', 'Number', 'Bool'])
    orAtom = OperationAtom('or', lambda a, b: a or b) #, ['Bool', 'Bool', 'Bool'])
    andAtom = OperationAtom('and', lambda a, b: a and b) #, ['Bool', 'Bool', 'Bool'])
    notAtom = OperationAtom('not', lambda a: not a) #, ['Bool', 'Bool'])
    return {
        r"==": equalAtom,
        r"<": lessAtom,
        r">": greaterAtom,
        r"<=": lessEqAtom,
        r">=": greaterEqAtom,
        r"or": orAtom,
        r"and": andAtom,
        r"not": notAtom
    }
