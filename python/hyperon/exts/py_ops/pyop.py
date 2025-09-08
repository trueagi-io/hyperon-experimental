from hyperon import *
from hyperon.ext import register_atoms, register_tokens

@register_tokens
def arithm_types():
    return {
        r"True|False": lambda token: ValueAtom(token == "True", 'Bool'),
        r"[-+]?\d+": lambda token: ValueAtom(int(token), 'Number'),
        r"[-+]?\d+\.\d+": lambda token: ValueAtom(float(token), 'Number'),
        r"[-+]?\d+(\.\d+)?[eE][-+]?\d+": lambda token: ValueAtom(float(token), 'Number'),
        r"(?s)^\".*\"$": lambda token: ValueAtom(str(token[1:-1]), 'String'),
    }

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
    greaterAtom = OperationAtom('>', lambda a, b: a > b) #, ['Number', 'Number', 'Bool'])
    lessAtom = OperationAtom('<', lambda a, b: a < b) #, ['Number', 'Number', 'Bool'])
    greaterEqAtom = OperationAtom('>=', lambda a, b: a >= b) #, ['Number', 'Number', 'Bool'])
    lessEqAtom = OperationAtom('<=', lambda a, b: a <= b) #, ['Number', 'Number', 'Bool'])
    orAtom = OperationAtom('or', lambda a, b: a or b) #, ['Bool', 'Bool', 'Bool'])
    andAtom = OperationAtom('and', lambda a, b: a and b) #, ['Bool', 'Bool', 'Bool'])
    notAtom = OperationAtom('not', lambda a: not a) #, ['Bool', 'Bool'])
    return {
        r"<": lessAtom,
        r">": greaterAtom,
        r"<=": lessEqAtom,
        r">=": greaterEqAtom,
        r"or": orAtom,
        r"and": andAtom,
        r"not": notAtom
    }
