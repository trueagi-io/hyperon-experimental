from .atoms import GroundedAtom, OperationAtom, ValueAtom, NoReduceError
from .ext import register_atoms, register_tokens

@register_atoms
def arithm_ops():
    subAtom = OperationAtom('-', lambda a, b: a - b, ['Number', 'Number', 'Number'])
    mulAtom = OperationAtom('*', lambda a, b: a * b, ['Number', 'Number', 'Number'])
    addAtom = OperationAtom('+', lambda a, b: a + b, ['Number', 'Number', 'Number'])
    divAtom = OperationAtom('/', lambda a, b: a / b, ['Number', 'Number', 'Number'])
    modAtom = OperationAtom('%', lambda a, b: a % b, ['Number', 'Number', 'Number'])
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
    greaterAtom = OperationAtom('>', lambda a, b: a > b, ['Number', 'Number', 'Bool'])
    lessAtom = OperationAtom('<', lambda a, b: a < b, ['Number', 'Number', 'Bool'])
    orAtom = OperationAtom('or', lambda a, b: a or b, ['Bool', 'Bool', 'Bool'])
    andAtom = OperationAtom('and', lambda a, b: a and b, ['Bool', 'Bool', 'Bool'])
    notAtom = OperationAtom('not', lambda a: not a, ['Bool', 'Bool'])
    return {
        r"==": equalAtom,
        r"<": lessAtom,
        r">": greaterAtom,
        r"or": orAtom,
        r"and": andAtom,
        r"not": notAtom
    }

@register_tokens
def type_tokens():
    return {
        r"\d+(\.\d+)": lambda token: ValueAtom(float(token), 'Number'),
        r"\d+" : lambda token: ValueAtom(int(token), 'Number'),
        "\"[^\"]*\"": lambda token: ValueAtom(str(token[1:-1]), 'String'),
        r"True|False": lambda token: ValueAtom(token == 'True', 'Bool')
    }


@register_tokens
def call_atom():
    def newCallAtom(token):
        # NOTE: we could use "call" as a plain symbol (insted of "call:...")
        #       with the method name as the parameter of call_atom_op
        #       (but this parameter should be unwrapped)
        # "call:..." is an interesting example of families of tokens for ops, though
        return OperationAtom(
                    token,
                    lambda obj, *args: call_atom_op(obj, token[5:], *args),
                    unwrap=False)

    def call_atom_op(atom, method_str, *args):
        if not isinstance(atom, GroundedAtom):
            # raise RuntimeError("call:" + method_str + " expects Python grounded atom")
            raise NoReduceError()
        obj = atom.get_object().value
        method = getattr(obj, method_str)
        result = method(*args)
        if result is None:
            return []
        # Fixme? getting results from call_atom raises some issues but convenient.
        # Running example is call:... &self (or another imported space)
        # However if we need to wrap the result into GroundedAtom, we don't know
        # its type. Also, if the method returns list, we can wrap it as whole or
        # can interpret it as multiple results.
        # Here, we don't wrap the list as whole, but wrap its elements even they
        # are atoms, for get_atoms to work nicely (wrapped list is not printed
        # nicely, while not wrapping atoms results in their further reduction)
        # This functionality can be improved/changed based on other more
        # important examples (e.g. dealing with DNN models) in the future,
        # while the core functions like &self.get_atoms can be dealt with
        # separately
        if not isinstance(result, list):
            result = [result]
        result = [ValueAtom(r) for r in result]
        #result = [r if isinstance(r, Atom) else ValueAtom(r) for r in result]
        return result

    return {
        r"call:[^\s]+": newCallAtom
    }
