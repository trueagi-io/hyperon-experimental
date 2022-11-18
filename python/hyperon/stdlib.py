from .atoms import GroundedAtom, OperationAtom, ValueAtom, NoReduceError

#E(S('->'), S('Number'), S('Number'), S('Number'))
subAtom = OperationAtom('-', lambda a, b: a - b, ['Number', 'Number', 'Number'])
mulAtom = OperationAtom('*', lambda a, b: a * b, ['Number', 'Number', 'Number'])
addAtom = OperationAtom('+', lambda a, b: a + b, ['Number', 'Number', 'Number'])
divAtom = OperationAtom('/', lambda a, b: a / b, ['Number', 'Number', 'Number'])
modAtom = OperationAtom('%', lambda a, b: a % b, ['Number', 'Number', 'Number'])
equalAtom = OperationAtom('==', lambda a, b: [ValueAtom(a == b, 'Bool')],
                          ['$t', '$t', 'Bool'], unwrap=False)
greaterAtom = OperationAtom('>', lambda a, b: a > b, ['Number', 'Number', 'Bool'])
lessAtom = OperationAtom('<', lambda a, b: a < b, ['Number', 'Number', 'Bool'])
orAtom = OperationAtom('or', lambda a, b: a or b, ['Bool', 'Bool', 'Bool'])
andAtom = OperationAtom('and', lambda a, b: a and b, ['Bool', 'Bool', 'Bool'])
notAtom = OperationAtom('not', lambda a: not a, ['Bool', 'Bool'])

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


def import_to(metta):
    metta.add_atom(r"\+", addAtom)
    metta.add_atom(r"-", subAtom)
    metta.add_atom(r"\*", mulAtom)
    metta.add_atom(r"/", divAtom)
    metta.add_atom(r"%", modAtom)
    metta.add_atom(r"==", equalAtom)
    metta.add_atom(r"<", lessAtom)
    metta.add_atom(r">", greaterAtom)
    metta.add_atom(r"or", orAtom)
    metta.add_atom(r"and", andAtom)
    metta.add_atom(r"not", notAtom)
    metta.add_token(r"\d+(\.\d+)",
                    lambda token: ValueAtom(float(token), 'Number'))
    metta.add_token(r"\d+",
                    lambda token: ValueAtom(int(token), 'Number'))
    metta.add_token("\"[^\"]*\"",
                    lambda token: ValueAtom(str(token[1:-1]), 'String'))
    metta.add_token(r"True|False",
                    lambda token: ValueAtom(token == 'True', 'Bool'))
    metta.add_token(r"call:[^\s]+", newCallAtom)
