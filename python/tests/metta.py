import os
import sys

from hyperon import *
from common import MeTTa, SpaceAtom

def import_file(fname, space=None):
    f = open(fname, "r")
    program = f.read()
    f.close()
    return run_metta(program, space)

def import_op(metta, space, fname):
    # Check if space wasn't resolved
    if space.get_type() == AtomKind.SYMBOL:
        # Create new space
        name = space.get_name()
        space = GroundingSpace()
        # Register this space under name `name`
        metta.add_atom(name, SpaceAtom(space, name))
    else:
        space = space.get_object().value
    # A tricky part (FixMe or is this behavior indended?):
    # * `run_metta` will create another MeTTa object,
    #   which will resolve `&self` as `space`, all
    #   other syntax modification will not be inherited,
    #   so the file should not know that it is imported,
    #   but it will not be able to use parent's tokens
    # * tokens introduced in the file, will be resolved
    #   during its processing, and will be lost after it,
    #   so we cannot import syntax this way - only spaces
    # (another operation is needed for importing syntax)
    return import_file(fname.get_object().value, space)

def newImportOp(metta):
    # unwrap=False, because space name can remain
    # an unresolved symbol atom
    return OperationAtom(
        'import!',
        lambda s, f: import_op(metta, s, f),
        unwrap=False)

def pragma_op(metta, key, *args):
    # TODO: add support Grounded values when needed
    metta.settings[key.get_name()] = \
        args[0].get_name() if len(args) == 1 else \
        [arg.get_name() for arg in args]
    return []

def newPragmaOp(metta):
    return OperationAtom(
        'pragma!',
        lambda key, *args: pragma_op(metta, key, *args),
        unwrap=False)

def run_metta(program, space=None):
    metta = MeTTa(space)
    metta.settings = {'type-check': None}
    metta.add_token(r"import!", lambda _: newImportOp(metta))
    metta.add_token(r"pragma!", lambda _: newPragmaOp(metta))
    status = "normal"
    result = []
    for expr in metta._parse_all(program):
        if expr == S('!'):
            status = "interp"
            continue
        if expr.get_type() == AtomKind.SYMBOL and expr.get_name()[0] == ';':
            status = "comment"
            continue
        if status != "comment":
            if metta.settings['type-check'] == 'auto':
                if not validate_atom(metta.space, expr):
                    print("Type error in ", expr)
                    break
            if status == "interp":
                r = interpret(metta.space, expr)
                if r != []: result += [r]
            else:
                metta.space.add_atom(expr)
        status = "normal"
    return result

if __name__ == "__main__":
    os.system('clear')
    print("\n========= MeTTa version 0.0 =========\n\n")
    import_file(sys.argv[1])
