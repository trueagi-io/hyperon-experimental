from hyperon import *

def match_op(space, pattern, templ_op):
    space = space.get_object().value
    # TODO: hack to make both quoted and unquoted expression work
    if (templ_op.get_type() == AtomKind.EXPR and
        templ_op.get_children()[0].get_type() == AtomKind.SYMBOL and
        templ_op.get_children()[0].get_name() == 'q'):
        quoted = templ_op.get_children()[1:]
        templ = E(*quoted)
    else:
        templ = templ_op
    return space.subst(pattern, templ)

def let_op(pattern, atom, templ):
    space = GroundingSpace()
    space.add_atom(atom)
    return space.subst(pattern, templ)

def call_atom_op(atom, method_str, *args):
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

def print_op(atom):
    print(atom)
    return []

#E(S('->'), S('Number'), S('Number'), S('Number'))
subAtom = OperationAtom('-', lambda a, b: a - b, ['Number', 'Number', 'Number'])
mulAtom = OperationAtom('*', lambda a, b: a * b, ['Number', 'Number', 'Number'])
addAtom = OperationAtom('+', lambda a, b: a + b, ['Number', 'Number', 'Number'])
divAtom = OperationAtom('/', lambda a, b: a / b, ['Number', 'Number', 'Number'])
equalAtom = OperationAtom('==', lambda a, b: a == b, ['Number', 'Number', 'Bool'])
greaterAtom = OperationAtom('>', lambda a, b: a > b, ['Number', 'Number', 'Bool'])
lessAtom = OperationAtom('<', lambda a, b: a < b, ['Number', 'Number', 'Bool'])
orAtom = OperationAtom('or', lambda a, b: a or b, ['Bool', 'Bool', 'Bool'])
andAtom = OperationAtom('and', lambda a, b: a and b, ['Bool', 'Bool', 'Bool'])
notAtom = OperationAtom('not', lambda a: not a, ['Bool', 'Bool'])

# Any number of arguments for `nop` (including zero) due to *args
nopAtom = OperationAtom('nop', lambda *args: [], unwrap=False)

# TODO: Any for the argument is necessary to make argument reductable.
letAtom = OperationAtom('let', let_op,
    type_names=["Variable", "Any", "Atom", "Atom"], unwrap=False)
# FIXME: Need to introduce common Undefined type symbol. For now types.rs
# uses %Undefined%, and Undefined is used in Python code
matchAtom = OperationAtom('match', match_op,
    type_names=["Space", "Any", "Atom", "Atom"], unwrap=False)

printAtom = OperationAtom('println!', print_op, ['?', 'IO'], unwrap=False)

def newCallAtom(token):
    # REM: we could use "call" as a plain symbol (insted of "call:...")
    #      with the method name as the parameter of call_atom_op
    #      (but this parameter should be unwrapped)
    # "call:..." is an interesting example of families of tokens for ops, though
    return OperationAtom(
                token,
                lambda obj, *args: call_atom_op(obj, token[5:], *args),
                unwrap=False)

def SpaceAtom(grounding_space, repr_name=None):
    # Overriding grounding_space.repr_name here
    # It will be changed in all occurences of this Space
    if repr_name is not None:
        grounding_space.repr_name = repr_name
    return ValueAtom(grounding_space, 'Space')


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
    # * `run` will create another MeTTa object,
    #   which will resolve `&self` as `space`, all
    #   other syntax modification will not be inherited,
    #   so the file should not know that it is imported,
    #   but it will not be able to use parent's tokens
    # * tokens introduced in the file, will be resolved
    #   during its processing, and will be lost after it,
    #   so we cannot import syntax this way - only spaces
    # (another operation is needed for importing syntax)
    return MeTTa(space).import_file(fname.get_object().value)

def newImportOp(metta):
    # unwrap=False, because space name can remain
    # an unresolved symbol atom
    return OperationAtom(
        'import!',
        lambda s, f: import_op(metta, s, f),
        unwrap=False)

def pragma_op(metta, key, *args):
    # TODO: add support for Grounded values when needed
    metta.settings[key.get_name()] = \
        args[0].get_name() if len(args) == 1 else \
        [arg.get_name() for arg in args]
    return []

def newPragmaOp(metta):
    return OperationAtom(
        'pragma!',
        lambda key, *args: pragma_op(metta, key, *args),
        unwrap=False)


class MeTTa:

    def __init__(self, space=None):
        self.space = GroundingSpace("&self") if space is None else space
        self.tokenizer = Tokenizer()
        self._tokenizer()

    def _tokenizer(self):
        self.add_atom(r"\+", addAtom)
        self.add_atom(r"-", subAtom)
        self.add_atom(r"\*", mulAtom)
        self.add_atom(r"/", divAtom)
        self.add_atom(r"==", equalAtom)
        self.add_atom(r"<", lessAtom)
        self.add_atom(r">", greaterAtom)
        self.add_atom(r"or", orAtom)
        self.add_atom(r"and", andAtom)
        self.add_atom(r"not", notAtom)
        self.add_token(r"\d+(\.\d+)",
                       lambda token: ValueAtom(float(token), 'Number'))
        self.add_token(r"\d+",
                       lambda token: ValueAtom(int(token), 'Number'))
        #self.add_token(r"'[^']*'",
        #               lambda token: ValueAtom(str(token[1:-1]), 'String'))
        self.add_token("\"[^\"]*\"",
                       lambda token: ValueAtom(str(token[1:-1]), 'String'))
        self.add_token(r"True|False",
                       lambda token: ValueAtom(token == 'True', 'Bool'))
        self.add_atom(r"match", matchAtom)
        self.add_token(r"call:[^\s]+", newCallAtom)
        self.add_atom(r"let", letAtom)
        self.add_atom(r"nop", nopAtom)
        self.add_atom(r"println!", printAtom)
        self.add_atom(r"&self", SpaceAtom(self.space))
        self.add_atom(r"import!", newImportOp(self))
        self.add_atom(r"pragma!", newPragmaOp(self))

    def add_token(self, regexp, constr):
        self.tokenizer.register_token(regexp, constr)

    def add_atom(self, name, symbol):
        self.add_token(name, lambda _: symbol)

    def _parse_all(self, program):
        parser = SExprParser(program)
        while True:
            atom = parser.parse(self.tokenizer)
            if atom is None:
                break
            yield atom

    def parse_all(self, program):
        return list(self._parse_all(program))

    def parse_single(self, program):
        return next(self._parse_all(program))

    def add_parse(self, program):
        for atom in self._parse_all(program):
            self.space.add_atom(atom)

    def interpret(self, program):
        target = self.parse_single(program)
        return interpret(self.space, target)

    def import_file(self, fname):
        f = open(fname, "r")
        program = f.read()
        f.close()
        return self.run(program)

    def run(self, program):
        self.settings = {'type-check': None}
        status = "normal"
        result = []
        for expr in self._parse_all(program):
            if expr == S('!'):
                status = "interp"
                continue
            if expr.get_type() == AtomKind.SYMBOL and expr.get_name()[0] == ';':
                status = "comment"
                continue
            if status != "comment":
                if self.settings['type-check'] == 'auto':
                    if not validate_atom(self.space, expr):
                        print("Type error in ", expr)
                        break
                if status == "interp":
                    r = interpret(self.space, expr)
                    if r != []: result += [r]
                else:
                    self.space.add_atom(expr)
            status = "normal"
        return result
