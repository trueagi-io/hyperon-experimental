from hyperon import *

def interpret_until_result(target, kb):
    return interpret(kb, target)


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
    method(*args)
    return []

subAtom = G(TypedOperation('-', lambda a, b: a - b, ['Number', 'Number', 'Number']))
mulAtom = G(TypedOperation('*', lambda a, b: a * b, ['Number', 'Number', 'Number']))
addAtom = G(TypedOperation('+', lambda a, b: a + b, ['Number', 'Number', 'Number']))
divAtom = G(TypedOperation('/', lambda a, b: a / b, ['Number', 'Number', 'Number']))
equalAtom = G(TypedOperation('==', lambda a, b: a == b, ['Number', 'Number', 'Bool']))
greaterAtom = G(TypedOperation('>', lambda a, b: a > b, ['Number', 'Number', 'Bool']))
lessAtom = G(TypedOperation('<', lambda a, b: a < b, ['Number', 'Number', 'Bool']))
orAtom = G(TypedOperation('or', lambda a, b: a or b, ['Bool', 'Bool', 'Bool']))
andAtom = G(TypedOperation('and', lambda a, b: a and b, ['Bool', 'Bool', 'Bool']))
notAtom = G(TypedOperation('not', lambda a: not a, ['Bool', 'Bool']))

nopAtom = OperationAtom('nop', lambda _: [], unwrap=False)
commaAtom = OperationAtom(',', lambda args: args.get_children(), unwrap=False)

letAtom = OperationAtom('let', let_op, unwrap=False)
matchAtom = OperationAtom('match', match_op, unwrap=False)

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
    return G(TypedValue(grounding_space, 'Space'))

class Atomese:

    def __init__(self):
        self.tokens = {}

    def _tokenizer(self):
        tokenizer = Tokenizer()
        tokenizer.register_token(r"\+", lambda _: addAtom)
        tokenizer.register_token(r"-", lambda _: subAtom)
        tokenizer.register_token(r"\*", lambda _: mulAtom)
        tokenizer.register_token(r"/", lambda _: divAtom)
        tokenizer.register_token(r"==", lambda _: equalAtom)
        tokenizer.register_token(r"<", lambda _: lessAtom)
        tokenizer.register_token(r">", lambda _: greaterAtom)
        tokenizer.register_token(r"or", lambda _: orAtom)
        tokenizer.register_token(r"and", lambda _: andAtom)
        tokenizer.register_token(r"not", lambda _: notAtom)
        tokenizer.register_token(r"\d+(\.\d+)",
                                 lambda token: G(TypedValue(float(token), 'Number')))
        tokenizer.register_token(r"\d+",
                                 lambda token: G(TypedValue(int(token), 'Number')))
        tokenizer.register_token(r"'[^']*'",
                                 lambda token: G(TypedValue(str(token[1:-1]), 'String')))
        tokenizer.register_token(r"True|False",
                                 lambda token: G(TypedValue(token == 'True', 'Bool')))
        tokenizer.register_token(r"match", lambda _: matchAtom)
        tokenizer.register_token(r"call:[^\s]+", newCallAtom)
        tokenizer.register_token(r",", lambda _: commaAtom)
        tokenizer.register_token(r"let", lambda _: letAtom)
        tokenizer.register_token(r"nop", lambda _: nopAtom)
        for regexp in self.tokens.keys():
            tokenizer.register_token(regexp, self.tokens[regexp])
        return tokenizer

    def _parse_all(self, program):
        tokenizer = self._tokenizer()
        parser = SExprParser(program)
        while True:
            atom = parser.parse(tokenizer)
            if atom is None:
                break
            yield atom

    def parse_all(self, program):
        return list(self._parse_all(program))

    def parse_single(self, program):
        return next(self._parse_all(program))

    def parse(self, program, kb=None):
        if not kb:
            kb = GroundingSpace()
        for atom in self._parse_all(program):
            kb.add_atom(atom)
        return kb

    def add_token(self, regexp, constr):
        self.tokens[regexp] = constr

    def add_atom(self, name, symbol):
        self.add_token(name, lambda _: symbol)


class MeTTa(Atomese):

    def __init__(self, space=None):
        super().__init__()
        self.space = GroundingSpace("&self") if space is None else space
        self.add_atom(r"&self", SpaceAtom(self.space))

    def add_parse(self, program):
        return super().parse(program, self.space)

    def interpret(self, program):
        target = self.parse_single(program)
        return interpret(self.space, target)
