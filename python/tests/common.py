from hyperon import *

def interpret_until_result(target, kb):
    return interpret(kb, target)

def SpacesAtom(spaces):
    return G(SpaceCollection(spaces))

class SpaceCollection(ConstGroundedAtom):

    def __init__(self, spaces):
        super().__init__()
        self.spaces = spaces

    def execute(self, name):
        return [ValueAtom(self.spaces[name.get_symbol()])]

    def __eq__(self, other):
        return isinstance(other, SpacesAtom) and self.spaces == other.spaces

    def __repr__(self):
        return "spaces"

class MatchAtom(OpGroundedAtom):

    def __init__(self):
        super().__init__()

    def execute(self, space, pattern, templ_op):
        space = space.get_object().value
        # TODO: hack to make both quoted and unquoted expression work
        if (templ_op.get_type() == AtomType.EXPR and
            templ_op.get_children()[0].get_type() == AtomType.SYMBOL and
            templ_op.get_children()[0].get_symbol() == 'q'):
            quoted = content[3].get_children()[1:]
            templ = E(*quoted)
        else:
            templ = templ_op
        return space.subst(pattern, templ)

    def __repr__(self):
        return "match"

class UnaryOpAtom(OpGroundedAtom):

    def __init__(self, name, op):
        super().__init__()
        self.name = name
        self.op = op

    def execute(self, arg):
        return [ValueAtom(self.op(arg.get_object().value))]

    def __eq__(self, other):
        return isinstance(other, UnaryOpAtom) and self.name == self.name

    def __repr__(self):
        return self.name

class BinaryOpAtom(OpGroundedAtom):

    def __init__(self, name, op):
        super().__init__()
        self.name = name
        self.op = op

    def execute(self, a, b):
        return [ValueAtom(self.op(a.get_object().value,
            b.get_object().value))]

    def __eq__(self, other):
        return isinstance(other, BinaryOpAtom) and self.name == self.name

    def __repr__(self):
        return self.name

class SubAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__("-", lambda a, b: a - b)

class MulAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__("*", lambda a, b: a * b)

class AddAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__("+", lambda a, b: a + b)

class DivAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__("/", lambda a, b: a / b)

class EqualAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__("==", lambda a, b: a == b)

class GreaterAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__(">", lambda a, b: a > b)

class LessAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__("<", lambda a, b: a < b)

class OrAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__("or", lambda a, b: a or b)

class AndAtom(BinaryOpAtom):
    def __init__(self):
        super().__init__("and", lambda a, b: a and b)

class NotAtom(UnaryOpAtom):
    def __init__(self):
        super().__init__("not", lambda a: not a)

class CallAtom(OpGroundedAtom):

    def __init__(self, method_name):
        super().__init__()
        self.method_name = method_name

    def execute(self, obj, *args):
        obj = obj.get_object().value
        method = getattr(obj, self.method_name)
        method(*args)
        return []

    def __eq__(self, other):
        if isinstance(other, CallAtom):
            return self.method_name == other.method_name
        return False

    def __repr__(self):
        return "call:" + self.method_name

class LetAtom(OpGroundedAtom):

    def __init__(self):
        super().__init__()

    def execute(self, pattern, atom, templ):
        space = GroundingSpace()
        space.add_atom(atom)
        return space.subst(pattern, templ)

    def __repr__(self):
        return "let"

class CommaAtom(OpGroundedAtom):

    def __init__(self):
        super().__init__()

    def execute(self, args):
        return args.get_children()

    def __repr__(self):
        return ","

class AtomspaceAtom(Value):

    def __init__(self, value, name):
        super().__init__(value)
        self.name = name

    def __repr__(self):
        return self.name

class Atomese:

    def __init__(self):
        self.tokens = {}

    def _parser(self):
        parser = SExprSpace()
        parser.register_token(r"\+", lambda token: G(AddAtom()))
        parser.register_token(r"-", lambda token: G(SubAtom()))
        parser.register_token(r"\*", lambda token: G(MulAtom()))
        parser.register_token(r"/", lambda token: G(DivAtom()))
        parser.register_token(r"==", lambda token: G(EqualAtom()))
        parser.register_token(r"<", lambda token: G(LessAtom()))
        parser.register_token(r">", lambda token: G(GreaterAtom()))
        parser.register_token(r"or", lambda token: G(OrAtom()))
        parser.register_token(r"and", lambda token: G(AndAtom()))
        parser.register_token(r"not", lambda token: G(NotAtom()))
        parser.register_token(r"\d+(\.\d+)", lambda token: ValueAtom(float(token)))
        parser.register_token(r"\d+", lambda token: ValueAtom(int(token)))
        parser.register_token(r"'[^']*'", lambda token: ValueAtom(str(token[1:-1])))
        parser.register_token(r"True|False", lambda token: ValueAtom(token == 'True'))
        parser.register_token(r"match", lambda token: G(MatchAtom()))
        parser.register_token(r"call:[^\\s)]+", lambda token: G(CallAtom(token[5:])))
        parser.register_token(r",", lambda token: G(CommaAtom()))
        parser.register_token(r"let", lambda token: G(LetAtom()))
        for regexp in self.tokens.keys():
            parser.register_token(regexp, self.tokens[regexp])
        return parser

    def parse_single(self, program):
        kb = GroundingSpace()
        text = self._parser()
        text.add_string(program)
        text.add_to(kb)
        exprs = list(kb.get_atoms())
        if len(exprs) == 1:
            return exprs[0]
        else:
            return exprs

    def parse(self, program, kb=None):
        if not kb:
            kb = GroundingSpace()
        text = self._parser()
        text.add_string(program)
        text.add_to(kb)
        return kb

    def add_token(self, regexp, constr):
        self.tokens[regexp] = constr

    def add_atom(self, name, symbol):
        self.add_token(name, lambda _: symbol)


