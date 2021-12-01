from hyperon import *

def interpret_until_result(target, kb):
    return interpret(kb, target)

def SpacesAtom(spaces):
    return G(SpaceCollection(spaces))

class SpaceCollection(ConstGroundedAtom):

    def __init__(self, spaces):
        super().__init__()
        self.spaces = spaces

    def execute(self, ops, data):
        name = data.pop().get_symbol()
        data.push(ValueAtom(self.spaces[name]))

    def __eq__(self, other):
        return isinstance(other, SpacesAtom) and self.spaces == other.spaces

    def __repr__(self):
        return "spaces"

class MatchAtom(OpGroundedAtom):

    def __init__(self):
        super().__init__()

    def execute(self, ops, data):
        space = data.pop().get_object().value
        pattern = data.pop()
        # TODO: hack to make both quoted and unquoted expression work
        templ_op = data.pop()
        if (templ_op.get_type() == AtomType.EXPR and
            templ_op.get_children()[0].get_type() == AtomType.SYMBOL and
            templ_op.get_children()[0].get_symbol() == 'q'):
            quoted = content[3].get_children()[1:]
            templ = E(*quoted)
        else:
            templ = templ_op
        for m in space.subst(pattern, templ):
            data.push(m)

    def __repr__(self):
        return "match"

class UnaryOpAtom(OpGroundedAtom):

    def __init__(self, name, op):
        super().__init__()
        self.name = name
        self.op = op

    def execute(self, ops, data):
        arg = data.pop().get_object()
        data.push(ValueAtom(self.op(arg.value)))

    def __eq__(self, other):
        return isinstance(other, UnaryOpAtom) and self.name == self.name

    def __repr__(self):
        return self.name

class BinaryOpAtom(OpGroundedAtom):

    def __init__(self, name, op):
        super().__init__()
        self.name = name
        self.op = op

    def execute(self, ops, data):
        a = data.pop().get_object()
        b = data.pop().get_object()
        data.push(ValueAtom(self.op(a.value, b.value)))

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

    def execute(self, ops, data):
        obj = data.pop().get_object().value
        args = data.pop().get_children()
        method = getattr(obj, self.method_name)
        method(*args)

    def __eq__(self, other):
        if isinstance(other, CallAtom):
            return self.method_name == other.method_name
        return False

    def __repr__(self):
        return "call:" + self.method_name

class CommaAtom(OpGroundedAtom):

    def __init__(self):
        super().__init__()

    def execute(self, ops, data):
        args = data.pop().get_children()
        for arg in args:
            data.push(arg)

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
        #parser.register_token(r"let", lambda token: IFMATCH)
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
