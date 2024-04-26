import re
import sys
import os

from .atoms import ExpressionAtom, E, GroundedAtom, OperationAtom, ValueAtom, NoReduceError, AtomType, MatchableObject, \
    G, S, Atoms, get_string_value
from .base import Tokenizer, SExprParser
from .ext import register_atoms, register_tokens
import hyperonpy as hp

class Char:
    """Emulate Char type as in a traditional FPL"""
    def __init__(self, char):
        if len(char) != 1:
            raise ValueError("A Char object must be initialized with a single character.")
        self.char = char

    def __str__(self):
        return self.char

    def __repr__(self):
        return f"'{self.char}'"

    def __eq__(self, other):
        if isinstance(other, Char):
            return self.char == other.char
        return False

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
    greaterEqAtom = OperationAtom('>=', lambda a, b: a >= b, ['Number', 'Number', 'Bool'])
    lessEqAtom = OperationAtom('<=', lambda a, b: a <= b, ['Number', 'Number', 'Bool'])
    orAtom = OperationAtom('or', lambda a, b: a or b, ['Bool', 'Bool', 'Bool'])
    andAtom = OperationAtom('and', lambda a, b: a and b, ['Bool', 'Bool', 'Bool'])
    notAtom = OperationAtom('not', lambda a: not a, ['Bool', 'Bool'])
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

class RegexMatchableObject(MatchableObject):
    ''' To match atoms with regular expressions'''

    def __init__(self, content, id=None):
        super().__init__(content, id)

        self.content = self.content.replace("[[", "(").replace("]]", ")").replace("~", " ")

    def match_text(self, text, regexpr):
        return re.search(pattern=regexpr, string=text.strip(), flags=re.IGNORECASE)

    def match_(self, atom):
        pattern = self.content
        text = get_string_value(atom)
        text = ' '.join([x.strip() for x in text.split()])
        if pattern.startswith("regex:"):
            pattern = get_string_value(pattern[6:])
            matched = self.match_text(text, pattern)
            if matched is not None:
                return [{"matched_pattern": S(pattern)}]
        return []

@register_atoms(pass_metta=True)
def text_ops(run_context):
    """Add text operators

    repr: convert Atom to string.
    parse: convert String to Atom.
    stringToChars: convert String to tuple of Char.
    charsToString: convert tuple of Char to String.

    see test_stdlib.py for examples.

    """

    reprAtom = OperationAtom('repr', lambda a: [ValueAtom(repr(a))],
                             ['Atom', 'String'], unwrap=False)
    parseAtom = OperationAtom('parse', lambda s: [SExprParser(str(s)[1:-1]).parse(run_context.tokenizer())],
                              ['String', 'Atom'], unwrap=False)
    stringToCharsAtom = OperationAtom('stringToChars', lambda s: [E(*[ValueAtom(Char(c)) for c in str(s)[1:-1]])],
                                      ['String', 'Atom'], unwrap=False)
    charsToStringAtom = OperationAtom('charsToString', lambda a: [ValueAtom("".join([str(c)[1:-1] for c in a.get_children()]))],
                                      ['Atom', 'String'], unwrap=False)
    return {
        r"repr": reprAtom,
        r"parse": parseAtom,
        r"stringToChars": stringToCharsAtom,
        r"charsToString": charsToStringAtom
    }

@register_tokens
def type_tokens():
    return {
        r"[-+]?\d+" : lambda token: ValueAtom(int(token), 'Number'),
        r"[-+]?\d+\.\d+": lambda token: ValueAtom(float(token), 'Number'),
        r"[-+]?\d+(\.\d+)?[eE][-+]?\d+": lambda token: ValueAtom(float(token), 'Number'),
        "\"[^\"]*\"": lambda token: ValueAtom(str(token[1:-1]), 'String'),
        "\'[^\']\'": lambda token: ValueAtom(Char(token[1]), 'Char'),
        r"True|False": lambda token: ValueAtom(token == 'True', 'Bool'),
        r'regex:"[^"]*"': lambda token: G(RegexMatchableObject(token),  AtomType.UNDEFINED)
    }


def import_from_module(path, mod=None):
    ps = path.split(".")
    obj = mod
    if obj is None:
        import importlib
        # FIXME? Do we need this?
        current_directory = os.getcwd()
        appended = False
        if current_directory not in sys.path:
            sys.path.append(current_directory)
            appended = True
        for i in range(len(ps)):
            j = len(ps) - i
            try:
                obj = importlib.import_module('.'.join(ps[:j]))
                ps = ps[j:]
                break
            except:
                pass
        if appended:
            sys.path.remove(current_directory)
        assert obj is not None
    for p in ps:
        obj = getattr(obj, p)
    return obj

def find_py_obj(path, mod=None):
    try:
        obj = import_from_module(path, mod)
    except:
        # If path is not found, check if the object itself exists
        if hasattr(sys.modules.get('__main__'), path):
            return getattr(sys.modules['__main__'], path)
        # FIXME? This was introduced for something like (py-obj str) to work.
        # But this works as one-line Python eval. Do we need it here?
        local_scope = {}
        try:
            exec(f"__obj = {path}", {}, local_scope)
            obj = local_scope['__obj']
        except:
            raise RuntimeError(f'Failed to find "{path}"')
    return obj

def get_py_atom(path, typ=AtomType.UNDEFINED, mod=None):
    name = str(path.get_object().content if isinstance(path, GroundedAtom) else path)
    if mod is not None:
        if not isinstance(mod, GroundedAtom):
            raise NoReduceError()
        mod = mod.get_object().content
    obj = find_py_obj(name, mod)
    if callable(obj):
        return [OperationAtom(name, obj, typ, unwrap=True)]
    else:
        return [ValueAtom(obj, typ)]

def do_py_dot(mod, path, typ=AtomType.UNDEFINED):
    return get_py_atom(path, typ, mod)

@register_atoms
def py_obj_atoms():
    return {
        r"py-atom": OperationAtom("py-atom", get_py_atom, unwrap=False),
        r"py-dot": OperationAtom("py-dot", do_py_dot, unwrap=False),
    }


@register_atoms
def load_ascii():
    def load_ascii_atom(space, name):
        space_obj = space.get_object()
        hp.load_ascii(name.get_name(), space_obj.cspace)
        #['Space', 'Symbol', 'Unit'],
        return [Atoms.UNIT]

    loadAtom = OperationAtom('load-ascii', load_ascii_atom,
                             unwrap=False)
    return {
        r"load-ascii": loadAtom
    }
