import re
import sys
import os

from .atoms import ExpressionAtom, E, GroundedAtom, OperationAtom, ValueAtom, NoReduceError, AtomType, MatchableObject, \
    G, S, Atoms, get_string_value, GroundedObject, SymbolAtom
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
        r"(?s)^\".*\"$": lambda token: ValueAtom(str(token[1:-1]), 'String'),
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

def try_unwrap_python_object(a, is_symbol_to_str = False):
    if isinstance(a, GroundedAtom):
        # FIXME? Do we need to unwrap a grounded object if it is not GroundedObject?
        return a.get_object().content if isinstance(a.get_object(), GroundedObject) else a.get_object()
    if is_symbol_to_str and isinstance(a, SymbolAtom):
        return a.get_name()
    return a

# convert nested tuples to nested python tuples or lists
def _py_tuple_list(tuple_list, metta_tuple):
    rez = []
    for a in metta_tuple.get_children():
        if isinstance(a, ExpressionAtom):
            rez.append(_py_tuple_list(tuple_list, a))
        else:
            rez.append(try_unwrap_python_object(a))
    return tuple_list(rez)

def py_tuple(metta_tuple):
    return [ValueAtom(_py_tuple_list(tuple, metta_tuple))]

def py_list(metta_tuple):
    return [ValueAtom(_py_tuple_list(list, metta_tuple))]

def tuple_to_keyvalue(a):
    ac = a.get_children()
    if len(ac) != 2:
        raise Exception("Syntax error in tuple_to_keyvalue")
    return try_unwrap_python_object(ac[0], is_symbol_to_str = True), try_unwrap_python_object(ac[1])

# convert pair of tuples to python dictionary
def py_dict(metta_tuple):
    return [ValueAtom(dict([tuple_to_keyvalue(a) for a in metta_tuple.get_children()]))]

# chain python objects with |  (syntactic sugar for langchain)
def py_chain(metta_tuple):
    objects = [try_unwrap_python_object(a) for a in metta_tuple.get_children()]
    result = objects[0]
    for obj in objects[1:]:
        result = result | obj
    return [ValueAtom(result)]

@register_atoms()
def py_funs():
    return {"py-tuple": OperationAtom("py-tuple", py_tuple, unwrap = False),
            "py-list" : OperationAtom("py-list" , py_list , unwrap = False),
            "py-dict" : OperationAtom("py-dict" , py_dict , unwrap = False),
            "py-chain": OperationAtom("py-chain", py_chain, unwrap = False)}
