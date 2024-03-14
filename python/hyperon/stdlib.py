import re

from .atoms import ExpressionAtom, E, GroundedAtom, OperationAtom, ValueAtom, NoReduceError, AtomType, MatchableObject, \
    G, S, Atoms
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

def get_string_value(value) -> str:
    if not isinstance(value, str):
        value = repr(value)
    if len(value) > 2 and ("\"" == value[0]) and ("\"" == value[-1]):
        return value[1:-1]
    return value

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




@register_atoms
def text_ops():
    """Add text operators

    repr: convert Atom to string.
    parse: convert String to Atom.
    stringToChars: convert String to tuple of Char.
    charsToString: convert tuple of Char to String.

    see test_stdlib.py for examples.

    """

    reprAtom = OperationAtom('repr', lambda a: [ValueAtom(repr(a))],
                             ['Atom', 'String'], unwrap=False)
    parseAtom = OperationAtom('parse', lambda s: [ValueAtom(SExprParser(str(s)[1:-1]).parse(Tokenizer()))],
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
