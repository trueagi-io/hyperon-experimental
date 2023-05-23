import hyperonpy as hp

from .atoms import Atom

class GroundingSpace:

    def __init__(self, cspace = None):
        if cspace is None:
            self.cspace = hp.space_new_grounding()
        else:
            self.cspace = cspace

    @staticmethod
    def _from_cspace(cspace):
        return GroundingSpace(cspace)

    def __del__(self):
        hp.space_free(self.cspace)

    def __eq__(self, other):
        return (isinstance(other, GroundingSpace) and
                hp.space_eq(self.cspace, other.cspace))

    def add_atom(self, atom):
        hp.space_add(self.cspace, atom.catom)

    def remove_atom(self, atom):
        return hp.space_remove(self.cspace, atom.catom)

    def replace_atom(self, atom, replacement):
        return hp.space_replace(self.cspace, atom.catom, replacement.catom)

    def get_atoms(self):
        res = hp.space_list(self.cspace)
        result = []
        for r in res:
            result.append(Atom._from_catom(r))
        return result

    def query(self, pattern):
        result = hp.space_query(self.cspace, pattern.catom)
        return [{k: Atom._from_catom(v) for k, v in bindings.items()} for bindings in result]

    def subst(self, pattern, templ):
        return [Atom._from_catom(catom) for catom in
                hp.space_subst(self.cspace, pattern.catom,
                                         templ.catom)]

class Tokenizer:

    def __init__(self, ctokenizer = None):
        if ctokenizer is None:
            self.ctokenizer = hp.tokenizer_new()
        else:
            self.ctokenizer = ctokenizer

    @staticmethod
    def _from_ctokenizer(ctokenizer):
        return Tokenizer(ctokenizer)

    def __del__(self):
        hp.tokenizer_free(self.ctokenizer)

    def register_token(self, regex, constr):
        hp.tokenizer_register_token(self.ctokenizer, regex, constr)

class SExprParser:

    def __init__(self, text):
        self.cparser = hp.CSExprParser(text)

    def parse(self, tokenizer):
        catom = self.cparser.parse(tokenizer.ctokenizer)
        return Atom._from_catom(catom) if catom is not None else None

class Interpreter:

    def __init__(self, gnd_space, expr):
        self.step_result = hp.interpret_init(gnd_space.cspace, expr.catom)

    def has_next(self):
        return hp.step_has_next(self.step_result)

    def next(self):
        if not self.has_next():
            raise StopIteration()
        self.step_result = hp.interpret_step(self.step_result)

    def get_result(self):
        if self.has_next():
            raise RuntimeError("Plan execution is not finished")
        return hp.step_get_result(self.step_result)

    def get_step_result(self):
        return self.step_result


def interpret(gnd_space, expr):
    interpreter = Interpreter(gnd_space, expr)
    while interpreter.has_next():
        interpreter.next()
    return [Atom._from_catom(catom) for catom in interpreter.get_result()]

def check_type(gnd_space, atom, type):
    return hp.check_type(gnd_space.cspace, atom.catom, type.catom)

def validate_atom(gnd_space, atom):
    return hp.validate_atom(gnd_space.cspace, atom.catom)

def get_atom_types(gnd_space, atom):
    result = hp.get_atom_types(gnd_space.cspace, atom.catom)
    return [Atom._from_catom(catom) for catom in result]
