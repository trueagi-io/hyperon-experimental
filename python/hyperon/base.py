import hyperonpy as hp

from .atoms import Atom

class PySpace:

    def __init__(self, content, id):
        self.content = content
        self.id = id

    def __repr__(self):
        return self.id

def call_query_on_python_space(space, query_catom):
    query_atom = Atom._from_catom(query_catom)
    return space.query(query_atom)

def call_add_on_python_space(space, catom):
    atom = Atom._from_catom(catom)
    space.add(atom)

def call_remove_on_python_space(space, catom):
    atom = Atom._from_catom(catom)
    return space.remove(atom)

def call_replace_on_python_space(space, cfrom, cto):
    from_atom = Atom._from_catom(cfrom)
    to_atom = Atom._from_catom(cto)
    return space.replace(from_atom, to_atom)

def call_atom_count_on_python_space(space):
    if hasattr(space, "atom_count"):
        return space.atom_count()
    else:
        return -1

def call_new_iter_state_on_python_space(space):
    if hasattr(space, "atoms_iter"):
        return space.atoms_iter()
    else:
        return None

class Space:

    def __init__(self, space_obj):
        if type(space_obj) is hp.CSpace:
            self.cspace = space_obj
        else:
            self.cspace = hp.space_new_custom(space_obj)

    def __del__(self):
        hp.space_free(self.cspace)

    def __eq__(self, other):
        return hp.space_eq(self.cspace, other.cspace)

    @staticmethod
    def _from_cspace(cspace):
        return Space(cspace)

    def copy(self):
        return self

    def add_atom(self, atom):
        hp.space_add(self.cspace, atom.catom)

    def remove_atom(self, atom):
        return hp.space_remove(self.cspace, atom.catom)

    def replace_atom(self, atom, replacement):
        return hp.space_replace(self.cspace, atom.catom, replacement.catom)

    def atom_count(self):
        return hp.space_atom_count(self.cspace)

    def get_atoms(self):
        res = hp.space_list(self.cspace)
        if res == None:
            return None
        result = []
        for r in res:
            result.append(Atom._from_catom(r))
        return result

    def get_payload(self):
        return hp.space_get_payload(self.cspace)

    def query(self, pattern):
        result = hp.space_query(self.cspace, pattern.catom)
        return [{k: Atom._from_catom(v) for k, v in bindings.items()} for bindings in result]

    def subst(self, pattern, templ):
        return [Atom._from_catom(catom) for catom in
                hp.space_subst(self.cspace, pattern.catom,
                                         templ.catom)]

class GroundingSpace(Space):

    def __init__(self, cspace = None):
        if cspace is None:
            self.cspace = hp.space_new_grounding()
        else:
            self.cspace = cspace

    @staticmethod
    def _from_cspace(cspace):
        return GroundingSpace(cspace)

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
