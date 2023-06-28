import hyperonpy as hp

from .atoms import Atom, BindingsSet

"""
A virtual base class upon which Spaces can be implemented in Python
"""
class AbstractSpace:

    def __init__(self):
        return

    def query(self, query_atom):
        raise RuntimeError("Space::query() is not implemented")

    # TODO (INTERNAL): Currently unimplemented.  We may do this differently depending on lazy / comprehensions
    # so I don't want to write throw-away code unless it's needed downstream.
    # def subst(self, pattern, templ):
    #     None

    def add(self, atom):
        raise RuntimeError("Space::add() is not implemented")

    def remove(self, atom):
        raise RuntimeError("Space::remove() is not implemented")

    def replace(self, atom, replacement):
        raise RuntimeError("Space::replace() is not implemented")

    def atom_count(self):
        None

    def atoms_iter(self):
        None

"""
A wrapper over the native GroundingSpace implementation, that can be subclassed and extended within Python
"""
class GroundingSpace(AbstractSpace):

    def __init__(self, unwrap=True):
        super().__init__()
        # self.cspace = hp.space_new_grounding()
        self.gspace = GroundingSpaceRef()

    def query(self, query_atom):
        return self.gspace.query(query_atom)

    # TODO (INTERNAL): Currently unimplemented.
    # def subst(self, pattern, templ):

    def add(self, atom):
        self.gspace.add_atom(atom)

    def remove(self, atom):
        return self.gspace.remove_atom(atom)

    def replace(self, from_atom, to_atom):
        return self.gspace.replace_atom(from_atom, to_atom)

    def atom_count(self):
        return self.gspace.atom_count()

    def atoms_iter(self):
        return iter(self.gspace.get_atoms())

"""
Private glue for Hyperonpy implementation
"""
def _priv_call_query_on_python_space(space, query_catom):
    query_atom = Atom._from_catom(query_catom)
    return space.query(query_atom)

"""
Private glue for Hyperonpy implementation
"""
def _priv_call_add_on_python_space(space, catom):
    atom = Atom._from_catom(catom)
    space.add(atom)

"""
Private glue for Hyperonpy implementation
"""
def _priv_call_remove_on_python_space(space, catom):
    atom = Atom._from_catom(catom)
    return space.remove(atom)

"""
Private glue for Hyperonpy implementation
"""
def _priv_call_replace_on_python_space(space, cfrom, cto):
    from_atom = Atom._from_catom(cfrom)
    to_atom = Atom._from_catom(cto)
    return space.replace(from_atom, to_atom)

"""
Private glue for Hyperonpy implementation
"""
def _priv_call_atom_count_on_python_space(space):
    if hasattr(space, "atom_count"):
        count = space.atom_count()
        if count is not None:
            return count
        else:
            return -1
    else:
        return -1

"""
Private glue for Hyperonpy implementation
"""
def _priv_call_new_iter_state_on_python_space(space):
    if hasattr(space, "atoms_iter"):
        return space.atoms_iter()
    else:
        return None

"""
A reference to a Space, which may be accessed directly, wrapped in a grounded atom,
or passed to a MeTTa interpreter
"""
class SpaceRef:

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
        return SpaceRef(cspace)

    """
    Returns a new copy of the SpaceRef, referencing the same underlying Space
    """
    def copy(self):
        return self

    """
    Add an Atom to the Space
    """
    def add_atom(self, atom):
        hp.space_add(self.cspace, atom.catom)

    """
    Delete the specified atom from the Space
    """
    def remove_atom(self, atom):
        return hp.space_remove(self.cspace, atom.catom)

    """
    Replace the specified Atom, if it exists in the Space, with the supplied replacement Atom
    """
    def replace_atom(self, atom, replacement):
        return hp.space_replace(self.cspace, atom.catom, replacement.catom)

    """
    Returns the number of Atoms in the Space, or -1 if it cannot readily computed
    """
    def atom_count(self):
        return hp.space_atom_count(self.cspace)

    """
    Returns a list of all Atoms in the Space, or None if that is impossible
    """
    def get_atoms(self):
        res = hp.space_list(self.cspace)
        if res == None:
            return None
        result = []
        for r in res:
            result.append(Atom._from_catom(r))
        return result

    """
    Returns the Space object referenced by the SpaceRef, or None if the object does not have a
    direct Python interface
    """
    def get_payload(self):
        return hp.space_get_payload(self.cspace)

    """
    Performs the specified query on the Space, and returns the result BindingsSet
    """
    def query(self, pattern):
        result = hp.space_query(self.cspace, pattern.catom)
        return BindingsSet(result)

    """
    Performs a substitution within the Space
    """
    def subst(self, pattern, templ):
        return [Atom._from_catom(catom) for catom in
                hp.space_subst(self.cspace, pattern.catom,
                                         templ.catom)]

"""
A reference to a native GroundingSpace, implemented by the MeTTa core library
"""
class GroundingSpaceRef(SpaceRef):

    def __init__(self, cspace = None):
        if cspace is None:
            self.cspace = hp.space_new_grounding()
        else:
            self.cspace = cspace

    @staticmethod
    def _from_cspace(cspace):
        return GroundingSpaceRef(cspace)

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
