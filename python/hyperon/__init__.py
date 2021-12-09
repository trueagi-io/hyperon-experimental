import hyperonpy as hp

from hyperonpy import AtomType, init_logger

class Atom:

    def __init__(self, catom):
        self.catom = catom

    def __del__(self):
        hp.atom_free(self.catom)

    def __eq__(self, other):
        return (isinstance(other, Atom) and
                hp.atom_eq(self.catom, other.catom))

    def __repr__(self):
        return hp.atom_to_str(self.catom)

    def get_type(self):
        return hp.atom_get_type(self.catom)

    @staticmethod
    def _from_catom(catom):
        type = hp.atom_get_type(catom)
        if type == AtomType.SYMBOL:
            return SymbolAtom(catom)
        elif type == AtomType.VARIABLE:
            return VariableAtom(catom)
        elif type == AtomType.EXPR:
            return ExpressionAtom(catom)
        elif type == AtomType.GROUNDED:
            return GroundedAtom(catom)
        else:
            raise Exception("Unexpected type of the atom: " + str(type))

class SymbolAtom(Atom):

    def __init__(self, catom):
        super().__init__(catom)

    def get_name(self):
        return hp.atom_get_name(self.catom)

def S(name):
    return SymbolAtom(hp.atom_sym(name))

class VariableAtom(Atom):

    def __init__(self, catom):
        super().__init__(catom)

    def get_name(self):
        return hp.atom_get_name(self.catom)

def V(name):
    return VariableAtom(hp.atom_var(name))

class ExpressionAtom(Atom):

    def __init__(self, catom):
        super().__init__(catom)

def E(*args):
    return ExpressionAtom(hp.atom_expr([atom.catom for atom in args]))

class GroundedAtom(Atom):

    def __init__(self, catom):
        super().__init__(catom)

    def get_object(self):
        return hp.atom_get_object(self.catom)

def G(object):
    return GroundedAtom(hp.atom_gnd(object))

class BaseVecAtom:

    def __init__(self, cvec):
        self.cvec = cvec

    def size(self):
        return hp.vec_atom_size(self.cvec)

    def is_empty(self):
        return self.size() == 0

    def push(self, atom):
        hp.vec_atom_push(self.cvec, atom.catom)

    def pop(self):
        return Atom._from_catom(hp.vec_atom_pop(self.cvec))

class VecAtom(BaseVecAtom):

    def __init__(self):
        super().__init__(hp.vec_atom_new())

    def __del__(self):
        hp.vec_atom_free(self.cvec)

class ConstGroundedAtom:

    def copy(self):
        return self

class OpGroundedAtom(ConstGroundedAtom):

    def __init__(self):
        super().__init__()

    def __eq__(self, other):
        return self.__class__ == other.__class__

def ValueAtom(value):
    return G(Value(value))

class Value(ConstGroundedAtom):

    def __init__(self, value):
        super().__init__()
        self.value = value

    def __eq__(self, other):
        if isinstance(other, Value):
            return self.value == other.value
        return False

    def __repr__(self):
        return str(self.value)

class GroundingSpace:

    def __init__(self):
        self.cspace = hp.grounding_space_new()

    def __del__(self):
        hp.grounding_space_free(self.cspace)

    def __eq__(self, other):
        return (isinstance(other, GroundingSpace) and
                hp.grounding_space_eq(self.cspace, other.cspace))

    def add_atom(self, atom):
        hp.grounding_space_add(self.cspace, atom.catom)

    def get_atoms(self):
        return [Atom._from_catom(hp.grounding_space_get(self.cspace, i))
                for i in range(0, hp.grounding_space_len(self.cspace))]

    def query(self, pattern):
        return hp.grounding_space_query(self.cspace, pattern.catom)

    def subst(self, pattern, templ):
        return [Atom._from_catom(catom) for catom in
                hp.grounding_space_subst(self.cspace, pattern.catom,
                                         templ.catom)]

class SExprSpace:

    def __init__(self):
        self.cspace = hp.sexpr_space_new()

    def __del__(self):
        hp.sexpr_space_free(self.cspace)

    def register_token(self, regex, constr):
        hp.sexpr_space_register_token(self.cspace, regex, constr)

    def add_string(self, text):
        hp.sexpr_space_add_str(self.cspace, text)

    def add_to(self, gspace):
        hp.sexpr_space_into_grounding_space(self.cspace, gspace.cspace)

def interpret(gnd_space, expr):
    return [Atom._from_catom(catom) for catom in
            hp.interpret(gnd_space.cspace, expr.catom)]
