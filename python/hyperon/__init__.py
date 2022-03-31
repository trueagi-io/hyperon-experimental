import hyperonpy as hp

from hyperonpy import AtomKind, init_logger

class Atom:

    def __init__(self, catom):
        self.catom = catom

    def __del__(self):
        #import sys; sys.stderr.write("Atom._del_(" + str(self) + ")\n"); sys.stderr.flush()
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
        if type == AtomKind.SYMBOL:
            return SymbolAtom(catom)
        elif type == AtomKind.VARIABLE:
            return VariableAtom(catom)
        elif type == AtomKind.EXPR:
            return ExpressionAtom(catom)
        elif type == AtomKind.GROUNDED:
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

    def get_children(self):
        return [Atom._from_catom(catom) for catom in
                hp.atom_get_children(self.catom)]

def E(*args):
    return ExpressionAtom(hp.atom_expr([atom.catom for atom in args]))

class GroundedAtom(Atom):

    def __init__(self, catom):
        super().__init__(catom)

    def get_object(self):
        return hp.atom_get_object(self.catom)

def G(object):
    return GroundedAtom(hp.atom_gnd(object))

def call_execute_on_grounded_atom(gnd, args):
    args = [Atom._from_catom(catom) for catom in args]
    return gnd.execute(*args)



# =========================================
class ConstGroundedObject:

    def copy(self):
        return self

class TypedObject(ConstGroundedObject):

    def __init__(self, atype):
        super().__init__()
        self.atype = atype

class TypedValue(TypedObject):

    def __init__(self, value, type_name):
        #super().__init__(S(type_name))
        super().__init__(type_name)
        self.value = value

    def __eq__(self, other):
        # TODO: ?typecheck
        if isinstance(other, TypedValue):
            return self.value == other.value
        return False

    def __repr__(self):
        return str(self.value)

class TypedOperation(TypedObject):

    def __init__(self, name, op, type_names, unwrap=True):
        # TODO: if we want to have arbitrary expressions as types
        super().__init__(type_names)
        self.name = name
        self.op = op
        self.unwrap = unwrap

    def execute(self, *args):
        # type-check?
        if self.unwrap:
            args = [arg.get_object().value for arg in args]
            return [G(TypedValue(self.op(*args), self.atype[-1]))]
        else:
            return self.op(*args)

    def __eq__(self, other):
        # TODO: instance
        return isinstance(other, TypedOperation) and self.name == other.name

    def __repr__(self):
        return self.name

def OperationAtom(name, op, type_names=['?'], unwrap=True):
    return G(TypedOperation(name, op, type_names, unwrap))

def ValueAtom(value, type_name='?'):
    return G(TypedValue(value, type_name))

class GroundingSpace:

    def __init__(self, repr_name=None):
        self.cspace = hp.grounding_space_new()
        # This name is used only for __repr__ now
        self.repr_name = repr_name

    def __del__(self):
        hp.grounding_space_free(self.cspace)

    def __eq__(self, other):
        return (isinstance(other, GroundingSpace) and
                hp.grounding_space_eq(self.cspace, other.cspace))

    def __repr__(self):
        return super().__repr__() if self.repr_name is None else self.repr_name

    def add_atom(self, atom):
        hp.grounding_space_add(self.cspace, atom.catom)

    def remove_atom(self, atom):
        return hp.grounding_space_remove(self.cspace, atom.catom)

    def replace_atom(self, atom, replacement):
        return hp.grounding_space_replace(self.cspace, atom.catom, replacement.catom)

    def get_atoms(self):
        return [Atom._from_catom(hp.grounding_space_get(self.cspace, i))
                for i in range(0, hp.grounding_space_len(self.cspace))]

    def query(self, pattern):
        return hp.grounding_space_query(self.cspace, pattern.catom)

    def subst(self, pattern, templ):
        return [Atom._from_catom(catom) for catom in
                hp.grounding_space_subst(self.cspace, pattern.catom,
                                         templ.catom)]

class Tokenizer:

    def __init__(self):
        self.ctokenizer = hp.tokenizer_new()

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

class AtomType:

    _UNDEFINED = None

    @staticmethod
    def undefined():
        if AtomType._UNDEFINED is None:
            AtomType._UNDEFINED = AtomType(hp.CAtomType.UNDEFINED)
        return AtomType._UNDEFINED

    @staticmethod
    def specific(atom):
        return AtomType(hp.atom_type_specific(atom.catom))

    def __init__(self, ctype):
        self.ctype = ctype

    def __del__(self):
        hp.atom_type_free(self.ctype)

    # def __eq__(self, other):
        # return (isinstance(other, AtomType) and
                # hp.atom_type_eq(self.ctype, other.ctype))

    # def __repr__(self):
        # return hp.atom_type_to_str(self.ctype)

    # def get_type(self):
        # return hp.atom_type_get_type(self.ctype)

def check_type(gnd_space, atom, type):
    return hp.check_type(gnd_space.cspace, atom.catom, type.ctype)

def validate_atom(gnd_space, expr):
    return hp.validate_atom(gnd_space.cspace, expr.catom)
