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

    UNDEFINED_TYPE = S("Undefined")

    def __init__(self, catom):
        super().__init__(catom)

    def get_object(self):
        return hp.atom_get_object(self.catom)

    def get_grounded_type(self):
        return Atom._from_catom(hp.atom_get_grounded_type(self.catom))

def G(object, type=GroundedAtom.UNDEFINED_TYPE):
    return GroundedAtom(hp.atom_gnd(object, type.catom))

def call_execute_on_grounded_atom(gnd, typ, args):
    # ... if hp.atom_to_str(typ) == GroundedAtom.UNDEFINED_TYPE
    res_typ = GroundedAtom.UNDEFINED_TYPE if hp.atom_get_type(typ) != AtomKind.EXPR \
        else Atom._from_catom(hp.atom_get_children(typ)[-1])
    args = [Atom._from_catom(catom) for catom in args]
    return gnd.execute(*args, res_typ=res_typ)

def atoms_are_equivalent(first, second):
    return hp.atoms_are_equivalent(first.catom, second.catom)

class ConstGroundedObject:

    def copy(self):
        return self

class ValueObject(ConstGroundedObject):

    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        # TODO: ?typecheck
        if isinstance(other, ValueObject):
            return self.value == other.value
        return False

    def __repr__(self):
        return str(self.value)

class OperationObject(ConstGroundedObject):

    def __init__(self, name, op, unwrap=True):
        self.name = name
        self.op = op
        self.unwrap = unwrap

    def execute(self, *args, res_typ=GroundedAtom.UNDEFINED_TYPE):
        # type-check?
        if self.unwrap:
            args = [arg.get_object().value for arg in args]
            return [G(ValueObject(self.op(*args)), res_typ)]
        else:
            return self.op(*args)

    def __eq__(self, other):
        # TODO: instance
        return isinstance(other, OperationObject) and self.name == other.name

    def __repr__(self):
        return self.name

def OperationAtom(name, op, type_names=None, unwrap=True):
    # TODO: nested arrows
    if type_names is not None:
        typ = E(S("->"), *[S(n) for n in type_names])
    else:
        typ = S("Undefined")
    return G(OperationObject(name, op, unwrap), typ)

def ValueAtom(value, type_name='Undefined'):
    return G(ValueObject(value), S(type_name))

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
        result = hp.grounding_space_query(self.cspace, pattern.catom)
        return [{k: Atom._from_catom(v) for k, v in bindings.items()} for bindings in result]

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
