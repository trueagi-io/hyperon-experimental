import hyperonpy as hp
from hyperonpy import AtomKind


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

class AtomType:

    UNDEFINED = Atom._from_catom(hp.CAtomType.UNDEFINED)
    TYPE = Atom._from_catom(hp.CAtomType.TYPE)
    ATOM = Atom._from_catom(hp.CAtomType.ATOM)
    SYMBOL = Atom._from_catom(hp.CAtomType.SYMBOL)
    VARIABLE = Atom._from_catom(hp.CAtomType.VARIABLE)
    EXPRESSION = Atom._from_catom(hp.CAtomType.EXPRESSION)
    GROUNDED = Atom._from_catom(hp.CAtomType.GROUNDED)

class GroundedAtom(Atom):

    def __init__(self, catom):
        super().__init__(catom)

    def get_object(self):
        return hp.atom_get_object(self.catom)

    def get_grounded_type(self):
        return Atom._from_catom(hp.atom_get_grounded_type(self.catom))

def G(object, type=AtomType.UNDEFINED):
    assert hasattr(object, "copy"), "Method copy should be implemented by grounded object"
    return GroundedAtom(hp.atom_gnd(object, type.catom))

def call_execute_on_grounded_atom(gnd, typ, args):
    # ... if hp.atom_to_str(typ) == AtomType.UNDEFINED
    res_typ = AtomType.UNDEFINED if hp.atom_get_type(typ) != AtomKind.EXPR \
        else Atom._from_catom(hp.atom_get_children(typ)[-1])
    args = [Atom._from_catom(catom) for catom in args]
    return gnd.execute(*args, res_typ=res_typ)

def call_match_on_grounded_atom(gnd, catom):
    return gnd.match_(Atom._from_catom(catom))

def atoms_are_equivalent(first, second):
    return hp.atoms_are_equivalent(first.catom, second.catom)

class GroundedObject:

    def __init__(self, content, id=None):
        self.content = content
        self.id = id

    def __repr__(self):
        return str(self.content) if self.id is None else self.id

    def copy(self):
        return self

class ValueObject(GroundedObject):

    @property
    def value(self):
        return self.content

    def __eq__(self, other):
        # TODO: ?typecheck
        return isinstance(other, ValueObject) and self.content == other.content

class NoReduceError(Exception):
    pass

class OperationObject(GroundedObject):

    def __init__(self, name, op, unwrap=True):
        super().__init__(op, name)
        self.unwrap = unwrap

    @property
    def op(self):
        return self.content

    @property
    def name(self):
        return self.id

    def execute(self, *args, res_typ=AtomType.UNDEFINED):
        # type-check?
        if self.unwrap:
            for arg in args:
                if not isinstance(arg, GroundedAtom):
                    # REM:
                    # Currently, applying grounded operations to pure atoms is not reduced.
                    # If we want, we can raise an exception, or to form a error expression instead,
                    # so a MeTTa program can catch and analyze it.
                    # raise RuntimeError("Grounded operation " + self.name + " with unwrap=True expects only grounded arguments")
                    raise NoReduceError()
            args = [arg.get_object().content for arg in args]
            return [G(ValueObject(self.op(*args)), res_typ)]
        else:
            result = self.op(*args)
            if not isinstance(result, list):
                raise RuntimeError("Grounded operation `" + self.name + "` should return list")
            return result

    def __eq__(self, other):
        return isinstance(other, OperationObject) and self.name == other.name

class MatchableObject(ValueObject):
    def match_(self, atom):
        raise RuntimeError("MatchableObject::match_() is not implemented")

def _type_sugar(type_names):
    if type_names is None:
        return AtomType.UNDEFINED
    if isinstance(type_names, list):
        return E(S("->"), *[_type_sugar(n) for n in type_names])
    if isinstance(type_names, str):
        return V(type_names) if type_names[0] == '$' else S(type_names)
    return type_names

def OperationAtom(name, op, type_names=None, unwrap=True):
    return G(OperationObject(name, op, unwrap), _type_sugar(type_names))

def ValueAtom(value, type_name=None, atom_id=None):
    return G(ValueObject(value, atom_id), _type_sugar(type_name))

def MatchableAtom(value, type_name=None, atom_id=None):
    return G(MatchableObject(value, atom_id), _type_sugar(type_name))


class Bindings:

    def __init__(self, bindings: hp.CBindings | None = None):
        if bindings is None:
            self.cbindings = hp.bindings_new()
        else:
            self.cbindings = bindings

    def __del__(self):
        if self.cbindings is not None:
            hp.bindings_free(self.cbindings)

    def __eq__(self, other):
        return (isinstance(other, Bindings) and
                hp.bindings_eq(self.cbindings, other.cbindings))

    def __repr__(self):
        return hp.bindings_to_str(self.cbindings)

    def __deepcopy__(self, memodict={}):
        return self.clone()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.cbindings is not None:
            hp.bindings_free(self.cbindings)
            self.cbindings = None

    def clone(self):
        return Bindings(hp.bindings_clone(self.cbindings))

    @staticmethod
    def merge(left, right):
        return Bindings(hp.bindings_merge(left.cbindings, right.cbindings))

    def add_var_bindings(self, var_name:str, atom: Atom) -> bool:
        return hp.bindings_add_var_bindings(self.cbindings, var_name, atom.catom)

    def is_empty(self) -> bool:
        return hp.bindings_is_empty(self.cbindings)

    def resolve(self, var_name: str) -> Atom | None:
        raw_atom = hp.bindings_resolve(self.cbindings, var_name)
        return None if raw_atom is None else Atom(raw_atom)

    def resolve_and_remove(self, var_name: str) -> Atom | None:
        raw_atom = hp.bindings_resolve_and_remove(self.cbindings, var_name)
        return None if raw_atom is None else Atom(raw_atom)

    def iterator(self):
        res = hp.bindings_list(self.cbindings)
        result = []
        for r in res:
            result.append((r[0], Atom(r[1])))

        return iter(result)

