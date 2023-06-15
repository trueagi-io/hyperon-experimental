import hyperonpy as hp
from hyperonpy import AtomKind
from typing import Union

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

    def iterate(self):
        res = hp.atom_iterate(self.catom)
        result = []
        for r in res:
            result.append(Atom._from_catom(r))
        return result

    def match_atom(self, b):
        return BindingsSet(hp.atom_match_atom(self.catom, b.catom))

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

class GroundedAtomType:
    SPACE = Atom._from_catom(hp.CAtomType.GROUNDED_SPACE)

class GroundedAtom(Atom):

    def __init__(self, catom):
        super().__init__(catom)

    def get_object(self):
        from .base import Space
        if self.get_grounded_type() == GroundedAtomType.SPACE:
            return Space._from_cspace(hp.atom_get_space(self.catom))
        else:
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
        # Overwrite Python default representation of a string to use
        # double quotes instead of single quotes.
        if isinstance(self.content, str):
            return f'"{self.content}"'

        # Use default representation for everything else
        return repr(self.content) if self.id is None else self.id

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
        return V(type_names[1:]) if type_names[0] == '$' else S(type_names)
    return type_names

def OperationAtom(name, op, type_names=None, unwrap=True):
    return G(OperationObject(name, op, unwrap), _type_sugar(type_names))

def ValueAtom(value, type_name=None, atom_id=None):
    return G(ValueObject(value, atom_id), _type_sugar(type_name))

def MatchableAtom(value, type_name=None, atom_id=None):
    return G(MatchableObject(value, atom_id), _type_sugar(type_name))


class Bindings:

    def __init__(self, bindings: Union[hp.CBindings, None] = None):
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

    def merge_v2(left, right) -> 'BindingsSet':
        return BindingsSet(hp.bindings_merge_v2(left.cbindings, right.cbindings))

    def add_var_bindings(self, var: Union[str, Atom], atom: Atom) -> bool:
        if isinstance(var, Atom):
            return hp.bindings_add_var_bindings(self.cbindings, var.get_name(), atom.catom)
        else:
            return hp.bindings_add_var_bindings(self.cbindings, var, atom.catom)

    def is_empty(self) -> bool:
        return hp.bindings_is_empty(self.cbindings)

    def narrow_vars(self, vars ):
        cvars = hp.CVecAtom = hp.vec_atom_new()
        for var in vars:
            hp.vec_atom_push(cvars, var.catom)
        hp.bindings_narrow_vars(self.cbindings, cvars)
        hp.vec_atom_free(cvars)

    def resolve(self, var_name: str) -> Union[Atom, None]:
        raw_atom = hp.bindings_resolve(self.cbindings, var_name)
        return None if raw_atom is None else Atom._from_catom(raw_atom)

    def resolve_and_remove(self, var_name: str) -> Union[Atom, None]:
        raw_atom = hp.bindings_resolve_and_remove(self.cbindings, var_name)
        return None if raw_atom is None else Atom._from_catom(raw_atom)

    def iterator(self):
        res = hp.bindings_list(self.cbindings)
        result = []
        for r in res:
            result.append((r[0], Atom._from_catom(r[1])))

        return iter(result)

class BindingsSet:

    def __init__(self, input: Union[hp.CBindingsSet, Bindings, None] = None):
        if input is None:
            self.c_set = hp.bindings_set_single()
        elif isinstance(input, Bindings):
            self.c_set = hp.bindings_set_from_bindings(input.cbindings)
        else:
            self.c_set = input

    def __del__(self):
        if self.c_set is not None:
            hp.bindings_set_free(self.c_set)
            self.c_set = None

    def __eq__(self, other):
        return (isinstance(other, BindingsSet) and
                hp.bindings_set_eq(self.c_set, other.c_set))

    def __repr__(self):
        return hp.bindings_set_to_str(self.c_set)

    def __deepcopy__(self, memodict={}):
        return self.clone()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.c_set is not None:
            hp.bindings_set_free(self.c_set)
            self.c_set = None

    def empty():
        return BindingsSet(hp.bindings_set_empty())

    def clone(self):
        return BindingsSet(hp.bindings_set_clone(self.c_set))

    def is_empty(self) -> bool:
        return hp.bindings_set_is_empty(self.c_set)

    def is_single(self) -> bool:
        return hp.bindings_set_is_single(self.c_set)

    def push(self, bindings: Bindings):
        hp.bindings_set_push(self.c_set, bindings.cbindings)

    def add_var_binding(self, var: Atom, value: Atom) -> bool:
        return hp.bindings_set_add_var_binding(self.c_set, var.catom, value.catom)

    def add_var_equality(self, a: Atom, b: Atom) -> bool:
        return hp.bindings_set_add_var_equality(self.c_set, a.catom, b.catom)

    def merge_into(self, input: Union['BindingsSet', Bindings]):
        if isinstance(input, BindingsSet):
            hp.bindings_set_merge_into(self.c_set, input.c_set);
        else:
            new_set = BindingsSet(input);
            hp.bindings_set_merge_into(self.c_set, new_set.c_set);

    def iterator(self):
        res = hp.bindings_set_list(self.c_set)
        result = []
        for r in res:
            result.append(Bindings(r))
        return iter(result)
