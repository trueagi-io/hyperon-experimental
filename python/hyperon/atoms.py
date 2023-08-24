"""
The Python wrapper for Hyperon Atom Rust types
"""

import hyperonpy as hp
from hyperonpy import AtomKind
from typing import Union

class Atom:
    """Represents an Atom of any type"""

    def __init__(self, catom):
        """Initialize an atom"""
        self.catom = catom

    def __del__(self):
        """Frees an atom and all associated resources."""
        #import sys; sys.stderr.write("Atom._del_(" + str(self) + ")\n"); sys.stderr.flush()
        hp.atom_free(self.catom)


    def __eq__(self, other):
        """Checks if two atom objects represent the same conceptual atom. """
        return (isinstance(other, Atom) and
                hp.atom_eq(self.catom, other.catom))

    def __repr__(self):
        """Renders a human-readable text description of an atom. """
        return hp.atom_to_str(self.catom)

    def get_type(self):
        """Gets the type of this Atom"""
        return hp.atom_get_type(self.catom)

    def iterate(self):
        """Performs a depth-first exhaustive iteration of an atom and all its children recursively."""
        res = hp.atom_iterate(self.catom)
        result = []
        for r in res:
            result.append(Atom._from_catom(r))
        return result

    def match_atom(self, b):
        """Matches one atom with another, establishing bindings between them."""
        return BindingsSet(hp.atom_match_atom(self.catom, b.catom))

    @staticmethod
    def _from_catom(catom):
        """Constructs an Atom from C Atom of the same type"""
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
    """Symbol Atom represents a single concepts which is identified by name. Two symbols
    which have the same name reference the same concept."""

    def __init__(self, catom):
        """Initialize a symbol atom"""
        super().__init__(catom)

    def get_name(self):
        """Renders the name of an atom into a text buffer."""
        return hp.atom_get_name(self.catom)

def S(name):
    return SymbolAtom(hp.atom_sym(name))

class VariableAtom(Atom):
    """
    Variable Atom represents a variable like a variable in the pattern.
    """

    def __init__(self, catom):
        """Initialize a variable atom"""
        super().__init__(catom)

    def get_name(self):
        """Renders the name of an atom into a text buffer."""
        return hp.atom_get_name(self.catom)

def V(name):
    return VariableAtom(hp.atom_var(name))

class ExpressionAtom(Atom):
    """Expression Atom combines other kinds of atoms including expressions themselves."""

    def __init__(self, catom):
        """Initialize an expression atom"""
        super().__init__(catom)

    def get_children(self):
        """Gets all of the children Atoms"""
        return [Atom._from_catom(catom) for catom in hp.atom_get_children(self.catom)]


def E(*args):
    return ExpressionAtom(hp.atom_expr([atom.catom for atom in args]))

class AtomType:
    """Defines all the types of Atom"""

    UNDEFINED = Atom._from_catom(hp.CAtomType.UNDEFINED)
    TYPE = Atom._from_catom(hp.CAtomType.TYPE)
    ATOM = Atom._from_catom(hp.CAtomType.ATOM)
    SYMBOL = Atom._from_catom(hp.CAtomType.SYMBOL)
    VARIABLE = Atom._from_catom(hp.CAtomType.VARIABLE)
    EXPRESSION = Atom._from_catom(hp.CAtomType.EXPRESSION)
    GROUNDED = Atom._from_catom(hp.CAtomType.GROUNDED)
    GROUNDED_SPACE = Atom._from_catom(hp.CAtomType.GROUNDED_SPACE)

class GroundedAtom(Atom):
    """Grounded Atom represents sub-symbolic knowledge. On the API level it allows
    keeping data and behaviour inside an atom. There are three aspects of the grounded
    atom which can be customized:

        - the type of grounded atom is provided by the atom itself;
        - matching algorithm of the atom can be modified by the user;
        - atom can be made executable; such atom can be used to apply some sub-symbolic
          operations to other atoms as arguments.
    """

    def __init__(self, catom):
        """Initialize a grounded atom"""
        super().__init__(catom)

    def get_object(self):
        """Gets the Grounded Atom object or the space wrapped inside a Grounded Atom"""
        from .base import SpaceRef
        if self.get_grounded_type() == AtomType.GROUNDED_SPACE:
            return SpaceRef._from_cspace(hp.atom_get_space(self.catom))
        else:
            return hp.atom_get_object(self.catom)

    def get_grounded_type(self):
        """Retrieve the grounded type of a Grounded Atom."""
        return Atom._from_catom(hp.atom_get_grounded_type(self.catom))

def G(object, type=AtomType.UNDEFINED):
    assert hasattr(object, "copy"), "Method copy should be implemented by grounded object"
    return GroundedAtom(hp.atom_gnd(object, type.catom))

def _priv_call_execute_on_grounded_atom(gnd, typ, args):
    """
    Private glue for Hyperonpy implementation
    """
    # ... if hp.atom_to_str(typ) == AtomType.UNDEFINED
    res_typ = AtomType.UNDEFINED if hp.atom_get_type(typ) != AtomKind.EXPR \
        else Atom._from_catom(hp.atom_get_children(typ)[-1])
    args = [Atom._from_catom(catom) for catom in args]
    return gnd.execute(*args, res_typ=res_typ)

def _priv_call_match_on_grounded_atom(gnd, catom):
    """
    Private glue for Hyperonpy implementation
    """
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
    """Interface for working with atom matching and variable-to-atom binding."""

    def __init__(self, bindings: Union[hp.CBindings, None] = None):
        """Initialize bindings"""
        if bindings is None:
            self.cbindings = hp.bindings_new()
        else:
            self.cbindings = bindings

    def __del__(self):
        """Frees a bindings"""
        if self.cbindings is not None:
            hp.bindings_free(self.cbindings)

    def __eq__(self, other):
        """Checks if two bindings objects contain identical associations."""
        return (isinstance(other, Bindings) and
                hp.bindings_eq(self.cbindings, other.cbindings))

    def __repr__(self):
        """Renders a text description of the bindings"""
        return hp.bindings_to_str(self.cbindings)

    def __deepcopy__(self, memodict={}):
        """Makes a "deep copy" of the bindings"""
        return self.clone()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.cbindings is not None:
            hp.bindings_free(self.cbindings)
            self.cbindings = None

    def clone(self):
        """Makes a "deep copy" of the bindings"""
        return Bindings(hp.bindings_clone(self.cbindings))

    def merge(self, other: 'Bindings') -> 'BindingsSet':
        """Merges two Bindings frames together into a Bindings Set."""
        return BindingsSet(hp.bindings_merge(self.cbindings, other.cbindings))

    def add_var_binding(self, var: Union[str, Atom], atom: Atom) -> bool:
        """Adds a new variable <-> atom association within a bindings"""
        if isinstance(var, Atom):
            return hp.bindings_add_var_binding(self.cbindings, var.get_name(), atom.catom)
        else:
            return hp.bindings_add_var_binding(self.cbindings, var, atom.catom)

    def is_empty(self) -> bool:
        """Checks if a bindings contains no associations. """
        return hp.bindings_is_empty(self.cbindings)

    def narrow_vars(self, vars ):
        """Removes all variable associations from a bindings except those in the
        supplied list."""
        cvars = hp.CVecAtom = hp.atom_vec_new()
        for var in vars:
            hp.atom_vec_push(cvars, var.catom)
        hp.bindings_narrow_vars(self.cbindings, cvars)
        hp.atom_vec_free(cvars)

    def resolve(self, var_name: str) -> Union[Atom, None]:
        """Returns the atom bound to the supplied variable name in the bindings"""
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
    """Represents a set of Bindings frames. Potentially expressing all possible
    matches produced by a match operarion. """

    def __init__(self, input: Union[hp.CBindingsSet, Bindings, None] = None):
        """Initialize Bindings set"""
        self.shadow_list = None # A lazily initialized list that shadows the BindingsSet values for indexed access
        if input is None:
            self.c_set = hp.bindings_set_single()
        elif isinstance(input, Bindings):
            self.c_set = hp.bindings_set_from_bindings(input.cbindings)
        else:
            self.c_set = input

    def __del__(self):
        """Frees a Frees a Bindings set"""
        if self.c_set is not None:
            hp.bindings_set_free(self.c_set)
            self.c_set = None

    def __eq__(self, other):
        """Checks if two Bindings set objects contain identical associations."""
        return (isinstance(other, BindingsSet) and
                hp.bindings_set_eq(self.c_set, other.c_set))

    def __repr__(self):
        """Renders a text description of a Bindings set"""
        return hp.bindings_set_to_str(self.c_set)

    def __deepcopy__(self, memodict={}):
        """Makes a "deep copy" of a Bindings set"""
        return self.clone()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.c_set is not None:
            hp.bindings_set_free(self.c_set)
            self.c_set = None

    def __getitem__(self, key):
        if self.shadow_list is None:
            result = hp.bindings_set_unpack(self.c_set)
            self.shadow_list = [{k: Atom._from_catom(v) for k, v in bindings.items()} for bindings in result]
        return self.shadow_list[key]

    def empty():
        """Creates a new Bindings set without any Bindings frames.
        Conceptually this means no valid matches exist.
        """
        return BindingsSet(hp.bindings_set_empty())

    def clone(self):
        """Makes a "deep copy" of a Bindings set"""
        return BindingsSet(hp.bindings_set_clone(self.c_set))

    def is_empty(self) -> bool:
        """Checks if a Bindings set contains no Bindings frames, and thus indicates
        no match."""
        return hp.bindings_set_is_empty(self.c_set)

    def is_single(self) -> bool:
        """Checks if a Bindings set contains a frame with no associations, and is
        thus allows variables to take on any value.
        """
        return hp.bindings_set_is_single(self.c_set)

    def push(self, bindings: Bindings):
        """Adds a Bindings frame to an existing Bindings set

        Parameters
        ----------
        bindings:
            The Bindings set to incorporate into set. Ownership of this argument is
            taken by this function
        """
        self.shadow_list = None
        hp.bindings_set_push(self.c_set, bindings.cbindings)

    def add_var_binding(self, var: Union[str, Atom], value: Atom) -> bool:
        """Adds a new variable <-> atom association to every Bindings frame in a 
        Bindings set.
        """
        self.shadow_list = None
        if isinstance(var, Atom):
            return hp.bindings_set_add_var_binding(self.c_set, var.catom, value.catom)
        else:
            return hp.bindings_set_add_var_binding(self.c_set, V(var), value.catom)

    def add_var_equality(self, a: Atom, b: Atom) -> bool:
        """Asserts equality between two Variable atoms in a Bindings set."""
        self.shadow_list = None
        return hp.bindings_set_add_var_equality(self.c_set, a.catom, b.catom)

    def merge_into(self, input: Union['BindingsSet', Bindings]):
        """Merges the contents of one Bindings set into another Bindings set.  """
        self.shadow_list = None
        if isinstance(input, BindingsSet):
            hp.bindings_set_merge_into(self.c_set, input.c_set);
        else:
            new_set = BindingsSet(input);
            hp.bindings_set_merge_into(self.c_set, new_set.c_set);

    def iterator(self):
        """Gets Bindings set iterator"""
        res = hp.bindings_set_list(self.c_set)
        result = []
        for r in res:
            result.append(Bindings(r))
        return iter(result)
