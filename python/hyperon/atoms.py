"""
The Python wrapper for Hyperon Atom Rust types
"""

import hyperonpy as hp
from hyperonpy import AtomKind, SerialResult, Serializer
from typing import Union
from hyperon.conversion import ConvertingSerializer

class Atom:
    """Represents an Atom of any type"""

    def __init__(self, catom):
        """Initialize an Atom"""
        self.catom = catom

    def __del__(self):
        """Frees an Atom and all associated resources."""
        hp.atom_free(self.catom)

    def __eq__(self, other):
        """Checks if two atom objects represent the same conceptual Atom."""
        return (isinstance(other, Atom) and
                hp.atom_eq(self.catom, other.catom))

    def __repr__(self):
        """Renders a human-readable text description of the Atom."""
        return hp.atom_to_str(self.catom)

    def get_metatype(self):
        """Gets the metatype (kind) of the current Atom instance"""
        return hp.atom_get_metatype(self.catom)

    def iterate(self):
        """Performs a depth-first exhaustive iteration of an Atom and all its children recursively."""
        res = hp.atom_iterate(self.catom)
        result = []
        for r in res:
            result.append(Atom._from_catom(r))
        return result

    def match_atom(self, b):
        """Matches one Atom with another, establishing bindings between them."""
        return BindingsSet(hp.atom_match_atom(self.catom, b.catom))

    @staticmethod
    def _from_catom(catom):
        """Constructs an Atom by wrapping a C Atom"""
        type = hp.atom_get_metatype(catom)
        if type == AtomKind.SYMBOL:
            return SymbolAtom(catom)
        elif type == AtomKind.VARIABLE:
            return VariableAtom(catom)
        elif type == AtomKind.EXPR:
            return ExpressionAtom(catom)
        elif type == AtomKind.GROUNDED:
            return GroundedAtom(catom)
        else:
            raise Exception("Unexpected type of the Atom: " + str(type))

class SymbolAtom(Atom):
    """A SymbolAtom represents a single concept, identified by name. If two symbols
    have the same name, they reference the same concept."""

    def __init__(self, catom):
        """Initialize a SymbolAtom"""
        super().__init__(catom)

    def get_name(self):
        """Returns the name of the Atom."""
        return hp.atom_get_name(self.catom)

def S(name):
    """A convenient method to construct a SymbolAtom"""
    return SymbolAtom(hp.atom_sym(name))

class VariableAtom(Atom):
    """A VariableAtom represents a variable in an expression. It serves as a
    placeholder that can be matched with, or bound to other Atoms."""

    def __init__(self, catom):
        """Initialize a VariableAtom"""
        super().__init__(catom)

    def get_name(self):
        """Returns the name of the Atom."""
        return hp.atom_get_name(self.catom)

    @staticmethod
    def parse_name(name):
        """Construct new VariableAtom instance from VariableAtom.get_name()
        method results."""
        return VariableAtom(hp.atom_var_parse_name(name))

def V(name):
    """A convenient method to construct a VariableAtom"""
    return VariableAtom(hp.atom_var(name))

class ExpressionAtom(Atom):
    """An ExpressionAtom combines different kinds of Atoms, including expressions."""

    def __init__(self, catom):
        """Initialize an expression atom"""
        super().__init__(catom)

    def get_children(self):
        """Returns all children Atoms of an expression"""
        return [Atom._from_catom(catom) for catom in hp.atom_get_children(self.catom)]


def E(*args):
    """A convenient method to construct an ExpressionAtom"""
    return ExpressionAtom(hp.atom_expr([atom.catom for atom in args]))

class AtomType:
    """Defines all Atom types"""

    UNDEFINED = Atom._from_catom(hp.CAtomType.UNDEFINED)
    TYPE = Atom._from_catom(hp.CAtomType.TYPE)
    ATOM = Atom._from_catom(hp.CAtomType.ATOM)
    SYMBOL = Atom._from_catom(hp.CAtomType.SYMBOL)
    VARIABLE = Atom._from_catom(hp.CAtomType.VARIABLE)
    EXPRESSION = Atom._from_catom(hp.CAtomType.EXPRESSION)
    GROUNDED = Atom._from_catom(hp.CAtomType.GROUNDED)
    GROUNDED_SPACE = Atom._from_catom(hp.CAtomType.GROUNDED_SPACE)
    UNIT = Atom._from_catom(hp.CAtomType.UNIT)
    NUMBER = Atom._from_catom(hp.CAtomType.NUMBER)
    BOOL = Atom._from_catom(hp.CAtomType.BOOL)

class Atoms:

    EMPTY = Atom._from_catom(hp.CAtoms.EMPTY)
    UNIT = Atom._from_catom(hp.CAtoms.UNIT)
    METTA = Atom._from_catom(hp.CAtoms.METTA)

class GroundedAtom(Atom):
    """
    A GroundedAtom represents sub-symbolic knowledge. At the API level, it allows
    keeping data and behaviour inside an Atom. There are three aspects of a GroundedAtom
    which can be customized:

        - the type of GroundedAtom is provided by the Atom itself;
        - the matching algorithm used by the Atom;
        - an Atom can be made executable, and used to apply sub-symbolic
          operations to other Atoms as arguments.
    """

    def __init__(self, catom):
        """Initialize a GroundedAtom"""
        super().__init__(catom)

    def get_object(self):
        """Returns the GroundedAtom object, or the Space wrapped inside a GroundedAtom,
           or convert supported Rust grounded objects into corresponding ValueObjects
        """
        # TODO: Here code assumes CGrounded object is always Python object.
        # This is not true in general case. To make code universal we need to
        # keep kind of the original runtime in CGrounded structure.
        if hp.atom_is_cgrounded(self.catom):
            return hp.atom_get_object(self.catom)
        else:
            return _priv_gnd_get_object(self)

    def get_grounded_type(self):
        """Retrieve the grounded type of the GroundedAtom."""
        return Atom._from_catom(hp.atom_get_grounded_type(self.catom))

def _priv_gnd_get_object(atom):
    """
    Tries to convert grounded object into a one of the standard Python values.
    This implementation is used to automatically convert values from other
    runtimes to Python.
    """
    typ = atom.get_grounded_type()
    # TODO: GroundedSpace is a special case right now, but it could be
    # implemented using serializer as well
    if typ == AtomType.GROUNDED_SPACE:
        from .base import SpaceRef
        return SpaceRef._from_cspace(hp.atom_get_space(atom.catom))
    elif typ == S('Bool') or typ == S('Number'):
        converter = ConvertingSerializer()
        try:
            res = hp.atom_gnd_serialize(atom.catom, converter)
        except Exception as e:
            raise RuntimeError(f"Could not convert atom {atom} to Python value, exception caught: {e}")
        if res != SerialResult.OK or converter.value is None:
            raise RuntimeError(f"Could not convert atom {atom} to Python value")
        else:
            return ValueObject(converter.value)
    else:
        raise TypeError(f"Cannot get Python object of unsupported non-C atom {atom}")


def G(object, type=AtomType.UNDEFINED):
    """A convenient method to construct a GroundedAtom"""
    return GroundedAtom(_priv_atom_gnd(object, type))

def _priv_atom_gnd(obj, type):
    """
    Converts Python object into grounded atom. It has special processing for
    the object which has cspace attribute and for ValueObject instances of primitive
    types. Spaces usually should be treated by a special way. Primitive atoms
    are converted into the MeTTa primitives.
    """
    catom = None
    if hasattr(obj, "cspace"):
        assert type == AtomType.UNDEFINED, f"Grounded Space Atoms {obj} can't have a custom type {type}"
        catom = hp.atom_space(obj.cspace)
    elif isinstance(obj, ValueObject):
        value = obj.value
        if isinstance(value, bool):
            assert type == AtomType.BOOL or type == AtomType.UNDEFINED, f"Grounded bool {obj} can't have a custom type {type}"
            catom = hp.atom_bool(value)
        elif isinstance(value, int):
            assert type == AtomType.NUMBER or type == AtomType.UNDEFINED, f"Grounded int {obj} can't have a custom type {type}"
            catom = hp.atom_int(value)
        elif isinstance(value, float):
            assert type == AtomType.NUMBER or type == AtomType.UNDEFINED, f"Grounded float {obj} can't have a custom type {type}"
            catom = hp.atom_float(value)
    if catom is None:
        assert hasattr(obj, "copy"), f"Method copy should be implemented by grounded object {obj}"
        catom = hp.atom_py(obj, type.catom)
    return catom

def _priv_call_execute_on_grounded_atom(gnd, typ, args):
    """
    Private glue for Hyperonpy implementation.
    Executes grounded Atoms.
    """
    # ... if hp.atom_to_str(typ) == AtomType.UNDEFINED
    res_typ = AtomType.UNDEFINED if hp.atom_get_metatype(typ) != AtomKind.EXPR \
        else Atom._from_catom(hp.atom_get_children(typ)[-1])
    args = [Atom._from_catom(catom) for catom in args]
    return gnd.execute(*args, res_typ=res_typ)

def _priv_call_match_on_grounded_atom(gnd, catom):
    """
    Private glue for Hyperonpy implementation.
    Matches grounded atoms
    """
    return gnd.match_(Atom._from_catom(catom))

def _priv_call_serialize_on_grounded_atom(gnd, serializer):
    """
    Private glue for Hyperonpy implementation.
    Serializes grounded atoms
    """
    return gnd.serialize(serializer)

def _priv_compare_value_atom(gnd, catom):
    """
    Private glue for Hyperonpy implementation.
    Tests for equality between a grounded value atom and another atom
    """
    if hp.atom_get_metatype(catom) == AtomKind.GROUNDED:
        atom = GroundedAtom(catom)
        try:
            return gnd == atom.get_object()
        except TypeError:
            return False
    else:
        return False

def atoms_are_equivalent(first, second):
    """Check if two atoms are equivalent"""
    return hp.atoms_are_equivalent(first.catom, second.catom)

class GroundedObject:
    """A GroundedObject holds some content and, optionally, an identifier."""

    def __init__(self, content, id=None):
        """Initializes a new GroundedObject with the given content and identifier."""
        self.content = content
        self.id = id

    def __repr__(self):
        """Returns the object's ID if present, or a string representation of
        its content if not."""
        # Overwrite Python default representation of a string to use
        # double quotes instead of single quotes.
        if isinstance(self.content, str):
            newstr = repr(self.content)[1:-1].translate(str.maketrans({'"' : r'\"'}))
            return f'"{newstr}"'

        # Use default representation for everything else
        return repr(self.content) if self.id is None else self.id

    def copy(self):
        """
        Returns a copy of this GroundedObject instance.

        Note: Currently, this method returns the original instance.
        """
        return self

class ValueObject(GroundedObject):
    """
    A ValueObject is a specialized form of GroundedObject, which treats its content
    as a value. It allows for equality comparison between the content of two ValueObjects.

    Example:
        obj1 = ValueObject(5)
        obj2 = ValueObject(5)
        obj3 = ValueObject(6)

        print(obj1 == obj2)  # True
        print(obj1 == obj3)  # False
    """

    @property
    def value(self):
        """Gets the value of the object, which is its content."""
        return self.content

    def __eq__(self, other):
        """Compares the equality of this ValueObject with another based on their content."""
        # TODO: We need to hook this up the the Value-Bridging mechanism when it's designed and built
        # https://github.com/trueagi-io/hyperon-experimental/issues/351

        # TODO: ?typecheck for the contents
        return isinstance(other, ValueObject) and self.content == other.content

    def serialize(self, serializer):
        """
        Serialize standard Python values. This implementation is used to
        pass Python values into the foreign runtime.
        """
        if isinstance(self.content, bool):
            return serializer.serialize_bool(self.content)
        elif isinstance(self.content, int):
            return serializer.serialize_int(self.content)
        elif isinstance(self.content, float):
            return serializer.serialize_float(self.content)
        else:
            return SerialResult.NOT_SUPPORTED

class NoReduceError(Exception):
    """Custom exception; raised when a reduction operation cannot be performed."""
    pass

class IncorrectArgumentError(Exception):
    """
    Argument is not recognized by function implementation. It can be
    argument of incorrect type or in incorrect format. Interpreter handles
    this error similarly to the situation when pure function definition
    is not matched.
    """
    pass

class MettaError(Exception):
    """Custom exception; raised when a error should be returned from OperationAtom,
       , but we don't want to output Python error stack."""
    pass

def unwrap_args(atoms):
    args = []
    kwargs = {}
    for a in atoms:
        if isinstance(a, ExpressionAtom):
            ch = a.get_children()
            if len(ch) > 0 and repr(ch[0]) == "Kwargs":
                for c in ch[1:]:
                    try:
                        kwarg = c.get_children()
                        assert len(kwarg) == 2
                    except:
                        raise RuntimeError(f"Incorrect kwarg format {kwarg}")
                    try:
                        kwargs[get_string_value(kwarg[0])] = kwarg[1].get_object().content
                    except:
                        raise NoReduceError()
                continue
        if hasattr(a, 'get_object'):
            args.append(a.get_object().content)
        else:
            # NOTE:
            # Currently, applying grounded operations to pure atoms is not reduced.
            # If we want, we can raise an exception, or form an error expression instead,
            # so a MeTTa program can catch and analyze it.
            # raise RuntimeError("Grounded operation " + self.name + " with unwrap=True expects only grounded arguments")
            raise NoReduceError()
    return args, kwargs

class OperationObject(GroundedObject):
    """
    An OperationObject represents an operation as a grounded object, allowing for more
    advanced logic like lazy evaluation, type-checking, and more.

    Inherits:
        GroundedObject: The parent class that provides the basic wrapper around content.

    Attributes:
        unwrap (bool): Determines whether to unwrap the content of GroundedAtoms
                       when passed as arguments to the operation.

    Properties:
        op: Returns the operation function.
        name: Returns the identifier name for this operation object.

    Methods:
        __init__(name, op, unwrap): Initializes an OperationObject instance.
        execute(*args, res_typ): Executes the operation with the provided arguments.
        __eq__(other): Compares the equality of this OperationObject instance with another.

    Example:
        def add(a, b):
            return a + b

        op_obj = OperationObject("addition", add)
        result = op_obj.execute(3, 4)
    """

    def __init__(self, name, op, unwrap=True):
        """
        Initializes a new OperationObject with a name identifier, operation function,
        and an optional unwrap flag.
        Parameters:
            name (str): The identifier for this operation.
            op (function): The function representing the operation.
            unwrap (bool, optional): Whether to unwrap GroundedAtom content when applying
                                     the operation. Defaults to True.

        """
        super().__init__(op, name)
        self.unwrap = unwrap

    @property
    def op(self):
        """Returns the operation function."""
        return self.content

    @property
    def name(self):
        """Returns the identifier name for this operation object."""
        return self.id

    def execute(self, *atoms, res_typ=AtomType.UNDEFINED):
        """
        Executes the operation with the provided arguments.

        Parameters:
            *args: Arguments to pass to the operation function.
            res_typ (AtomType, optional): The expected result type. Defaults to AtomType.UNDEFINED.

        Returns:
            The result of the operation.

        Raises:
            NoReduceError: Raised when `unwrap=True` and a non-GroundedAtom argument is provided.
            RuntimeError: Raised when the result of the operation is not a list.

        Note:
            Depending on the `unwrap` attribute, this method will either unwrap GroundedAtoms
            before passing them to the operation or pass them as is.
        """
        # type-check?
        if self.unwrap:
            args, kwargs = unwrap_args(atoms)
            try:
                result = self.op(*args, **kwargs)
            except MettaError as e:
                return [E(S('Error'), *e.args)]
            if result is None:
                return [Atoms.UNIT]
            if callable(result):
                return [OperationAtom(repr(result), result, unwrap=True)]
            return [ValueAtom(result, res_typ)]
        else:
            result = self.op(*atoms)
            try:
                iter(result)
            except TypeError:
                raise RuntimeError("Grounded operation `" + self.name + "` should return list")
            return result

    def __eq__(self, other):
        """
        Compares the equality of this OperationObject with another based on their names.

        Parameters:
            other (OperationObject): Another OperationObject instance to compare.

        Returns:
            True if both OperationObjects have the same name; False otherwise.
        """
        return isinstance(other, OperationObject) and self.name == other.name

class MatchableObject(ValueObject):
    """
    Represents an object that can be involved in a matching operation with an Atom.

    This class is meant to be subclassed by objects that define specific matching behavior
    with an Atom. It provides a stub method for the matching operation that raises
    a RuntimeError when called, which must be overridden by subclasses.

    Inherits:
        ValueObject: The parent class that provides basic value-based equality and representation.

    Methods:
        match_(atom): A stub method for matching the object with an Atom.

    Example:
        class MyMatchableObject(MatchableObject):
            def match_(self, atom):
                # Implement the matching logic here
                pass

        my_obj = MyMatchableObject("some_value")
        my_obj.match_(some_atom)  # Should not raise RuntimeError

    Raises:
        RuntimeError: Raised when the match_ method is called without being overridden by a subclass.
    """

    def match_(self, atom):
        """
        A stub method for matching the object with an Atom.

        This method is intended to be overridden by subclasses to provide specific
        matching behavior with an Atom.

        Parameters:
            atom (Atom): An Atom object to match against.

        Raises:
            RuntimeError: Raised when this method is called without being overridden in a subclass.
        """
        raise RuntimeError("MatchableObject::match_() is not implemented")

def _type_sugar(type_names):
    """
    Transforms a variety of type representations into a unified Atom-based format.

    This utility function is intended for internal use to handle different ways in which
    type information can be provided. It converts `type_names` into a form that can be
    readily used for type checking or other internal operations.

    Parameters:
        type_names (Union[None, list, str, AtomType]): The type information to be converted.
            - If None, will return AtomType.UNDEFINED.
            - If list, will recursively transform each element.
            - If str, will return a Variable Atom (`V`) if the string starts with '$'; otherwise, returns a Symbol Atom (`S`).
            - If already an AtomType, returns it as is.

    Returns:
        AtomType: The transformed type information in AtomType format.

    Examples:
        _type_sugar(None)                 => AtomType.UNDEFINED
        _type_sugar(["int", "str"])       => E(S("->"), S("int"), S("str"))
        _type_sugar("$var")               => V("var")
        _type_sugar("int")                => S("int")
        _type_sugar(AtomType.SOME_TYPE)   => AtomType.SOME_TYPE
    """
    if type_names is None:
        return AtomType.UNDEFINED
    if isinstance(type_names, list):
        return E(S("->"), *[_type_sugar(n) for n in type_names])
    if isinstance(type_names, str):
        return V(type_names[1:]) if type_names[0] == '$' else S(type_names)
    return type_names

def OperationAtom(name, op, type_names=None, unwrap=True):
    """
    An OperationAtom wraps an operation with optional type information into a GroundedAtom
    and associates a name with it. Useful for registering custom operations
    that can be executed in an Atom-based computational environment.
    """
    return G(OperationObject(name, op, unwrap), _type_sugar(type_names))

def ValueAtom(value, type_name=None, atom_id=None):
    """
    Creates a GroundedAtom that wraps a given value, optionally specifying its
    type and identifier. It has special processing for the objects which have
    cspace attribute and for ValueObject instances of primitive types. Spaces
    usually should be treated by a special way. Primitive atoms are converted
    into the MeTTa primitives.
    """
    return G(ValueObject(value, atom_id), _type_sugar(type_name))

def PrimitiveAtom(value, type_name=None, atom_id=None):
    """
    Creates a GroundedAtom that wraps a given Python primitive value without
    converting it into the MeTTa primitive. By default ValueAtom function
    converts Python primitives into MeTTa ones. This function is added to
    override this rule if needed.
    """
    PRIMITIVE_TYPES = (int, float, bool)
    assert isinstance(value, PRIMITIVE_TYPES), f"Primitive value {PRIMITIVE_TYPES} is expected"
    type = _type_sugar(type_name)
    return GroundedAtom(hp.atom_py(ValueObject(value, atom_id), type.catom))

def MatchableAtom(value, type_name=None, atom_id=None):
    """
    Creates a Grounded Atom that wraps a matchable value, optionally specifying its type and identifier.
    """
    return G(MatchableObject(value, atom_id), _type_sugar(type_name))


class Bindings:
    """Interface for working with atom matching and variable-to-atom binding."""

    def __init__(self, bindings: Union[hp.CBindings, None] = None):
        """Initializes with or without pre-existing bindings."""
        if bindings is None:
            self.cbindings = hp.bindings_new()
        else:
            self.cbindings = bindings

    def __del__(self):
        """Frees the binding resources."""
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
        """Makes a "deep copy" of the bindings."""
        return self.clone()

    def __enter__(self):
        """For context management."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Frees resources on exit."""
        if self.cbindings is not None:
            hp.bindings_free(self.cbindings)
            self.cbindings = None

    def clone(self):
        """Makes a "deep copy" of the bindings"""
        return Bindings(hp.bindings_clone(self.cbindings))

    def merge(self, other: 'Bindings') -> 'BindingsSet':
        """Merges with another Bindings instance, into a Bindings Set."""
        return BindingsSet(hp.bindings_merge(self.cbindings, other.cbindings))

    def add_var_binding(self, var: VariableAtom, atom: Atom) -> bool:
        """Adds a binding between a variable and an Atom."""
        return hp.bindings_add_var_binding(self.cbindings, var.catom, atom.catom)

    def is_empty(self) -> bool:
        """Checks if a bindings contains no associations."""
        return hp.bindings_is_empty(self.cbindings)

    def narrow_vars(self, vars ):
        """Keeps only specific variable associations."""
        cvars = hp.CVecAtom = hp.atom_vec_new()
        for var in vars:
            hp.atom_vec_push(cvars, var.catom)
        hp.bindings_narrow_vars(self.cbindings, cvars)
        hp.atom_vec_free(cvars)

    def resolve(self, var: VariableAtom) -> Union[Atom, None]:
        """Finds the atom for a given variable"""
        raw_atom = hp.bindings_resolve(self.cbindings, var.catom)
        return None if raw_atom is None else Atom._from_catom(raw_atom)

    def iterator(self):
        """Returns an iterator over the variable-atom pairs in the bindings"""
        res = hp.bindings_list(self.cbindings)
        result = [(Atom._from_catom(r[0]), Atom._from_catom(r[1])) for r in res]
        return iter(result)

class BindingsSet:
    """Represents a set of Bindings frames, potentially expressing all possible
    matches produced by a match operation."""

    def __init__(self, input: Union[hp.CBindingsSet, Bindings, None] = None):
        """Initializes with optional input."""
        self.shadow_list = None # A lazily initialized list that shadows the BindingsSet values for indexed access
        if input is None:
            self.c_set = hp.bindings_set_single()
        elif isinstance(input, Bindings):
            self.c_set = hp.bindings_set_from_bindings(input.cbindings)
        else:
            self.c_set = input

    def __del__(self):
        """Frees the BindingsSet"""
        if self.c_set is not None:
            hp.bindings_set_free(self.c_set)
            self.c_set = None

    def __eq__(self, other):
        """Checks if other BindingsSet contains identical associations."""
        return (isinstance(other, BindingsSet) and
                hp.bindings_set_eq(self.c_set, other.c_set))

    def __repr__(self):
        """Renders a text description of a BindingsSet"""
        return hp.bindings_set_to_str(self.c_set)

    def __deepcopy__(self, memodict={}):
        """Makes a "deep copy" of a BindingsSet"""
        return self.clone()

    def __enter__(self):
        """For context management."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Frees resources on exit."""
        if self.c_set is not None:
            hp.bindings_set_free(self.c_set)
            self.c_set = None

    def __getitem__(self, key):
        """Gets a Bindings frame by index"""
        if self.shadow_list is None:
            result = hp.bindings_set_unpack(self.c_set)
            self.shadow_list = [{k: Atom._from_catom(v) for k, v in bindings.items()} for bindings in result]
        return self.shadow_list[key]

    def empty():
        """Creates a new BindingsSet without any Bindings frames.
        Conceptually, this means no valid matches exist.
        """
        return BindingsSet(hp.bindings_set_empty())

    def clone(self):
        """Makes a "deep copy" of a BindingsSet"""
        return BindingsSet(hp.bindings_set_clone(self.c_set))

    def is_empty(self) -> bool:
        """Checks if a BindingsSet contains no Bindings frames, and thus indicates
        no match."""
        return hp.bindings_set_is_empty(self.c_set)

    def is_single(self) -> bool:
        """Checks if a Bindings set contains a frame with no associations, and
        thus allows variables to take any value.
        """
        return hp.bindings_set_is_single(self.c_set)

    def push(self, bindings: Bindings):
        """Adds a Bindings frame to an existing BindingsSet

        Parameters
        ----------
        bindings:
            The Bindings set to incorporate into set. Ownership of this argument is
            taken by this function.
        """
        self.shadow_list = None
        hp.bindings_set_push(self.c_set, bindings.cbindings)

    def add_var_binding(self, var: VariableAtom, value: Atom) -> bool:
        """Adds a new variable to atom association to every Bindings frame in a
        BindingsSet.
        """
        self.shadow_list = None
        return hp.bindings_set_add_var_binding(self.c_set, var.catom, value.catom)

    def add_var_equality(self, a: Atom, b: Atom) -> bool:
        """Asserts equality between two Variable atoms in a BindingsSet."""
        self.shadow_list = None
        return hp.bindings_set_add_var_equality(self.c_set, a.catom, b.catom)

    def merge_into(self, input: Union['BindingsSet', Bindings]):
        """Merges the contents of another BindingsSet or Bindings frame."""
        self.shadow_list = None
        if isinstance(input, BindingsSet):
            hp.bindings_set_merge_into(self.c_set, input.c_set)
        else:
            new_set = BindingsSet(input)
            hp.bindings_set_merge_into(self.c_set, new_set.c_set)

    def iterator(self):
        """Returns an iterator over all Bindings frames"""
        res = hp.bindings_set_list(self.c_set)
        result = [Bindings(r) for r in res]
        return iter(result)

def get_string_value(value) -> str:
    if not isinstance(value, str):
        value = repr(value)
    if len(value) > 2 and ("\"" == value[0]) and ("\"" == value[-1]):
        return value[1:-1]
    return value
