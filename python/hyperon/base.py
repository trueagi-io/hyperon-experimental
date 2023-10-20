import hyperonpy as hp

from .atoms import Atom, BindingsSet
from hyperonpy import SyntaxNodeType

class AbstractSpace:
    """
    A virtual base class upon which Spaces can be implemented in Python
    """
    def __init__(self):
        """Initialiize the AbstractSpace. Does nothing in the base class"""
        return

    def query(self, query_atom):
        """
        Performs the specified query on the Space.
        Should be overridden to return a BindingsSet as the result of the query.
        """
        raise RuntimeError("Space::query() is not implemented")

    # TODO (INTERNAL): Currently unimplemented.  We may do this differently depending on lazy / comprehensions
    # so I don't want to write throw-away code unless it's needed downstream.
    # def subst(self, pattern, templ):
    #     None

    def add(self, atom):
        """
        Adds an Atom to the atom space. Must be implemented in derived classes.
        """
        raise RuntimeError("Space::add() is not implemented")

    def remove(self, atom):
        """
        Removes an Atom from the atom space. Must be implemented in derived classes.
        """
        raise RuntimeError("Space::remove() is not implemented")

    def replace(self, atom, replacement):
        """
        Replaces an Atom from the atom space. Must be implemented in derived classes.
        """
        raise RuntimeError("Space::replace() is not implemented")

    def atom_count(self):
        """
        Counts the number of atoms in the atom space. Optional for derived classes.
        """
        None

    def atoms_iter(self):
        """
        Returns an iterator over atoms in the Space. Optional for derived classes.
        """
        None

class GroundingSpace(AbstractSpace):
    """
    A wrapper over the native GroundingSpace implementation, which can be subclassed
    and extended within Python
    """
    def __init__(self):
        """Initialize GroundingSpace and its underlying native implementation."""
        super().__init__()
        # self.cspace = hp.space_new_grounding()
        self.gspace = GroundingSpaceRef()

    def query(self, query_atom):
        """
        Delegates the query to the underlying native GroundingSpace
        and returns the result BindingsSet
        """
        return self.gspace.query(query_atom)

    # TODO (INTERNAL): Currently unimplemented.
    # def subst(self, pattern, templ):

    def add(self, atom):
        """
        Adds an Atom to the atom space.
        """
        self.gspace.add_atom(atom)

    def remove(self, atom):
        """
        Removes an Atom from the atom space.
        """
        return self.gspace.remove_atom(atom)

    def replace(self, from_atom, to_atom):
        """
        Replaces an Atom in the atom space.
        """
        return self.gspace.replace_atom(from_atom, to_atom)

    def atom_count(self):
        """
        Counts the number of Atoms in the atom space.
        """
        return self.gspace.atom_count()

    def atoms_iter(self):
        """
        Returns an iterator over atoms in the atom space.
        """
        return iter(self.gspace.get_atoms())

def _priv_call_query_on_python_space(space, query_catom):
    """
    Private glue for Hyperonpy implementation.
    Translates a native 'catom' into an Atom object, and then delegates the query
    to the provided 'space' object.
    """
    query_atom = Atom._from_catom(query_catom)
    return space.query(query_atom)

def _priv_call_add_on_python_space(space, catom):
    """
    Private glue for Hyperonpy implementation.
    Translates a native 'catom' into an Atom object, and then adds it
    to the provided 'space' object.
    """
    atom = Atom._from_catom(catom)
    space.add(atom)

def _priv_call_remove_on_python_space(space, catom):
    """
    Private glue for Hyperonpy implementation.
    Translates a native 'catom' into an Atom object, and then removes it
    from the provided 'space' object.
    """
    atom = Atom._from_catom(catom)
    return space.remove(atom)

def _priv_call_replace_on_python_space(space, cfrom, cto):
    """
    Private glue for Hyperonpy implementation.
    Translates native 'catom' objects into Atom objects, and then replaces
    the first with the second in the provided 'space' object.
    """
    from_atom = Atom._from_catom(cfrom)
    to_atom = Atom._from_catom(cto)
    return space.replace(from_atom, to_atom)

def _priv_call_atom_count_on_python_space(space):
    """
    Private glue for Hyperonpy implementation.
    Returns the number of Atoms in the provided 'space' object.
    """
    if hasattr(space, "atom_count"):
        count = space.atom_count()
        if count is not None:
            return count
        else:
            return -1
    else:
        return -1

def _priv_call_new_iter_state_on_python_space(space):
    """
    Private glue for Hyperonpy implementation.
    Returns an iterator over Atoms in the provided 'space' object.
    """
    if hasattr(space, "atoms_iter"):
        return space.atoms_iter()
    else:
        return None

class SpaceRef:
    """
    A reference to a Space, which may be accessed directly, wrapped in a grounded atom,
    or passed to a MeTTa interpreter.
    """

    def __init__(self, space_obj):
        """
        Initialize a new SpaceRef based on the given space object, either a CSpace 
        or a custom Python object.
        """
        if type(space_obj) is hp.CSpace:
            self.cspace = space_obj
        else:
            self.cspace = hp.space_new_custom(space_obj)

    def __del__(self):
        """Free the underlying CSpace object """
        hp.space_free(self.cspace)

    def __eq__(self, other):
        """Compare two SpaceRef objects for equality, based on their underlying spaces."""
        return hp.space_eq(self.cspace, other.cspace)

    @staticmethod
    def _from_cspace(cspace):
        """
        Create a new SpaceRef based on the given CSpace object.
        """
        return SpaceRef(cspace)

    def copy(self):
        """
        Returns a new copy of the SpaceRef, referencing the same underlying Space.
        """
        return self

    def add_atom(self, atom):
        """
        Add an Atom to the Space.
        """
        hp.space_add(self.cspace, atom.catom)

    def remove_atom(self, atom):
        """
        Delete the specified Atom from the Space.
        """
        return hp.space_remove(self.cspace, atom.catom)

    def replace_atom(self, atom, replacement):
        """
        Replaces the specified Atom, if it exists in the Space, with the supplied replacement.
        """
        return hp.space_replace(self.cspace, atom.catom, replacement.catom)

    def atom_count(self):
        """
        Returns the number of Atoms in the Space, or -1 if it cannot be readily computed.
        """
        return hp.space_atom_count(self.cspace)

    def get_atoms(self):
        """
        Returns a list of all Atoms in the Space, or None if that is impossible.
        """
        res = hp.space_list(self.cspace)
        if res == None:
            return None
        result = []
        for r in res:
            result.append(Atom._from_catom(r))
        return result

    def get_payload(self):
        """
        Returns the Space object referenced by the SpaceRef, or None if the object does not have a
        direct Python interface.
        """
        return hp.space_get_payload(self.cspace)

    def query(self, pattern):
        """
        Performs the specified query on the Space, and returns the result as a BindingsSet.
        """
        result = hp.space_query(self.cspace, pattern.catom)
        return BindingsSet(result)

    def subst(self, pattern, templ):
        """
        Performs a substitution within the Space, based on a pattern and a template.
        """
        return [Atom._from_catom(catom) for catom in
                hp.space_subst(self.cspace, pattern.catom,
                                         templ.catom)]

class GroundingSpaceRef(SpaceRef):
    """
    A reference to a native GroundingSpace, implemented by the MeTTa core library.
    """

    def __init__(self, cspace = None):
        """
        Initialize a new GroundingSpaceRef.
        If a CSpace object is provided, use it; otherwise create a new GroundingSpace.
        """
        if cspace is None:
            self.cspace = hp.space_new_grounding()
        else:
            self.cspace = cspace

    @staticmethod
    def _from_cspace(cspace):
        """
        Creates a GroundingSpaceRef from a CSpace object.
        """
        return GroundingSpaceRef(cspace)

class Tokenizer:
    """
    A class responsible for text tokenization in the context of Hyperon.
    This class wraps around a Tokenizer object from the core library.
    """

    def __init__(self, ctokenizer = None):
        """
        Initialize a new Tokenizer.
        """
        if ctokenizer is None:
            self.ctokenizer = hp.tokenizer_new()
        else:
            self.ctokenizer = ctokenizer

    @staticmethod
    def _from_ctokenizer(ctokenizer):
        """
        Creates a Tokenizer from a CTokenizer object.
        """
        return Tokenizer(ctokenizer)

    def __del__(self):
        """
        Destructor that frees the underlying resources when the Tokenizer instance is destroyed.
        """
        hp.tokenizer_free(self.ctokenizer)

    def register_token(self, regex, constr):
        """
        Registers a new custom Token in the Tokenizer based on a regular expression.

        Parameters:
        ----------
        regex:
           A string representing the regular expression to match incoming text.
           Hyperon uses the Rust RegEx engine and syntax.
       constr:
           A constructor function for generating a new atom when the regex is triggered.
       """
        hp.tokenizer_register_token(self.ctokenizer, regex, constr)

class SyntaxNode:
    """
    A class representing a node in a parsed syntax tree
    """

    def __init__(self, cnode):
        """
        Initialize a new Tokenizer.
        """
        self.cnode = cnode

    def __del__(self):
        """
        Destructor for the SyntaxNode
        """
        hp.syntax_node_free(self.cnode)

    def get_type(self):
        """
        Returns the type of a SyntaxNode
        """
        return hp.syntax_node_type(self.cnode)

    def src_range(self):
        """
        Returns the range of offsets into the source code of the text represented by the SyntaxNode
        """
        range_tuple = hp.syntax_node_src_range(self.cnode)
        return range(range_tuple[0], range_tuple[1])

    def unroll(self):
        """
        Returns a list of all leaf nodes recursively contained within a SyntaxNode
        """
        syntax_nodes = []
        for cnode in hp.syntax_node_unroll(self.cnode):
            syntax_nodes.append(SyntaxNode(cnode))
        return syntax_nodes

class SExprParser:
    """
    A class responsible for parsing S-expressions (Symbolic Expressions).
    This class wraps around a SExprParser object from the core library.
    """

    def __init__(self, text):
        """Initialize a new SExprParser object."""
        self.cparser = hp.CSExprParser(text)

    def parse(self, tokenizer):
        """
        Parses the S-expression using the provided Tokenizer.
        """
        catom = self.cparser.parse(tokenizer.ctokenizer)
        if (catom is None):
            err_str = self.cparser.sexpr_parser_err_str()
            if (err_str is None):
                return None
            else:
                raise SyntaxError(err_str)
        else:
            return Atom._from_catom(catom)

    def parse_to_syntax_tree(self):
        """
        Parses the S-expression into a SyntaxNode representing the top-level of a syntax tree.
        """
        cnode = self.cparser.parse_to_syntax_tree()
        return SyntaxNode(cnode) if cnode is not None else None

class Interpreter:
    """
    A wrapper class for the MeTTa interpreter that handles the interpretation of expressions in a given grounding space.

    NOTE: This is a low-level API, and most applications would be better served by a `MeTTa` runner object
    """

    def __init__(self, gnd_space, expr):
        """
        Initializes the interpreter with the given grounding space and expression.
        """
        self.step_result = hp.interpret_init(gnd_space.cspace, expr.catom)

    def has_next(self):
        """
        Checks if there are more steps to execute in the interpretation plan.
        """
        return hp.step_has_next(self.step_result)

    def next(self):
        """
        Executes the next step in the interpretation plan.
        """
        if not self.has_next():
            raise StopIteration()
        self.step_result = hp.interpret_step(self.step_result)

    def get_result(self):
        """
        Retrieves the final outcome of the interpretation plan.
        """
        if self.has_next():
            raise RuntimeError("Plan execution is not finished")
        return hp.step_get_result(self.step_result)

    def get_step_result(self):
        """
        Gets the current result of the interpretation plan.
        """
        return self.step_result


def interpret(gnd_space, expr):
    """
    Parses the given expression in the specified grounding space.
    """
    interpreter = Interpreter(gnd_space, expr)
    while interpreter.has_next():
        interpreter.next()
    return [Atom._from_catom(catom) for catom in interpreter.get_result()]

def check_type(gnd_space, atom, type):
    """
    Checks whether the given Atom has the specified type in the given space context.

    Parameters
    ----------
    gnd_space:
        A pointer to the space_t representing the space context in which to perform
        the check
    atom:
        A pointer to the atom_t or atom_ref_t representing the atom whose Type the
        function will check
    type:
        A pointer to the atom_t or atom_ref_t representing the type to check against
    """

    return hp.check_type(gnd_space.cspace, atom.catom, type.catom)

def validate_atom(gnd_space, atom):
    """
    Checks whether the given Atom is correctly typed.

    Parameters
    ----------
    gnd_space:
        A pointer to the space_t representing the space context in which to perform
        the check
    atom:
        A pointer to the atom_t or atom_ref_t representing the atom whose Type the
        function will check

    Returns
    -------
    True if the Atom is correctly typed, otherwise false
    """
    return hp.validate_atom(gnd_space.cspace, atom.catom)

def get_atom_types(gnd_space, atom):
    """Provides all types for the given Atom in the context of the given Space."""
    result = hp.get_atom_types(gnd_space.cspace, atom.catom)
    return [Atom._from_catom(catom) for catom in result]

def atom_is_error(atom):
    """Checks whether an Atom is an error expression"""
    return hp.atom_is_error(atom.catom)
