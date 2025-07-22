from enum import Enum
from functools import wraps
from .atoms import OperationAtom

class RegisterType(Enum):
    ATOM = 1
    TOKEN = 2

def mark_register_function(type, args, kwargs):
    """Mark function as function which registers MeTTa atoms or tokens.
    The following attributes are added to the decorated function:
      - metta_type - value of the 'type' parameter
      - metta_pass_metta - value of `pass_metta` parameter

    Parameters
    ----------
    type:
        Kind of the register function:
        RegisterType.TOKEN if function register tokens;
        RegisterType.ATOM if it register atoms.
    args:
        args passed to decorator. If no argument are used in decorator then
        this list contains only function to be decorated. If decorator has
        some arguments then this list doesn't contain function and keeps
        decorator arguments instead.
    kwargs:
        kwargs passed to decorator. If arguments are used in decorator then
        it contains named arguments otherwise it is empty.
    """
    # Case 1: Decorator used without arguments (i.e., @decorator instead of @decorator(args))
    if len(args) == 1 and len(kwargs) == 0 and callable(args[0]):
        func = args[0]

        @wraps(func)
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs)

        wrapper.__dict__['metta_type'] = type
        wrapper.__dict__['metta_pass_metta'] = False

        return wrapper
    # Case 2: Decorator used with arguments (i.e., @decorator(args))
    else:
        pass_metta = kwargs.get('pass_metta', False)

        def no_args_decorator(func):

            @wraps(func)
            def wrapper(*args, **kwargs):
                return func(*args, **kwargs)

            wrapper.__dict__['metta_type'] = type
            wrapper.__dict__['metta_pass_metta'] = pass_metta

            return wrapper

        return no_args_decorator

def register_atoms(*args, **kwargs):
    """Function decorator which registers returned pairs of regular expressions
    and atoms in a Tokenizer using the RunContext.register_atom() method.

    Parameters
    ----------
    pass_metta:
        Pass instance of MeTTa class to the decorated function as an argument.
        Default is False.
    """
    return mark_register_function(RegisterType.ATOM, args, kwargs)

def register_tokens(*args, **kwargs):
    """Function decorator which registers returned pairs of regular expressions
    and lambdas in a Tokenizer using the RunContext.register_token() method.

    Parameters
    ----------
    pass_metta : bool, optional
        Pass instance of MeTTa class to the decorated function as an argument.
        Default is False.
    """
    return mark_register_function(RegisterType.TOKEN, args, kwargs)

def _register_grounded(metta, func):
    name = func.__name__
    func_atom = OperationAtom(name, func, unwrap=True)
    if metta is not None:
        metta.register_atom(name, func_atom)
    else:
        return mark_register_function(RegisterType.ATOM, [lambda: {name: func_atom}], [])

def grounded(arg):
    """Function decorator which registers a purely Python grounded function
    using its name as a token and unwrap=True.
    There are two ways of using this decorator:
      - @grounded without arguments and parentheses in extensions
      - @grounded(metta), where metta is a MeTTa instance, within Python scripts
    Note that MeTTa object is passed to the decorator - not to the grounded function,
    and @grounded creates an atom out of the function itself (which differs from
    register_atoms, which decorates a function returning mappings from tokens to atoms)
    """
    if callable(arg):
        return _register_grounded(None, arg)
    else:
        return lambda func: _register_grounded(arg, func)
