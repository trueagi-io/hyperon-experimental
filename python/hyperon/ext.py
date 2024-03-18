from .runner import MeTTa

def register_results(method, args, kwargs):
    """Returns a decorator for registering the results of a method.
    The behavior of the decorator depends on whether it is used with or without arguments."""

    # Case 1: Decorator used without arguments (i.e., @decorator instead of @decorator(args))
    if len(args) == 1 and len(kwargs) == 0 and callable(args[0]):
        func = args[0]  # func is the decorated function

        # Define the decorator
        def metta_register(run_context):
            # Register the results of calling the decorated function using the provided method
            method(run_context, func())
        return metta_register

    # Case 2: Decorator used with arguments (i.e., @decorator(args))
    else:
        # Check if the decorator is used with arguments
        pass_metta = kwargs.get('pass_metta', False)

        # Define the decorator
        def inner(func):
            def metta_register(run_context):
                # Get the results of calling the decorated function
                regs = func(run_context.metta()) if pass_metta else func()
                # Register the results using the provided method
                method(run_context, regs)
            return metta_register
        return inner

def register_atoms(*args, **kwargs):
    """Function decorator which registers returned pairs of regular expressions
    and atoms in a Tokenizer using the RunContext.register_atom() method.

    Parameters
    ----------
    pass_metta:
        Pass instance of MeTTa class to the decorated function as an argument.
        Default is False.
    """
    def register_atoms_internal(run_context, regs):
        for rex, atom in regs.items():
            run_context.register_atom(rex, atom)
    return register_results(register_atoms_internal, args, kwargs)

def register_tokens(*args, **kwargs):
    """Function decorator which registers returned pairs of regular expressions
    and lambdas in a Tokenizer using the RunContext.register_token() method.

    Parameters
    ----------
    pass_metta : bool, optional
        Pass instance of MeTTa class to the decorated function as an argument.
        Default is False.
    """
    def register_tokens_internal(run_context, regs):
        for rex, lam in regs.items():
            run_context.register_token(rex, lam)
    return register_results(register_tokens_internal, args, kwargs)
