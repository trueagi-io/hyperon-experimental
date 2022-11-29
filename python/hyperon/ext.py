from .runner import MeTTa

def register_results(method, args, kwargs):
    if len(args) == 1 and len(kwargs) == 0 and callable(args[0]):
        # no arguments
        func = args[0]
        def metta_register(metta):
            method(metta, func())
        return metta_register
    else:
        # with arguments
        pass_metta = kwargs.get('pass_metta', False)
        def inner(func):
            def metta_register(metta):
                regs = func(metta) if pass_metta else func()
                method(metta, regs)
            return metta_register
        return inner

def register_atoms(*args, **kwargs):
    """Function decorator which registers returned pairs of regular expressions
    and atoms in MeTTa tokenizer using MeTTa.register_atom() method.

    Parameters
    ----------
    pass_metta : bool, optional
        Pass instance of MeTTa class to the decorated function as an argument.
        Default is False.
    """
    def register_atoms_internal(metta, regs):
        for rex, atom in regs.items():
            metta.register_atom(rex, atom)
    return register_results(register_atoms_internal, args, kwargs)

def register_tokens(*args, **kwargs):
    """Function decorator which registers returned pairs of regular expressions
    and lambdas in MeTTa tokenizer using MeTTa.register_token() method.

    Parameters
    ----------
    pass_metta : bool, optional
        Pass instance of MeTTa class to the decorated function as an argument.
        Default is False.
    """
    def register_tokens_internal(metta, regs):
        for rex, lam in regs.items():
            metta.register_token(rex, lam)
    return register_results(register_tokens_internal, args, kwargs)
