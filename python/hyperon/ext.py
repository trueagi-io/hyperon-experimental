def pass_metta(func):
    def metta_register(metta):
        func(metta, pass_metta=True)
    return metta_register

def register_atoms(func):
    def metta_register(metta, pass_metta=False):
        regs = func(metta) if pass_metta else func()
        for rex, atom in regs.items():
            metta.register_atom(rex, atom)
    return metta_register

def register_tokens(func):
    def metta_register(metta, pass_metta=False):
        regs = func(metta) if pass_metta else func()
        for rex, atom in regs.items():
            metta.register_token(rex, atom)
    return metta_register
