def register_atoms(func):
    def metta_register_atoms(metta):
        for rex, atom in func().items():
            metta.register_atom(rex, atom)
    return metta_register_atoms

def register_tokens(func):
    def metta_register_tokens(metta):
        for rex, atom in func().items():
            metta.register_token(rex, atom)
    return metta_register_tokens
