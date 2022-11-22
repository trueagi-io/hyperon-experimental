def add_atoms(func):
    def metta_add_atoms(metta):
        for rex, atom in func().items():
            metta.add_atom(rex, atom)
    return metta_add_atoms

def add_tokens(func):
    def metta_add_tokens(metta):
        for rex, atom in func().items():
            metta.add_token(rex, atom)
    return metta_add_tokens
