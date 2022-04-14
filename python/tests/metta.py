import os
import sys

from hyperon import *
from common import Atomese, MeTTa

def run_metta(program):
    metta = MeTTa()
    status = "normal"
    result = []
    for expr in metta._parse_all(program):
        if expr == S('!'):
            status = "interp"
            continue
        if expr == S(';'):
            status = "comment"
            continue
        if status == "comment":
            pass
        elif status == "interp":
            r = interpret(metta.space, expr)
            if r != []: result += [r]
        else:
            metta.space.add_atom(expr)
        status = "normal"
    return result

if __name__ == "__main__":
    os.system('clear')
    print("\n========= MeTTa version 0.0 =========\n\n")
    f = open(sys.argv[1], "r")
    program = f.read()
    f.close()
    run_metta(program)
