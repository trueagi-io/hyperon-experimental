from hyperon.base import Atom
from hyperon.atoms import OperationAtom, E
from hyperon.ext import register_tokens
from hyperon.ext import register_atoms
from hyperon.atoms import G, AtomType
from hyperon.runner import MeTTa

import hyperonpy as hp

import sys
import readline
import os
import atexit
histfile = os.path.join(os.path.expanduser("~"), ".metta_history")

try:
    readline.set_history_length(10000)
    readline.read_history_file(histfile)
    h_len = readline.get_current_history_length()
except FileNotFoundError:
    open(histfile, 'wb').close()
    h_len = 0

def save(prev_h_len, histfile):
    new_h_len = readline.get_current_history_length()
    readline.set_history_length(10000)
    readline.append_history_file(new_h_len - prev_h_len, histfile)
atexit.register(save, h_len, histfile)



class MeTTaVS(MeTTa):
    def copy(self):
        return self

@register_atoms
def my_imported_runner_atom():
    # We don't use metta here, but we could...
    content = '''
        (: fact (-> Number Number))
        (= (fact $x)
           (case $x
            ((0 1)
             ($_ (* $x (fact (- $x 1)))))
           )
        )

        (some content)
        (= (self-from-self)
           (match &self (some $x) $x))

        something

        (= (call_func $f $arg) ($f $arg))
    '''    
    runner.run(content)
    
    return {
        'r': runnerAtom
    }

@register_tokens(pass_metta=True)
def my_resolver_atoms(metta):

    def run_resolved_symbol_op(runner, atom, *args):
        expr = E(atom, *args)
        result = hp.metta_evaluate_atom(runner.cmetta, expr.catom)
        result = [Atom._from_catom(catom) for catom in result]
        return result

    def resolve_atom(metta, token):
        # TODO: nested modules...
        runner_name, atom_name = token.split('::')
        # FIXME: using `run` for this is an overkill,
        #        but there is no good Python API for this;
        #        we may have an interface function for
        #        `tokenizer` to resolve individual symbols -
        #        metta.tokenizer().find_token ...
        #        or something else...
        # TODO: assert
        runner = metta.run('! ' + runner_name)[0][0].get_object()
        atom = runner.run('! ' + atom_name)[0][0]
        # A hack to make runner::&self work
        # TODO? the problem is that we need to return an operation to make this
        # work in parent expressions, thus, it is unclear how to return pure
        # symbols
        if atom.get_type() == hp.AtomKind.GROUNDED:
            return atom
        # TODO: borrow atom type to op
        return OperationAtom(
            token,
            lambda *args: run_resolved_symbol_op(runner, atom, *args),
            unwrap=False)

    return {
        r"[^\s]+::[^\s]+": lambda token: resolve_atom(metta, token)
    }


runner = MeTTaVS()
runnerAtom = G(runner, AtomType.ATOM)

class REPL:
    def __init__(self):
        self.history = []

    def main_loop(self):
        while True:
            try:
                # Use the input function to get user input                
                line = input("metta> ")
                
                # Check for history commands
                if line == '.history':
                    for idx, item in enumerate(self.history):
                        print(f"{idx + 1}: {item}")
                    continue

                # If the line isn't empty, evaluate it
                if line:
                    self.history.append(line)
                    result = runner.run(line)
                    if result is not None:
                        print(result)

            # Handle Ctrl+C to exit
            except KeyboardInterrupt:
                print("\nCtrl-C Exiting...")
                sys.exit(3)

            # Handle Ctrl+D to exit
            except EOFError:
                print("\n Ctrl-D EOF...")
                sys.exit(0)
                
            # If there's an error, print it
            except Exception as e:
                print(f"Error: {e}")

if __name__ == "__main__":
    repl = REPL()
    readline.add_history("!(match &self $ $)")
    repl.main_loop()

