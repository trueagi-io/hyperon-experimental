from hyperon import *
from common_old import *
import os

class MeTTa:

    def __init__(self, space=None):
        self.runner = Metta(space)
        self._tokenizer()

    def space(self):
        return self.runner.get_space()

    def tokenizer(self):
        return self.runner.get_tokenizer()

    def _tokenizer(self):
        self.add_atom(r"\+", addAtom)
        self.add_atom(r"-", subAtom)
        self.add_atom(r"\*", mulAtom)
        self.add_atom(r"/", divAtom)
        self.add_atom(r"%", modAtom)
        self.add_atom(r"==", equalAtom)
        self.add_atom(r"<", lessAtom)
        self.add_atom(r">", greaterAtom)
        self.add_atom(r"or", orAtom)
        self.add_atom(r"and", andAtom)
        self.add_atom(r"not", notAtom)
        self.add_token(r"\d+(\.\d+)",
                       lambda token: ValueAtom(float(token), 'Number'))
        self.add_token(r"\d+",
                       lambda token: ValueAtom(int(token), 'Number'))
        self.add_token("\"[^\"]*\"",
                       lambda token: ValueAtom(str(token[1:-1]), 'String'))
        self.add_token(r"True|False",
                       lambda token: ValueAtom(token == 'True', 'Bool'))
        self.add_token(r"call:[^\s]+", newCallAtom)

    def add_token(self, regexp, constr):
        self.tokenizer().register_token(regexp, constr)

    def add_atom(self, name, symbol):
        self.add_token(name, lambda _: symbol)

    def _parse_all(self, program):
        parser = SExprParser(program)
        while True:
            atom = parser.parse(self.tokenizer())
            if atom is None:
                break
            yield atom

    def parse_all(self, program):
        return list(self._parse_all(program))

    def parse_single(self, program):
        return next(self._parse_all(program))

    def import_file(self, fname):
        path = fname.split(os.sep)
        f = open(os.sep.join(path), "r")
        program = f.read()
        f.close()
        # changing cwd
        prev_cwd = os.getcwd()
        os.chdir(os.sep.join(path[:-1]))
        result = self.run(program)
        # restoring cwd
        os.chdir(prev_cwd)
        return result

    def run(self, program, flat=False):
        return self.runner.run(program)
