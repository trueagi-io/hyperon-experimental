import os
from importlib import import_module
import hyperonpy as hp
from .atoms import Atom, AtomType, OperationAtom
from .base import GroundingSpaceRef, Tokenizer, SExprParser

class MeTTa:

    def __init__(self, space = None, cwd = ".", cmetta = None):
        if cmetta is not None:
            self.cmetta = cmetta
        else:
            if space is None:
                space = GroundingSpaceRef()
            tokenizer = Tokenizer()
            self.cmetta = hp.metta_new(space.cspace, tokenizer.ctokenizer, cwd)
            self.load_py_module("hyperon.stdlib")
            hp.metta_load_module(self.cmetta, "stdlib")
            self.register_atom('extend-py!',
                OperationAtom('extend-py!',
                              lambda name: self.load_py_module(name) or [],
                              [AtomType.UNDEFINED, AtomType.ATOM], unwrap=False))

    def __del__(self):
        hp.metta_free(self.cmetta)

    def copy(self):
        return MeTTa(cmetta = hp.metta_clone(self.cmetta))

    def space(self):
        return GroundingSpaceRef._from_cspace(hp.metta_space(self.cmetta))

    def tokenizer(self):
        return Tokenizer._from_ctokenizer(hp.metta_tokenizer(self.cmetta))

    def register_token(self, regexp, constr):
        self.tokenizer().register_token(regexp, constr)

    def register_atom(self, name, symbol):
        self.register_token(name, lambda _: symbol)

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

    def load_py_module(self, name):
        if not isinstance(name, str):
            name = repr(name)
        mod = import_module(name)
        for n in dir(mod):
            obj = getattr(mod, n)
            if '__name__' in dir(obj) and obj.__name__ == 'metta_register':
                obj(self)

    def import_file(self, fname):
        path = fname.split(os.sep)
        if len(path) == 1:
            path = ['.'] + path
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
        parser = SExprParser(program)
        results = hp.metta_run(self.cmetta, parser.cparser)
        if flat:
            return [Atom._from_catom(catom) for result in results for catom in result]
        else:
            return [[Atom._from_catom(catom) for catom in result] for result in results]
