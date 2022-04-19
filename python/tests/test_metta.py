import unittest

from hyperon import *

class MettaTest(unittest.TestCase):

    def test_adding_tokens_while_parsing(self):
        metta = MeTTa()

        atom = metta.parse_single('(A B)')
        self.assertEquals(atom, E(S('A'), S('B')))

        metta.add_atom('A', S('C'))
        atom = metta.parse_single('(A B)')
        self.assertEquals(atom, E(S('C'), S('B')))


