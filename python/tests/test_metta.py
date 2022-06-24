import unittest

from hyperon import *
from common import MeTTa

class MettaTest(unittest.TestCase):

    def test_adding_tokens_while_parsing(self):
        metta = MeTTa()

        atom = metta.parse_single('(A B)')
        self.assertEquals(atom, E(S('A'), S('B')))

        metta.add_atom('A', S('C'))
        atom = metta.parse_single('(A B)')
        self.assertEquals(atom, E(S('C'), S('B')))

        # NOTE: currently, adding another atom for the same token
        #       doesn't change the previous binding
        # This can be changed later
        metta.add_atom('A', S('F'))
        atom = metta.parse_single('(A B)')
        self.assertEquals(atom, E(S('C'), S('B')))


