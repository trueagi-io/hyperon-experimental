import unittest

from hyperon import *

class GroundingSpaceTest(unittest.TestCase):

    def test_add(self):
        kb = GroundingSpace()
        kb.add_atom(S("a"))

        kb.add_atom(S("b"))

        self.assertEqual(kb.get_atoms(), [S("a"), S("b")])

    def test_remove(self):
        kb = GroundingSpace()
        kb.add_atom(S("a"))
        kb.add_atom(S("b"))
        kb.add_atom(S("c"))

        kb.remove_atom(S("b"))

        self.assertEqual(kb.get_atoms(), [S("a"), S("c")])

    def test_replace(self):
        kb = GroundingSpace()
        kb.add_atom(S("a"))
        kb.add_atom(S("b"))
        kb.add_atom(S("c"))

        kb.replace_atom(S("b"), S("d"))

        self.assertEqual(kb.get_atoms(), [S("a"), S("d"), S("c")])
