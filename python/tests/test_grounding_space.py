import unittest

from hyperon import *
from test_common import HyperonTestCase

class GroundingSpaceTest(HyperonTestCase):

    def test_add(self):
        kb = GroundingSpace()
        kb.add_atom(S("a"))

        kb.add_atom(S("b"))

        self.assertEqualNoOrder(kb.get_atoms(), [S("a"), S("b")])

    def test_remove(self):
        kb = GroundingSpace()
        kb.add_atom(S("a"))
        kb.add_atom(S("b"))
        kb.add_atom(S("c"))

        self.assertTrue(kb.remove_atom(S("b")))

        self.assertEqualNoOrder(kb.get_atoms(), [S("a"), S("c")])

    def test_replace(self):
        kb = GroundingSpace()
        kb.add_atom(S("a"))
        kb.add_atom(S("b"))
        kb.add_atom(S("c"))

        self.assertTrue(kb.replace_atom(S("b"), S("d")))

        self.assertEqualNoOrder(kb.get_atoms(), [S("a"), S("d"), S("c")])

    def test_complex_query(self):
        kb = GroundingSpace()
        kb.add_atom(E(S("A"), S("B")))
        kb.add_atom(E(S("C"), S("B")))

        result = kb.query(E(S(","), E(S("A"), V("x")), E(S("C"), V("x"))))
        self.assertEqualNoOrder(result, [{"x": S("B")}])
