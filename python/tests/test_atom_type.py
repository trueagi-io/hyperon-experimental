import unittest

from hyperon import *

class AtomTest(unittest.TestCase):

    def test_check_type(self):
        space = GroundingSpace()
        space.add_atom(E(S(":"), S("a"), S("A")))
        
        self.assertTrue(check_type(space, S("a"), AtomType.undefined()))
        self.assertTrue(check_type(space, S("a"), AtomType.specific(S("A"))))
        self.assertFalse(check_type(space, S("a"), AtomType.specific(S("B"))))

    def test_validate_expr(self):
        space = GroundingSpace()
        space.add_atom(E(S(":"), S("a"), S("A")))
        space.add_atom(E(S(":"), S("b"), S("B")))
        space.add_atom(E(S(":"), S("foo"), E(S("->"), S("A"), S("B"))))

        self.assertTrue(validate_expr(space, E(S("foo"), S("a"))))
