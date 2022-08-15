import unittest

from hyperon import *

class AtomTest(unittest.TestCase):

    def test_check_type(self):
        space = GroundingSpace()
        space.add_atom(E(S(":"), S("a"), S("A")))

        self.assertTrue(check_type(space, S("a"), AtomType.UNDEFINED))
        self.assertTrue(check_type(space, S("a"), S("A")))
        self.assertFalse(check_type(space, S("a"), S("B")))

    def test_validate_atom(self):
        space = GroundingSpace()
        space.add_atom(E(S(":"), S("a"), S("A")))
        space.add_atom(E(S(":"), S("b"), S("B")))
        space.add_atom(E(S(":"), S("foo"), E(S("->"), S("A"), S("B"))))

        self.assertTrue(validate_atom(space, E(S("foo"), S("a"))))

    def test_get_atom_types(self):
        space = GroundingSpace()
        space.add_atom(E(S(":"), S("a"), S("A")))
        space.add_atom(E(S(":"), S("b"), S("B")))
        space.add_atom(E(S(":"), S("foo"), E(S("->"), S("A"), S("B"))))

        self.assertEqual(get_atom_types(space, E(S("foo"), S("a"))), [S("B")])
        self.assertEqual(get_atom_types(space, E(S("foo"), S("b"))),
                [])
        self.assertEqual(get_atom_types(space, S("foo")), [E(S("->"), S("A"), S("B"))])
