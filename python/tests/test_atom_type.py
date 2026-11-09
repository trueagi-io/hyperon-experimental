import unittest

from hyperon import *

class AtomTest(unittest.TestCase):

    def test_nearby_type_is_found_in_unbounded_family(self):
        # The subtype rule gives `a` infinitely many types. The search is not limited unless
        # the max-type-depth pragma is set, and a check stops when it finds the expected type.
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run('(: a (F x)) (:< (F $x) (F (F $x)))')
        space = metta.space()
        self.assertTrue(check_type(space, S('a'), E(S('F'), E(S('F'), S('x')))))
        self.assertTrue(check_type(space, S('a'), E(S('F'), E(S('F'), E(S('F'), E(S('F'), S('x')))))))
        space.add_atom(E(S(':'), S('g'), E(S('F'), S('x'))))
        self.assertTrue(check_type(space, E(S('g')), E(E(S('F'), E(S('F'), S('x'))))))

    def test_long_subtype_chain_has_no_limit(self):
        space = GroundingSpaceRef()
        space.add_atom(E(S(":"), S("a"), S("T0")))
        for i in range(200):
            space.add_atom(E(S(":<"), S(f"T{i}"), S(f"T{i + 1}")))
        self.assertEqual(len(get_atom_types(space, S("a"))), 201)
        self.assertTrue(check_type(space, S("a"), S("T200")))

    def test_check_type(self):
        space = GroundingSpaceRef()
        space.add_atom(E(S(":"), S("a"), S("A")))

        self.assertTrue(check_type(space, S("a"), AtomType.UNDEFINED))
        self.assertTrue(check_type(space, S("a"), S("A")))
        self.assertFalse(check_type(space, S("a"), S("B")))

    def test_validate_atom(self):
        space = GroundingSpaceRef()
        space.add_atom(E(S(":"), S("a"), S("A")))
        space.add_atom(E(S(":"), S("b"), S("B")))
        space.add_atom(E(S(":"), S("foo"), E(S("->"), S("A"), S("B"))))

        self.assertTrue(validate_atom(space, E(S("foo"), S("a"))))

    def test_get_atom_types(self):
        space = GroundingSpaceRef()
        space.add_atom(E(S(":"), S("a"), S("A")))
        space.add_atom(E(S(":"), S("b"), S("B")))
        space.add_atom(E(S(":"), S("foo"), E(S("->"), S("A"), S("B"))))

        self.assertEqual(get_atom_types(space, E(S("foo"), S("a"))), [S("B")])
        self.assertEqual(get_atom_types(space, E(S("foo"), S("b"))),
                [])
        self.assertEqual(get_atom_types(space, S("foo")), [E(S("->"), S("A"), S("B"))])
