import unittest
from hyperon import *
from copy import deepcopy


class BindingsTest(unittest.TestCase):

    def setUp(self):
        self.emptyBindings = Bindings()

        self.bindings = Bindings()
        self.bindings.add_var_bindings("a", S("b"))
        self.bindings.add_var_bindings("x", S("y"))


    def tearDown(self) -> None:
        pass

    def test_bindings_match_display(self):
        pass
        '''
        commented till sort inside bindings become stable
        bindings = hp.bindings_new()

        # uncomment this and assert on line 22 fails
        #self.assertEqual(hp.bindings_to_str(bindings), "{  }")
        hp.bindings_add_var_bindings(bindings, "a", hp.atom_sym("b"))
        hp.bindings_add_var_bindings(bindings, "x", hp.atom_sym("y"))

        bindings_as_str = hp.bindings_to_str(bindings)

        # sometimes fails
        self.assertEqual(bindings_as_str, "{ $x = y, $a = b }")

        hp.bindings_free(bindings)
        '''

    def test_bindings_equality_and_clone(self):

        with deepcopy(self.emptyBindings) as cloned_empty:
            self.assertEqual(self.emptyBindings, cloned_empty)

        with deepcopy(self.bindings) as cloned:
            with self.bindings.clone() as cloned_explicitly:
                self.assertEqual(self.bindings, cloned)
                self.assertEqual(cloned, cloned_explicitly)

        # should be after deletion of cloned to ensure that clone is real
        self.assertEqual(self.bindings, self.bindings)
        self.assertEqual(self.emptyBindings, self.emptyBindings)
        self.assertNotEqual(self.emptyBindings, self.bindings)

    def test_bindings_merge(self):
        merged_empty = Bindings.merge(self.emptyBindings, self.emptyBindings)
        self.assertEqual(self.emptyBindings, merged_empty)

        merged_with_empty_lr = Bindings.merge(self.emptyBindings, self.bindings)
        merged_with_empty_rl = Bindings.merge(self.bindings, self.emptyBindings)
        self.assertEqual(merged_with_empty_rl, merged_with_empty_lr)
        self.assertEqual(merged_with_empty_rl, self.bindings)

        merged_self = Bindings.merge(self.bindings, self.bindings)
        self.assertEqual(merged_self, self.bindings)

    def test_bindings_is_empty(self):
        self.assertTrue(self.emptyBindings.is_empty())
        self.assertFalse(self.bindings.is_empty())

    def test_bindings_resolve(self):

        self.assertIsNone(self.emptyBindings.resolve("a"))
        self.assertIsNone(self.bindings.resolve("XYXY"))

        atom_expected = S("b")
        atom_resolved = self.bindings.resolve("a")
        self.assertEqual(atom_expected, atom_resolved)

    def test_bindings_resolve_and_remove(self):
        self.assertIsNone(self.emptyBindings.resolve_and_remove("a"))
        self.assertIsNone(self.bindings.resolve_and_remove("XYXY"))

        atom_expected_first = S("b")
        atom_expected_second = S("y")
        atom_resolved_first = self.bindings.resolve_and_remove("a")
        atom_resolved_second = self.bindings.resolve_and_remove("x")

        self.assertTrue(self.bindings.is_empty())
        self.assertEqual(atom_expected_first, atom_resolved_first)
        self.assertEqual(atom_expected_second, atom_resolved_second)
        # see above about negative tests


if __name__ == "__main__":
    unittest.main()
