import unittest
from hyperon import *
from copy import deepcopy


class BindingsTest(unittest.TestCase):

    def setUp(self):
        self.emptyBindings = Bindings()

        self.bindings = Bindings()
        self.bindings.add_var_binding("a", S("b"))
        self.bindings.add_var_binding("x", S("y"))


    def tearDown(self) -> None:
        pass

    def test_bindings_match_display(self):
        pass
        '''
        commented till sort inside bindings become stable
        bindings = hp.bindings_new()

        # uncomment this and assert on line 22 fails
        #self.assertEqual(hp.bindings_to_str(bindings), "{  }")
        hp.bindings_add_var_binding(bindings, "a", hp.atom_sym("b"))
        hp.bindings_add_var_binding(bindings, "x", hp.atom_sym("y"))

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
        self.assertEqual(BindingsSet(self.emptyBindings), merged_empty)

        merged_with_empty_lr = Bindings.merge(self.emptyBindings, self.bindings)
        merged_with_empty_rl = Bindings.merge(self.bindings, self.emptyBindings)
        self.assertEqual(merged_with_empty_rl, merged_with_empty_lr)
        self.assertEqual(merged_with_empty_rl, BindingsSet(self.bindings))

        merged_self = Bindings.merge(self.bindings, self.bindings)
        self.assertEqual(merged_self, BindingsSet(self.bindings))

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

    def test_bindings_iterator(self):
        pass
        # uncomment below as sort in bindings become stable.
        '''

        it = self.bindings.iterator()
        expected_names = ["x", "a"]
        expected_atoms = [S("y"), S("b")]

        for x, expected_name, expected_atom in zip(it, expected_names, expected_atoms):
            self.assertEqual(expected_name, x[0])
            self.assertEqual(expected_atom, x[1])
       '''

    def test_bindings_set(self):

        empty_set = BindingsSet.empty()
        self.assertFalse(empty_set.is_single())
        self.assertTrue(empty_set.is_empty())

        new_bindings = Bindings()
        new_bindings.add_var_binding(V("a"), S("A"))
        empty_set.push(new_bindings)
        no_longer_empty_set = empty_set

        set = BindingsSet()
        self.assertTrue(set.is_single())
        self.assertFalse(set.is_empty())

        set.add_var_binding(V("a"), S("A"))
        self.assertFalse(set.is_single())
        self.assertFalse(set.is_empty())
        self.assertEqual(set, no_longer_empty_set);        

        new_bindings = Bindings()
        new_bindings.add_var_binding(V("a"), S("A"))
        set_2 = BindingsSet(new_bindings)
        self.assertEqual(set, set_2)

        cloned_set = deepcopy(set)
        self.assertEqual(set, cloned_set)

        cloned_set.add_var_binding(V("a"), S("B"))
        self.assertTrue(cloned_set.is_empty())

        set.add_var_equality(V("a"), V("a_prime"))

        bindings_counter = 0
        for _ in set.iterator():
            bindings_counter += 1
        self.assertEqual(1, bindings_counter)

        new_bindings = Bindings()
        new_bindings.add_var_binding(V("b"), S("B"))
        set.merge_into(new_bindings)

        new_bindings = Bindings()
        new_bindings.add_var_binding(V("c"), S("C"))
        new_set = BindingsSet(new_bindings)
        set.merge_into(new_set)

        new_bindings = Bindings()
        new_bindings.add_var_binding(V("d"), S("D"))
        set_bindings_list = list(set.iterator())
        self.assertEqual(len(set_bindings_list), 1)

        new_set = new_bindings.merge(set_bindings_list[0])
        expected_bindings_set = BindingsSet()
        expected_bindings_set.add_var_binding(V("a"), S("A"))
        expected_bindings_set.add_var_binding(V("b"), S("B"))
        expected_bindings_set.add_var_binding(V("c"), S("C"))
        expected_bindings_set.add_var_binding(V("d"), S("D"))
        expected_bindings_set.add_var_equality(V("a"), V("a_prime"))

        self.assertEqual(new_set, expected_bindings_set)

if __name__ == "__main__":
    unittest.main()
