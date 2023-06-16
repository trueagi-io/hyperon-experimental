import unittest

from hyperon import *
from test_common import HyperonTestCase

class TestSpace(PySpace):

    def __init__(self, unwrap=True):
        super().__init__((), "TestSpace")
        self.atoms_list = []
        self.unwrap = unwrap

    # NOTE: this is a naive implementation barely good enough to pass the tests
    # Don't take this as a guide to implementing a space query function
    def query(self, query_atom):

        #Extract only the variables from the query atom
        query_vars = list(filter(lambda atom: atom.get_type() == AtomKind.VARIABLE, query_atom.iterate()))

        #Match the query atom against every atom in the space
        new_bindings_set = BindingsSet()
        for space_atom in self.atoms_list:
            match_results = space_atom.match_atom(query_atom)

            #Merge in the bindings from this match, after we narrow the match_results to
            # only include variables vars in the query atom 
            for bindings in match_results.iterator():
                bindings.narrow_vars(query_vars)
                if not bindings.is_empty():
                    new_bindings_set.merge_into(bindings)

        return new_bindings_set

    def add(self, atom):
        self.atoms_list.append(atom)

    def remove(self, atom):
        if atom in self.atoms_list:
            self.atoms_list.remove(atom)
            return True
        else:
            return False

    def replace(self, from_atom, to_atom):
        if from_atom in self.atoms_list:
            self.atoms_list.remove(from_atom)
            self.atoms_list.append(to_atom)
            return True
        else:
            return False

    def atom_count(self):
        return len(self.atoms_list)

    def atoms_iter(self):
        return iter(self.atoms_list)

class CustomSpaceTest(HyperonTestCase):

    def test_custom_space(self):

        test_space = TestSpace()
        test_space.test_attrib = "Test Space Payload Attrib"

        kb = Space(test_space)
        kb.add_atom(S("a"))
        kb.add_atom(S("b"))

        self.assertEqual(kb.atom_count(), 2)
        self.assertEqual(kb.get_payload().test_attrib, "Test Space Payload Attrib")
        self.assertEqualNoOrder(kb.get_atoms(), [S("a"), S("b")])

    def test_remove(self):
        kb = Space(TestSpace())
        kb.add_atom(S("a"))
        kb.add_atom(S("b"))
        kb.add_atom(S("c"))

        self.assertTrue(kb.remove_atom(S("b")))
        self.assertFalse(kb.remove_atom(S("bogus")))
        self.assertEqualNoOrder(kb.get_atoms(), [S("a"), S("c")])

    def test_replace(self):
        kb = Space(TestSpace())
        kb.add_atom(S("a"))
        kb.add_atom(S("b"))
        kb.add_atom(S("c"))

        self.assertTrue(kb.replace_atom(S("b"), S("d")))
        self.assertEqualNoOrder(kb.get_atoms(), [S("a"), S("d"), S("c")])

    def test_query(self):
        kb = Space(TestSpace())
        kb.add_atom(E(S("A"), S("B")))
        kb.add_atom(E(S("C"), S("D")))

        result = kb.query(E(S("A"), V("x")))
        self.assertEqualNoOrder(result, [{"x": S("B")}])

    def test_atom_containing_space(self):
        m = MeTTa()

        #Make a little space and add it to the MeTTa interpreter's space
        little_space = Space(TestSpace())
        little_space.add_atom(E(S("A"), S("B")))
        space_atom = G(little_space)
        m.space().add_atom(E(S("little-space"), space_atom))

        #Make sure we can get the little space back, and then query it
        kb_result = m.space().query(E(S("little-space"), V("s")))
        result_atom = kb_result[0].get("s")
        self.assertEqual(result_atom, space_atom)

        result = result_atom.get_object().query(E(S("A"), V("v")))
        self.assertEqualNoOrder(result, [{"v": S("B")}])

        #Add the MeTTa space to the little space for some space recursion
        little_space.add_atom(E(S("big-space"), G(m.space())))

    def test_match_nested_custom_space(self):
        nested = Space(TestSpace())
        nested.add_atom(E(S("A"), S("B")))
        space_atom = G(nested)

        runner = MeTTa()
        runner.space().add_atom(space_atom)
        runner.tokenizer().register_token("nested", lambda token: space_atom)

        result = runner.run("!(match nested (A $x) $x)")
        self.assertEqual([[S("B")]], result)
