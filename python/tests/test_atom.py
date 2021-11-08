import unittest

from hyperon import *

class AtomTest(unittest.TestCase):

    def test_symbol_equals(self):
        self.assertEqual(S("a"), S("a"))
        self.assertNotEqual(S("a"), S("b"))

    def test_symbol_str(self):
        self.assertEqual(str(S("a")), "a")

    def test_symbol_type(self):
        self.assertEqual(S("a").get_type(), Atom.SYMBOL)

    def test_symbol_get_symbol(self):
        self.assertEqual(S("a").get_symbol(), "a")

    def test_variable_equals(self):
        self.assertEqual(V("x"), V("x"))
        self.assertNotEqual(V("x"), V("y"))

    def test_variable_str(self):
        self.assertEqual(str(V("x")), "$x")

    def test_variable_type(self):
        self.assertEqual(V("x").get_type(), Atom.VARIABLE)

    def test_variable_get_name(self):
        self.assertEqual(V("x").get_name(), "x")

    def test_grounded_equals(self):
        self.assertEqual(ValueAtom(1.0), ValueAtom(1.0))
        self.assertNotEqual(ValueAtom(1.0), ValueAtom(2.0))

    def test_grounded_str(self):
        self.assertEqual(str(ValueAtom(1.0)), "1.0")

    def test_grounded_type(self):
        self.assertEqual(ValueAtom(1.0).get_type(), Atom.GROUNDED)

    def _test_grounded_execute_default(self):
        with self.assertRaises(RuntimeError) as e:
            ValueAtom(1.0).execute(GroundingSpace(), GroundingSpace())
        self.assertEqual(str(e.exception), "Operation is not supported")

    def _test_grounded_execute(self):
        self.assertEqual(X2Atom().execute(ValueAtom(1.0)), ValueAtom(2.0))

    def test_expr_equals(self):
        self.assertEqual(E(S("+"), S("1"), S("2")),
                E(S("+"), S("1"), S("2")))

    def test_expr_equals_grounded(self):
        self.assertEqual(E(X2Atom(), ValueAtom(1.0)),
                E(X2Atom(), ValueAtom(1.0)))

    def test_expr_str(self):
        self.assertEqual(str(E(X2Atom(), ValueAtom(1.0))), "(*2 1.0)")

    def test_expr_type(self):
        self.assertEqual(E(X2Atom(), ValueAtom(1.0)).get_type(), Atom.EXPR)

    def test_expr_get_children(self):
        self.assertEqual(E(X2Atom(), ValueAtom(1.0)).get_children(),
                [X2Atom(), ValueAtom(1.0)])

    def test_groundingspace_get_type(self):
        kb = GroundingSpace()
        self.assertEqual(kb.get_type(), GroundingSpace.TYPE)

    def test_groundingspace_str(self):
        kb = GroundingSpace()
        kb.add_atom(E(S("+"), S("1"), S("2")))
        self.assertEqual(str(kb), "<(+ 1 2)>")

    def test_groundingspace_equals(self):
        kb_a = GroundingSpace()
        kb_a.add_atom(E(S("+"), S("1"), S("2")))
        kb_b = GroundingSpace()
        kb_b.add_atom(E(S("+"), S("1"), S("2")))
        self.assertEqual(kb_a, kb_b)

    def test_textspace_get_type(self):
        text = TextSpace()
        self.assertEqual(text.get_type(), TextSpace.TYPE)

    def test_textspace_symbol(self):
        text = TextSpace()
        text.add_string("(+ 1 2)")
        kb = GroundingSpace()
        text.add_to(kb)

        expected = GroundingSpace()
        expected.add_atom(E(S("+"), S("1"), S("2")))
        self.assertEqual(kb, expected)

def X2Atom():
    return G(X2())

class X2(GroundedAtom):

    def __init__(self):
        GroundedAtom.__init__(self)

    def execute(self, ops, data):
        data.push(ValueAtom(2 * data.pop().value))
        return None

    def __eq__(self, other):
        return isinstance(other, X2)

    def __repr__(self):
        return "*2"

    def copy(self):
        return X2()

