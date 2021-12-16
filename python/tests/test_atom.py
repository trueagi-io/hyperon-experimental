import unittest

from hyperon import *

class AtomTest(unittest.TestCase):

    def test_symbol_equals(self):
        self.assertEqual(S("a"), S("a"))
        self.assertNotEqual(S("a"), S("b"))

    def test_symbol_str(self):
        self.assertEqual(str(S("a")), "a")

    def test_symbol_type(self):
        self.assertEqual(S("a").get_type(), AtomType.SYMBOL)

    def test_symbol_get_symbol(self):
        self.assertEqual(S("a").get_name(), "a")

    def test_symbol_get_symbol_utf8(self):
        self.assertEqual(S("здравствуйте").get_name(), "здравствуйте")

    def test_variable_equals(self):
        self.assertEqual(V("x"), V("x"))
        self.assertNotEqual(V("x"), V("y"))

    def test_variable_str(self):
        self.assertEqual(str(V("x")), "$x")

    def test_variable_type(self):
        self.assertEqual(V("x").get_type(), AtomType.VARIABLE)

    def test_variable_get_name(self):
        self.assertEqual(V("x").get_name(), "x")

    def test_grounded_equals(self):
        self.assertEqual(ValueAtom(1.0), ValueAtom(1.0))
        self.assertNotEqual(ValueAtom(1.0), ValueAtom(2.0))

    def test_grounded_str(self):
        self.assertEqual(str(ValueAtom(1.0)), "1.0")

    def test_grounded_type(self):
        self.assertEqual(ValueAtom(1.0).get_type(), AtomType.GROUNDED)

    # def test_grounded_execute_default(self):
        # self.assertEqual(ValueAtom(1.0).get_object().execute(VecAtom(),
            # VecAtom()), "1.0 is not executable")

    def test_grounded_execute(self):
        res = X2Atom().get_object().execute(ValueAtom(1.0))
        self.assertEqual(res, [ValueAtom(2.0)])

    def test_expr_equals(self):
        self.assertEqual(E(S("+"), S("1"), S("2")),
                E(S("+"), S("1"), S("2")))

    def test_expr_equals_grounded(self):
        self.assertEqual(E(X2Atom(), ValueAtom(1.0)),
                E(X2Atom(), ValueAtom(1.0)))

    def test_expr_str(self):
        self.assertEqual(str(E(X2Atom(), ValueAtom(1.0))), "(*2 1.0)")

    def test_expr_type(self):
        self.assertEqual(E(X2Atom(), ValueAtom(1.0)).get_type(), AtomType.EXPR)

    # def test_expr_get_children(self):
        # self.assertEqual(E(X2Atom(), ValueAtom(1.0)).get_children(),
                # [X2Atom(), ValueAtom(1.0)])

    # def test_groundingspace_str(self):
        # kb = GroundingSpace()
        # kb.add_atom(E(S("+"), S("1"), S("2")))
        # self.assertEqual(str(kb), "<(+ 1 2)>")

    def test_groundingspace_equals(self):
        kb_a = GroundingSpace()
        kb_a.add_atom(E(S("+"), S("1"), S("2")))
        kb_b = GroundingSpace()
        kb_b.add_atom(E(S("+"), S("1"), S("2")))
        self.assertEqual(kb_a, kb_b)

    # def test_sexprspace_get_type(self):
        # text = SExprSpace()
        # self.assertEqual(text.get_type(), SExprSpace.TYPE)

    def test_sexprspace_symbol(self):
        text = SExprSpace()
        text.add_string("(+ 1 2)")
        kb = GroundingSpace()
        text.add_to(kb)

        expected = GroundingSpace()
        expected.add_atom(E(S("+"), S("1"), S("2")))
        self.assertEqual(kb, expected)

    def test_sexprspace_token(self):
        text = SExprSpace()
        text.register_token("\\d+", lambda token: ValueAtom(int(token)))
        text.add_string("(+ 1 2)")
        kb = GroundingSpace()
        text.add_to(kb)

        expected = GroundingSpace()
        expected.add_atom(E(S("+"), ValueAtom(1), ValueAtom(2)))
        self.assertEqual(kb, expected)

    def test_interpret(self):
        space = GroundingSpace()
        self.assertEqual(interpret(space, E(X2Atom(), ValueAtom(1))),
                [ValueAtom(2)])

def X2Atom():
    return G(X2())

class X2(OpGroundedAtom):

    def __init__(self):
        super().__init__()

    def execute(self, arg):
        return [ValueAtom(2 * arg.get_object().value)]

    def __str__(self):
        return "*2"
