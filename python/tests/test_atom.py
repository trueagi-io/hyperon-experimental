import unittest

from hyperon import *

class AtomTest(unittest.TestCase):

    def test_symbol_equals(self):
        self.assertEqual(S("a"), S("a"))
        self.assertNotEqual(S("a"), S("b"))

    def test_symbol_str(self):
        self.assertEqual(str(S("a")), "a")

    def test_variable_equals(self):
        self.assertEqual(V("x"), V("x"))
        self.assertNotEqual(V("x"), V("y"))

    def test_variable_str(self):
        self.assertEqual(str(V("x")), "$x")

    def test_expr_equals(self):
        self.assertEqual(E(S("+"), S("1"), S("2")),
                E(S("+"), S("1"), S("2")))

    def test_expr_str(self):
        self.assertEqual(str(E(S("+"), S("1"), S("2"))), "(+ 1 2)")

