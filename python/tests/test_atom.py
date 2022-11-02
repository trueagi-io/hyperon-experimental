import unittest

from hyperon import *

class AtomTest(unittest.TestCase):

    def test_symbol_equals(self):
        self.assertEqual(S("a"), S("a"))
        self.assertNotEqual(S("a"), S("b"))

    def test_symbol_str(self):
        self.assertEqual(str(S("a")), "a")

    def test_symbol_type(self):
        self.assertEqual(S("a").get_type(), AtomKind.SYMBOL)

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
        self.assertEqual(V("x").get_type(), AtomKind.VARIABLE)

    def test_variable_get_name(self):
        self.assertEqual(V("x").get_name(), "x")

    def test_grounded_equals(self):
        self.assertEqual(ValueAtom(1.0), ValueAtom(1.0))
        self.assertNotEqual(ValueAtom(1.0), ValueAtom(2.0))

    def test_grounded_str(self):
        self.assertEqual(str(ValueAtom(1.0)), "1.0")

    def test_grounded_type(self):
        self.assertEqual(ValueAtom(1.0).get_type(), AtomKind.GROUNDED)

    def test_grounded_grounded_type(self):
        atom = G(GroundedObject(None), S("Float"))
        self.assertEqual(atom.get_grounded_type(), S("Float"))

    # def test_grounded_execute_default(self):
        # self.assertEqual(ValueAtom(1.0).get_object().execute(VecAtom(),
            # VecAtom()), "1.0 is not executable")

    def test_grounded_execute(self):
        res = x2Atom.get_object().execute(ValueAtom(1.0))
        self.assertEqual(res, [ValueAtom(2.0)])

    def test_expr_equals(self):
        self.assertEqual(E(S("+"), S("1"), S("2")),
                E(S("+"), S("1"), S("2")))

    def test_expr_equals_grounded(self):
        self.assertEqual(E(x2Atom, ValueAtom(1.0)),
                E(x2Atom, ValueAtom(1.0)))

    def test_expr_str(self):
        self.assertEqual(str(E(x2Atom, ValueAtom(1.0))), "(*2 1.0)")

    def test_expr_type(self):
        self.assertEqual(E(x2Atom, ValueAtom(1.0)).get_type(), AtomKind.EXPR)

    def test_expr_get_children(self):
        self.assertEqual(E(x2Atom, ValueAtom(1.0)).get_children(),
                [x2Atom, ValueAtom(1.0)])

    def test_groundingspace_equals(self):
        kb_a = GroundingSpace()
        kb_a.add_atom(E(S("+"), S("1"), S("2")))
        kb_b = GroundingSpace()
        kb_b.add_atom(E(S("+"), S("1"), S("2")))
        kb_c = kb_a
        self.assertEqual(kb_a.get_atoms(), kb_b.get_atoms())
        self.assertEqual(kb_a, kb_c)
        self.assertNotEqual(kb_a, kb_b)

    def test_interpret(self):
        space = GroundingSpace()
        self.assertEqual(interpret(space, E(x2Atom, ValueAtom(1))),
                [ValueAtom(2)])

    def test_plan(self):
        space = GroundingSpace()
        interpreter = Interpreter(space, E(x2Atom, ValueAtom(1)))
        self.assertEqual(str(interpreter.get_step_result()),
                "return [(-> int int)] then form alternative plans for expression (*2 1) using types")

    def test_no_reduce(self):
        space = GroundingSpace()
        self.assertEqual(interpret(space, E(noReduceAtom, ValueAtom(1))),
                [E(noReduceAtom, ValueAtom(1))])

# No unwrap
def x2_op(atom):
    return [ValueAtom(2 * atom.get_object().value)]
x2Atom = OperationAtom('*2', x2_op, type_names=["int", "int"], unwrap=False)

def no_reduce_op(atom):
    raise NoReduceError()
noReduceAtom = OperationAtom('no-reduce', no_reduce_op, unwrap=False)
