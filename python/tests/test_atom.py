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
        self.assertEqual(str(ValueAtom("1.0")), '"1.0"')

    def test_grounded_type(self):
        self.assertEqual(ValueAtom(1.0).get_type(), AtomKind.GROUNDED)

    def test_grounded_grounded_type(self):
        atom = G(GroundedObject(None), S("Float"))
        self.assertEqual(atom.get_grounded_type(), S("Float"))

    def test_grounded_no_copy(self):
        with self.assertRaises(AssertionError) as context:
            atom = G(GroundedNoCopy(), S("GroundedNoCopy"))
        self.assertEqual("Method copy should be implemented by grounded object",
                        str(context.exception))

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
        kb_a = GroundingSpaceRef()
        kb_a.add_atom(E(S("+"), S("1"), S("2")))
        kb_b = GroundingSpaceRef()
        kb_b.add_atom(E(S("+"), S("1"), S("2")))
        kb_c = kb_a
        self.assertEqual(kb_a.get_atoms(), kb_b.get_atoms())
        self.assertEqual(kb_a, kb_c)
        self.assertNotEqual(kb_a, kb_b)

    def test_interpret(self):
        space = GroundingSpaceRef()
        self.assertEqual(interpret(space, E(x2Atom, ValueAtom(1))),
                [ValueAtom(2)])

    def test_grounded_returns_python_value_unwrap_false(self):
        def x2_op(atom):
            return [2 * atom.get_object().value]
        x2Atom = OperationAtom('*2', x2_op, type_names=["int", "int"], unwrap=False)
        expr = E(x2Atom, ValueAtom(1))

        space = GroundingSpaceRef()
        self.assertEqual(interpret(space, expr),
                [E(S('Error'), expr, S('Grounded operation which is defined using unwrap=False should return atom instead of Python type'))])

    def test_plan(self):
        space = GroundingSpaceRef()
        interpreter = Interpreter(space, E(x2Atom, ValueAtom(1)))
        self.assertEqual(str(interpreter.get_step_result()),
                "return [(-> int int)] then form alternative plans for expression (*2 1) using types")

    def test_no_reduce(self):
        space = GroundingSpaceRef()
        self.assertEqual(interpret(space, E(noReduceAtom, ValueAtom(1))),
                [E(noReduceAtom, ValueAtom(1))])

    def test_match_(self):
        space = GroundingSpaceRef()
        match_atom = MatchableAtomTest(S("MatchableAtom"), type_name=None, atom_id=None)
        space.add_atom(match_atom)
        result = space.query(E(S('symbol_atom'), V('atom_type')))
        self.assertEqual(AtomKind.SYMBOL.name, str(result[0]['atom_type']))
        result = space.query(E(E(S("+"), S("1"), S("2")), V('atom_type')))
        self.assertEqual(AtomKind.EXPR.name, str(result[0]['atom_type']))
        atom = E(G(GroundedObject(None), S("Float")), V('atom_type'))
        result = space.query(atom)
        self.assertEqual(AtomKind.GROUNDED.name, str(result[0]['atom_type']))
        result = space.query(V("Z"))
        self.assertEqual(S("MatchableAtom").get_name(), str(result[0]['Z']))

# No unwrap
def x2_op(atom):
    return [ValueAtom(2 * atom.get_object().value)]
x2Atom = OperationAtom('*2', x2_op, type_names=["int", "int"], unwrap=False)

def no_reduce_op(atom):
    raise NoReduceError()
noReduceAtom = OperationAtom('no-reduce', no_reduce_op, unwrap=False)

class GroundedNoCopy:
    pass

class MatchableObjectTest(MatchableObject):
    def match_(self, atom):
        return [{'atom_type': S(atom.get_children()[0].get_type().name)}]

def MatchableAtomTest(value, type_name=None, atom_id=None):
    return G(MatchableObjectTest(value, atom_id), AtomType.UNDEFINED)

if __name__ == "__main__":
    unittest.main()
