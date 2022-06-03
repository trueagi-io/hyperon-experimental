import unittest

from hyperon import *
from common import MeTTa

class GroundedTypeTest(unittest.TestCase):

    def test_apply_type(self):
        metta = MeTTa()
        self.assertEqual(
            metta.parse_single("+").get_grounded_type(),
            metta.parse_single("*").get_grounded_type())
        self.assertEqual(
            metta.interpret("(+ (* 1 4) 2)")[0].get_grounded_type(),
            metta.parse_single("0").get_grounded_type())
        self.assertEqual(
            metta.interpret("(or True False)")[0].get_grounded_type(),
            metta.parse_single("False").get_grounded_type())
        self.assertEqual(
            metta.interpret("(> (* 2 2) 1)")[0].get_grounded_type(),
            metta.interpret("(or True True)")[0].get_grounded_type())
        metta.add_atom("untyped", ValueAtom(None))
        metta.add_atom("untop", OperationAtom("untop", lambda: None))
        self.assertEqual(
            metta.interpret("(untop)")[0].get_grounded_type(),
            metta.parse_single("untyped").get_grounded_type())
        self.assertNotEqual(
            metta.interpret("(untop)")[0].get_grounded_type(),
            metta.interpret("(+ 1 1)")[0].get_grounded_type())
        self.assertNotEqual(
            metta.interpret("(> 1 1)")[0].get_grounded_type(),
            metta.interpret("(+ 1 1)")[0].get_grounded_type())

    # TODO: For now OperationAtom without type has Undefined type. We discussed
    # that it would be nice to have something like *args in the future. The
    # type of untyped operations is yet another example showing this can be
    # useful. So, the type of untyped operations might become something like
    # (-> *Undefined Undefined) in the future.
    @unittest.skip("Behavior to be defined")
    def test_undefined_operation_type(self):
        metta = MeTTa()
        metta.add_atom("untyped", ValueAtom(None))
        metta.add_atom("untop", OperationAtom("untop", lambda: None))
        self.assertNotEqual(metta.parse_single("untop").get_grounded_type(),
                metta.parse_single("untyped").get_grounded_type())

