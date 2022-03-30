import unittest

from hyperon import *
from common import MeTTa

class GroundedTypeTest(unittest.TestCase):

    def test_apply_type(self):
        metta = MeTTa()
        self.assertEqual(
            metta.parse_single("+").get_object().atype,
            metta.parse_single("*").get_object().atype)
        self.assertEqual(
            metta.interpret("(+ (* 1 4) 2)")[0].get_object().atype,
            metta.parse_single("0").get_object().atype)
        self.assertEqual(
            metta.interpret("(or True False)")[0].get_object().atype,
            metta.parse_single("False").get_object().atype)
        self.assertEqual(
            metta.interpret("(> (* 2 2) 1)")[0].get_object().atype,
            metta.interpret("(or True True)")[0].get_object().atype)
        metta.add_atom("untyped", ValueAtom(None))
        metta.add_atom("untop", OperationAtom("untop", lambda: None))
        self.assertEqual(
            metta.interpret("(untop)")[0].get_object().atype,
            metta.parse_single("untyped").get_object().atype)
        self.assertNotEqual(
            metta.parse_single("untop").get_object().atype,
            metta.parse_single("untyped").get_object().atype)
        self.assertNotEqual(
            metta.interpret("(untop)")[0].get_object().atype,
            metta.interpret("(+ 1 1)")[0].get_object().atype)
        self.assertNotEqual(
            metta.interpret("(> 1 1)")[0].get_object().atype,
            metta.interpret("(+ 1 1)")[0].get_object().atype)

