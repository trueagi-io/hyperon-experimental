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

    def test_meta_types(self):
        metta = MeTTa()
        ### Basic functional types
        metta.add_atom(r"id_num", OperationAtom("id_num", lambda x: x, ['Number', 'Number']))
        metta.add_atom(r"as_int", OperationAtom("as_int", lambda x: x, ['Number', 'Int']))
        v1 = metta.interpret("(id_num (+ 2 2))")
        v2 = metta.interpret("4")
        v3 = metta.interpret("(as_int (+ 2 2))")
        self.assertEqual(v1, v2)
        self.assertEqual(v1[0].get_grounded_type(), v2[0].get_grounded_type())
        self.assertNotEqual(v1[0].get_grounded_type(), v3[0].get_grounded_type())
        # Untyped symbols don't cause type error, but the expression is not reduced
        self.assertEqual(metta.interpret("(id_num untyp)"), metta.parse_all("(id_num untyp)"))
        # Typed symbols cause empty results due to type mismatch
        metta.add_parse("(: myAtom myType)")
        self.assertEqual(metta.interpret("(id_num myAtom)"), [])
        self.assertEqual(metta.interpret("(id_num False)"), [])
        ### Grounded functions over Atom
        ### (should use unwrap=False to deal with non-grounded atoms)
        # All grounded and ungrounded, typed and untyped symbols should be processed
        metta.add_atom(r"id_atom", OperationAtom("id_atom", lambda x: [x], ['Atom', 'Atom'], unwrap=False))
        self.assertEqual(metta.interpret("(id_atom 1)"), metta.parse_all("1"))
        self.assertEqual(metta.interpret("(id_atom myAtom)"), metta.parse_all("myAtom"))
        self.assertEqual(metta.interpret("(id_atom untyp)"), metta.parse_all("untyp"))
        # FIXME: why does it get reduced?
        # self.assertEqual(metta.interpret("(id_atom (+ 1 1))"), metta.parse_all("(+ 1 1)"))
        ### Polymorphic without unwrapping
        # Nothing is done with `$t` on the Grounded side, but we check that:
        # - the argument has really a variable type
        # - the interpreter doesn't reject to process this grounded function
        metta.add_atom(r"id_poly_w", OperationAtom("id_poly_w", lambda x: [x], ['$t', '$t'], unwrap=False))
        self.assertEqual(metta.interpret("(id_poly_w 1)"), metta.parse_all("1"))
        self.assertEqual(metta.interpret("(id_poly_w myAtom)"), metta.parse_all("myAtom"))
        self.assertEqual(metta.interpret("(id_poly_w untyp)"), metta.parse_all("untyp"))
        # TODO: doesn't work ATM
        # self.assertEqual(metta.interpret("(id_poly_w (+ 1 1))"), metta.parse_all("2"))
        # self.assertEqual(metta.interpret("(+ 1 (id_poly_w 2))"), metta.parse_all("3"))
        ### Polymorphic with unwrapping
        # TODO: automatic unwrapping of arguments of grounded polymorphic function
        #       is not supported on the Python side
        metta.add_atom(r"id_poly_u", OperationAtom("id_poly_u", lambda x: x, ['$t', '$t']))
        ### Undefined arguments
        # It is a bad idea to have an undefined result with automatic wrapping, but it's ok here
        metta.add_atom(r"id_undef", OperationAtom("id_undef", lambda x: x, ['%Undefined%', '%Undefined%']))
        self.assertEqual(metta.interpret("(id_undef 1)"), metta.parse_all("1"))
        self.assertEqual(metta.interpret("(id_undef False)"), metta.parse_all("False"))
        # This will not be reduced, because unwrapping expects a grounded atom
        self.assertEqual(metta.interpret("(id_undef myAtom)"), metta.parse_all("(id_undef myAtom)"))
        self.assertEqual(metta.interpret("(id_undef untyp)"), metta.parse_all("(id_undef untyp)"))
        # FIXME: why it is not reduced?
        # self.assertEqual(metta.interpret("(id_undef (+ 1 1))"), metta.parse_all("2"))

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

