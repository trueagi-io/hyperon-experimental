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
            metta.run("!(+ (* 1 4) 2)")[0][0].get_grounded_type(),
            metta.parse_single("0").get_grounded_type())
        self.assertEqual(
            metta.run("!(or True False)")[0][0].get_grounded_type(),
            metta.parse_single("False").get_grounded_type())
        self.assertEqual(
            metta.run("!(> (* 2 2) 1)")[0][0].get_grounded_type(),
            metta.run("!(or True True)")[0][0].get_grounded_type())
        metta.add_atom("untyped", ValueAtom(None))
        metta.add_atom("untop", OperationAtom("untop", lambda: None))
        self.assertEqual(
            metta.run("!(untop)")[0][0].get_grounded_type(),
            metta.parse_single("untyped").get_grounded_type())
        self.assertNotEqual(
            metta.run("!(untop)")[0][0].get_grounded_type(),
            metta.run("!(+ 1 1)")[0][0].get_grounded_type())
        self.assertNotEqual(
            metta.run("!(> 1 1)")[0][0].get_grounded_type(),
            metta.run("!(+ 1 1)")[0][0].get_grounded_type())

    def test_higher_func(self):
        metta = MeTTa()
        metta.add_atom(
            r"curry_num",
            OperationAtom(
                "curry_num",
                lambda op, x: [OperationAtom(
                    'lmd',
                    lambda y: op.get_object().op(x.get_object().value, y),
                    ['Number', 'Number'])],
                #FIXME: interpreter refuses to execute typed curry_num
                #[['Number', 'Number', 'Number'], 'Number', ['Number', 'Number']],
                unwrap=False))
        self.assertEqual(metta.run("!((curry_num + 1) 2)"),
                         metta.run("! 3"))

    def test_meta_types(self):
        metta = MeTTa()
        ### Basic functional types
        metta.add_atom(r"id_num", OperationAtom("id_num", lambda x: x, ['Number', 'Number']))
        metta.add_atom(r"as_int", OperationAtom("as_int", lambda x: x, ['Number', 'Int']))
        v1 = metta.run("!(id_num (+ 2 2))")[0]
        v2 = metta.run("! 4")[0]
        v3 = metta.run("!(as_int (+ 2 2))")[0]
        self.assertEqual(v1, v2)
        self.assertEqual(v1[0].get_grounded_type(), v2[0].get_grounded_type())
        self.assertNotEqual(v1[0].get_grounded_type(), v3[0].get_grounded_type())
        # Untyped symbols don't cause type error, but the expression is not reduced
        self.assertEqual(metta.run("!(id_num untyp)"), [metta.parse_all("(id_num untyp)")])
        # Typed symbols cause type error when evaluated
        metta.run("(: myAtom myType)")
        self.assertEqual(metta.run('''
            !(id_num myAtom)
            !(id_num False)
            '''),
            [[E(S('Error'), S('myAtom'), S('BadType'))],
             [E(S('Error'), ValueAtom(False, 'Bool'), S('BadType'))]])
        ### Grounded functions over Atom
        ### (should use unwrap=False to deal with non-grounded atoms)
        # All grounded and ungrounded, typed and untyped symbols should be processed
        metta.add_atom(r"id_atom", OperationAtom("id_atom",
            lambda x: [x], [AtomType.ATOM, AtomType.ATOM], unwrap=False))
        self.assertEqual(metta.run('''
            !(id_atom 1)
            !(id_atom myAtom)
            !(id_atom untyp)
            ''', flat=True),
            metta.parse_all('''
            1
            myAtom
            untyp
            '''))
        # FIXME: why does it get reduced?
        # self.assertEqual(metta.run("!(id_atom (+ 1 1))"), [metta.parse_all("(+ 1 1)")])
        ### Polymorphic without unwrapping
        # Nothing is done with `$t` on the Grounded side, but we check that:
        # - the argument has really a variable type
        # - the interpreter doesn't reject to process this grounded function
        metta.add_atom(r"id_poly_w", OperationAtom("id_poly_w", lambda x: [x], ['$t', '$t'], unwrap=False))
        self.assertEqual(metta.run('''
            !(id_poly_w 1)
            !(id_poly_w myAtom)
            !(id_poly_w untyp)
            !(id_poly_w (+ 1 1))
            !(+ 1 (id_poly_w 2))
            ''', flat=True),
            metta.parse_all('''
             1
             myAtom
             untyp
             2
             3
            ''')
        )
        ### Polymorphic with unwrapping
        # TODO: automatic unwrapping of arguments of grounded polymorphic function
        #       is not supported on the Python side
        metta.add_atom(r"id_poly_u", OperationAtom("id_poly_u", lambda x: x, ['$t', '$t']))
        ### Undefined arguments
        # It is a bad idea to have an undefined result with automatic wrapping, but it's ok here
        metta.add_atom(r"id_undef", OperationAtom("id_undef", lambda x: x, [AtomType.UNDEFINED, AtomType.UNDEFINED]))
        self.assertEqual(metta.run('''
            !(id_undef 1)
            !(id_undef False)
            !(id_undef (+ 1 1))
            ''', flat=True),
            metta.parse_all("1 False 2"))
        # This will not be reduced, because unwrapping expects a grounded atom
        self.assertEqual(metta.run('''
            !(id_undef myAtom)
            !(id_undef untyp)
            ''', flat=True),
            metta.parse_all('''
             (id_undef myAtom)
             (id_undef untyp)
            ''')
        )

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

