import unittest

from hyperon import *
import hyperonpy as hp

class GroundedTypeTest(unittest.TestCase):

    def test_apply_type(self):
        metta = MeTTa(env_builder=Environment.test_env())
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
        metta.register_atom("untyped", ValueAtom(None))
        metta.register_atom("untop", OperationAtom("untop", lambda: None))
        self.assertEqual(
            metta.run("!(untop)")[0][0],
            metta.parse_single("()"))
        self.assertNotEqual(
            metta.run("!(> 1 1)")[0][0].get_grounded_type(),
            metta.run("!(+ 1 1)")[0][0].get_grounded_type())

    def test_higher_func(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.register_atom(r"plus", OperationAtom("plus", lambda a, b: a + b))
        metta.register_atom(
            r"curry_num",
            OperationAtom(
                "curry_num",
                lambda op, x: [OperationAtom(
                    'lmd',
                    lambda y: op.get_object().op(x.get_object().value, y),
                    ['Number', 'Number'])],
                [['Number', 'Number', 'Number'], 'Number', ['Number', 'Number']],
                unwrap=False))
        self.assertEqual(metta.run("!((curry_num plus 1) 2)"),
                         [[ValueAtom(3)]])

    def test_string_repr(self):
        metta = MeTTa(env_builder=Environment.test_env())
        self.assertEqual(metta.run('!(repr "A\n\tBB\x1b\u130A9")')[0][0].get_object(),
                         ValueObject("\"A\\n\\tBB\\x1b\u130A9\""))

    def test_meta_types(self):
        metta = MeTTa(env_builder=Environment.test_env())
        ### Basic functional types
        metta.register_atom(r"id_num", OperationAtom("id_num", lambda x: x, ['Number', 'Number']))
        metta.register_atom(r"as_int", OperationAtom("as_int", lambda x: x, ['Number', 'Number']))
        v1 = metta.run("!(id_num (+ 2 2))")[0][0]
        v2 = metta.run("! 4")[0][0]
        v3 = metta.run("!(as_int (+ 2 2))")[0][0]
        self.assertEqual(v1.get_object().value, v2.get_object().value)
        self.assertEqual(v1.get_grounded_type(), v2.get_grounded_type())
        # Python int is represented by the Rust Number grounded atom. Number is
        # implemented to have constant type "Number" which is common
        # implementation of the grounded atoms in Rust. Usually type is an
        # attribute of the atom and cannot be changed by reassigning. Thus one
        # cannot override type of the value by calling (-> Number Int)
        # function. It doesn't work for usual Rust grounded atoms.
        self.assertEqual(v1.get_grounded_type(), v3.get_grounded_type())
        # Untyped symbols don't cause type error, but the expression is not reduced
        self.assertEqual(metta.run("!(id_num untyp)"), [metta.parse_all("(id_num untyp)")])
        # Typed symbols cause type error when evaluated
        metta.run("(: myAtom myType)")
        self.assertEqual(metta.run('''
            !(id_num myAtom)
            !(id_num False)
            '''),
            [[E(S('Error'), S('myAtom'), S('BadType'))]])
        ### Grounded functions over Atom
        ### (should use unwrap=False to deal with non-grounded atoms)
        # All grounded and ungrounded, typed and untyped symbols should be processed
        metta.register_atom(r"id_atom", OperationAtom("id_atom",
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
        # FIXME: (id_atom (+ 1 1)) gets reduced because after id_atom returns
        # (+ 1 1) it is evaluated further and becomes 2. We don't have a manner
        # to prevent evaluation in MeTTa.
        # self.assertEqual(metta.run("!(id_atom (+ 1 1))"), [metta.parse_all("(+ 1 1)")])
        ### Polymorphic without unwrapping
        # Nothing is done with `$t` on the Grounded side, but we check that:
        # - the argument has really a variable type
        # - the interpreter doesn't reject to process this grounded function
        metta.register_atom(r"id_poly_w", OperationAtom("id_poly_w", lambda x: [x], ['$t', '$t'], unwrap=False))
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
        metta.register_atom(r"id_poly_u", OperationAtom("id_poly_u", lambda x: x, ['$t', '$t']))
        ### Undefined arguments
        # It is a bad idea to have an undefined result with automatic wrapping, but it's ok here
        metta.register_atom(r"id_undef", OperationAtom("id_undef", lambda x: x, [AtomType.UNDEFINED, AtomType.UNDEFINED]))
        self.assertEqual(metta.run('''
            !(id_undef 1)
            !(id_undef False)
            !(id_undef (+ 1 1))
            ''', flat=True),
            [ValueAtom(1), ValueAtom(False), ValueAtom(2)])
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
        metta = MeTTa(env_builder=Environment.test_env())
        metta.register_atom("untyped", ValueAtom(None))
        metta.register_atom("untop", OperationAtom("untop", lambda: None))
        self.assertNotEqual(metta.parse_single("untop").get_grounded_type(),
                metta.parse_single("untyped").get_grounded_type())

    def test_use_rust_value_in_python_grounded_function(self):
        # Test assumes default primitives parsed by MeTTa interpreter are Rust
        # and Python grounded functions return Python values without conversion
        metta = MeTTa(env_builder=Environment.test_env())

        metta.register_atom("python-int-func", OperationAtom("python-int-func", lambda x: x * 2))
        integer = metta.run('!(python-int-func 21)', flat=True)[0].get_object()
        self.assertEqual(integer, ValueObject(42))

        metta.register_atom("python-float-func", OperationAtom("python-float-func", lambda x: x / 2.0))
        float = metta.run('!(python-float-func 8.4)', flat=True)[0].get_object()
        self.assertEqual(float, ValueObject(4.2))

        metta.register_atom("python-bool-func", OperationAtom("python-bool-func", lambda x: not x))
        bool = metta.run('!(python-bool-func False)', flat=True)[0].get_object()
        self.assertTrue(bool.value)

        metta.register_atom("python-str-func", OperationAtom("python-str-func", lambda x: "test " + x))
        str = metta.run('!(python-str-func "string")', flat=True)[0].get_object()
        self.assertEqual(str, ValueObject("test string"))

    def test_use_python_value_in_rust_grounded_function(self):
        # Test assumes default primitives parsed by MeTTa interpreter are Rust
        # and Python grounded functions return Python values without conversion
        metta = MeTTa(env_builder=Environment.test_env())

        metta.register_atom("python-int", OperationAtom("python-int", lambda: 42))
        integer = metta.run('!(+ 1 (python-int))', flat=True)[0].get_object()
        self.assertEqual(integer, ValueObject(43))

        metta.register_atom("python-float", OperationAtom("python-float", lambda: 4.2))
        float = metta.run('!(+ 1.0 (python-float))', flat=True)[0].get_object()
        self.assertEqual(float, ValueObject(5.2))

        metta.register_atom("python-bool", OperationAtom("python-bool", lambda: True))
        print("result: ", metta.run('!(not (python-bool))', flat=True))
        bool = metta.run('!(not (python-bool))', flat=True)[0].get_object()
        self.assertEqual(bool, ValueObject(False))

        metta.register_atom("python-str", OperationAtom("python-str", lambda: "some string {}"))
        str = metta.run('!(format-args (python-str) (A))', flat=True)[0].get_object()
        self.assertEqual(str, ValueObject("some string A"))

    def test_match_python_value_with_rust_value_in_atomspace(self):
        # Test assumes default primitives parsed by MeTTa interpreter are Rust
        # and Python grounded functions return Python values without conversion
        metta = MeTTa(env_builder=Environment.test_env())

        metta.register_atom("python-int", OperationAtom("python-int", lambda: 42))
        metta.run("(= (match-int 42) ok)")
        result = metta.run('!(match-int (python-int))', flat=True)[0]
        self.assertEqual(result, S("ok"))

        metta.register_atom("python-float", OperationAtom("python-float", lambda: 4.2))
        metta.run("(= (match-float 4.2) ok)")
        result = metta.run('!(match-float (python-float))', flat=True)[0]
        self.assertEqual(result, S("ok"))

        metta.register_atom("python-bool", OperationAtom("python-bool", lambda: True))
        metta.run("(= (match-bool True) ok)")
        result = metta.run('!(match-bool (python-bool))', flat=True)[0]
        self.assertEqual(result, S("ok"))

        metta.register_atom("python-str", OperationAtom("python-str", lambda: "text"))
        metta.run("(= (match-str \"text\") ok)")
        result = metta.run('!(match-str (python-str))', flat=True)[0]
        self.assertEqual(result, S("ok"))

    def test_match_rust_value_with_python_value_in_atomspace(self):
        # Test assumes default primitives parsed by MeTTa interpreter are Rust
        # and Python grounded functions return Python values without conversion
        metta = MeTTa(env_builder=Environment.test_env())

        metta.register_atom("python-int", OperationAtom("python-int", lambda: 42))
        metta.run("!(let $x (python-int) (add-atom &self (= (match-int $x) ok)))")
        result = metta.run('!(match-int 42)', flat=True)[0]
        self.assertEqual(result, S("ok"))

        metta.register_atom("python-float", OperationAtom("python-float", lambda: 4.2))
        metta.run("!(let $x (python-float) (add-atom &self (= (match-float $x) ok)))")
        result = metta.run('!(match-float 4.2)', flat=True)[0]
        self.assertEqual(result, S("ok"))

        metta.register_atom("python-bool", OperationAtom("python-bool", lambda: True))
        metta.run("!(let $x (python-bool) (add-atom &self (= (match-bool $x) ok)))")
        result = metta.run('!(match-bool True)', flat=True)[0]
        self.assertEqual(result, S("ok"))

        metta.register_atom("python-str", OperationAtom("python-str", lambda: "text"))
        metta.run("!(let $x (python-str) (add-atom &self (= (match-str $x) ok)))")
        result = metta.run('!(match-str "text")', flat=True)[0]
        self.assertEqual(result, S("ok"))

    def test_grounded_override(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.register_atom(r'\+', OperationAtom('+', lambda a, b: a + b))
        atom = metta.parse_single('+')
        self.assertEqual(type(atom.get_object()), OperationObject)

    def test_assert_grounded_value_type(self):
        with self.assertRaises(AssertionError) as cm:
            ValueAtom(42, "Int")
        msg = str(cm.exception)
        self.assertTrue(msg.startswith("Grounded int 42 can't have a custom type Int"), f"Unexpected message \"{msg}\"")
        with self.assertRaises(AssertionError) as cm:
            ValueAtom(4.2, "Float")
        msg = str(cm.exception)
        self.assertTrue(msg.startswith("Grounded float 4.2 can't have a custom type Float"), f"Unexpected message \"{msg}\"")
        with self.assertRaises(AssertionError) as cm:
            ValueAtom(True, "bool")
        msg = str(cm.exception)
        self.assertTrue(msg.startswith("Grounded bool True can't have a custom type bool"), f"Unexpected message \"{msg}\"")

if __name__ == "__main__":
    unittest.main()
