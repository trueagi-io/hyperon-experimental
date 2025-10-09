import unittest

from hyperon import *
from hyperon.stdlib import Char
from test_common import *

class SimpleObject:
    def method(self, *args, **kwargs):
        # Expecting specific arguments for testing purposes
        return -1 if "arg2" not in args else kwargs["arg3"]

def proc_atom_noreduce(a: Atom):
    # Checking that the input atom is an expression
    # Reducible expression like (+ 1 2) should be passed here for check
    return [ValueAtom(isinstance(a, ExpressionAtom))]

class StdlibTest(HyperonTestCase):
    def test_text_ops(self):
        metta = MeTTa(env_builder=Environment.test_env())
        # Check that (repr (my atom)) == "(my atom)"
        self.assertEqualMettaRunnerResults(metta.run("!(repr (my atom))"),
                                           [[ValueAtom("(my atom)")]])

        # Check that (parse "(my atom)") == (my atom)
        self.assertEqualMettaRunnerResults(metta.run("!(parse \"(my atom)\")"),
                                           [[E(S("my"), S("atom"))]])

        #unstable renaming of variables causes random failures of the test
        #self.assertEqualMettaRunnerResults(metta.run('!(parse "$X")'),
        #                                  [[(V("X"))]])

        self.assertEqualMettaRunnerResults(metta.run('!(parse "\\"A\\"")'),
                                           metta.run('! "A"'))

        #self.assertEqualMettaRunnerResults(metta.run('!(parse "(func (Cons $x (Cons $xs $xss))) ")'),
        #                                   [[E(S("func"), E(S("Cons"), V("x"), E(S("Cons"), V("xs"), V("xss"))))]])

        self.assertEqualMettaRunnerResults(metta.run('!(parse "(A 2 \'S\')")'),
                                           metta.run("!(A 2 'S')"))

        # Check that (stringToChars "ABC") == ('A' 'B' 'C')
        # NOTE: `parse` is a Rust function, so we compare to metta.run
        #       while `stringToChars`` is a Python function, so we construct Python atoms
        self.assertEqualMettaRunnerResults(metta.run('!(stringToChars "ABC")'),
                                           [[E(ValueAtom(Char("A")), ValueAtom(Char("B")), ValueAtom(Char("C")))]])

        # Check that (charsToString ('A' 'B' 'C')) == "ABC"
        self.assertEqualMettaRunnerResults(metta.run("!(charsToString ('A' 'B' 'C'))"),
                                           [[ValueAtom("ABC")]])

    def test_py_atoms(self):
        metta = MeTTa(env_builder=Environment.test_env())
        # Basic tests
        # py-atom can give as both a module and a function in the module
        # py-dot works for the module returned by py-atom
        metta.run('''
                  ! (bind! &pow (py-atom math.pow (-> Number Number Number)))
                  ! (bind! &math (py-atom math))
                  ! (bind! &statistics (py-atom statistics))
        ''')
        self.assertEqual(metta.run('! (&pow 2 3)'),
                         metta.run('! ((py-dot &math pow) 2 3)'))
        self.assertEqual([[ValueAtom(3)]],
            metta.run('! ((py-dot &math floor) (py-dot &math pi))'))
        # Additional tests
        # py-atom can actually execute some Python code
        # py-dot works for dicts (and other classes) - not only modules
        self.assertEqual([[ValueAtom('f')]],
                         metta.run('! ((py-dot (py-atom "{5: \'f\'}") get) 5)'))
        # py-atom can return some built-in functions without modules
        self.assertEqual([[ValueAtom('5')]],
                         metta.run('! ((py-atom str) 5)'))
        # This test was rewritten due to its non-multiplatform behavior.
        self.assertEqual([[ValueAtom(3.5)]],
            metta.run('''
                ! ((py-dot &statistics mean)
                   (py-atom "[5, 2]"))
        '''))
        # Checking that we can import custom classes, create objects and access their methods
        metta.run('!(bind! &SimpleObject (py-atom test_stdlib.SimpleObject))')
        self.assertEqual([[ValueAtom(3)]],
            metta.run('!((py-dot (&SimpleObject) method) "arg1" "arg2" (Kwargs (arg3 3)))'))

    def test_py_atom_unwrap(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run('!(bind! &math (py-atom math))')
        self.assertTrue(atom_is_error(metta.run('!((py-dot &math pow False) 5 3)', True)[0]))
        self.assertEqual(metta.run('!((py-dot &math pow True) 5 3)', True)[0], ValueAtom(125.0))
        self.assertEqual(metta.run('!((py-dot &math pow True) 5 3)', True)[0], ValueAtom(125.0))
        self.assertEqual(metta.run('!((py-dot &math pow (-> Number Number Number)) 5 3)', True)[0], ValueAtom(125.0))
        self.assertEqual(metta.run('!((py-dot &math pow) 5 3)', True)[0], ValueAtom(125.0))
        # Here, we check that the Atom argument of unwrap=False function will not be reduced
        # NOTE: this works with bind!, but doesn't work with `(let $noreduce (py-atom ...) ...)`
        #       (+ 1 2) gets reduced before getting to proc_atom_noreduce...
        metta.run('!(bind! &noreduce (py-atom test_stdlib.proc_atom_noreduce (-> Atom Atom) False))')
        self.assertEqual(metta.run('!(&noreduce (+ 1 2))'),
                         [[ValueAtom(True)]])

    def test_number_parsing(self):
        metta = MeTTa(env_builder=Environment.test_env())
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1 2)"), metta.run("! 3"))
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5.0 -2.0)"), metta.run("! 3.0"))
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1.0e3 2.0e3)"), metta.run("! 3e3"))
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5e-3 -2e-3)"), metta.run("! 3e-3"))

    def test_string_parsing(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run("(= (id' $x) $x)")
        self.assertEqual(metta.run("!(id' \"test\")")[0][0].get_object().value, "test")
        self.assertEqual(metta.run("!(id' \"te st\")")[0][0].get_object().value, "te st")
        self.assertEqual(metta.run("!(id' \"te\\\"st\")")[0][0].get_object().value, "te\"st")
        self.assertEqual(metta.run("!(id' \"\")")[0][0].get_object().value, "")
        self.assertEqual(metta.run("!(id' \"te\\nst\")")[0][0].get_object().value, "te\nst")

    def test_regex(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run('''(= (intent regex:"^Hello[[\.|!]]?$") (Intent Hello))
                    (= (intent regex:"Good~morning.*[[\\.|!]]?") (Intent Hello))
                    (= (intent $x) (empty))''')
        self.assertEqual(metta.run('!(intent "hello")', True), [E(S("Intent"), S("Hello"))])

        self.assertEqual(metta.run('!(intent "Good morning my friend!")', True),
                                           [E(S("Intent"), S("Hello"))])

        self.assertEqual(metta.run('!(intent "Hi")', True), [])

    def test_py_list_tuple(self):
        metta = MeTTa(env_builder=Environment.test_env())
        self.assertEqual(metta.run('!(py-list ())'), [[ValueAtom( [] )]])
        self.assertEqual(metta.run('!(py-tuple ())'), [[ValueAtom( () )]])
        self.assertEqual(metta.run('!(py-dict ())'), [[ValueAtom( {} )]])
        self.assertEqual(metta.run('!(py-tuple (1 (2 (3 "3")) (py-atom list)))'), [[ValueAtom((1,(2,(3, "3")), list))]])
        self.assertEqual(metta.run('!(py-list (1 2 (4.5 3)))'), [[ValueAtom( [1,2,[4.5,3]] )]])
        self.assertEqual(metta.run('!(py-list (1 2 (py-tuple (3 4))))'), [[ValueAtom( [1,2, (3,4)] )]])

        self.assertEqual(metta.run('!(py-dict ((a "b") ("b" "c")))'), [[ValueAtom( {"a":"b", "b":"c"} )]])

        self.assertEqual(str(metta.run('!(py-list (a b c))')[0][0].get_object().content[2]), "c")

        # We need py-chain for langchain, e.g.
        #    !(bind! ChatOpenAI (py-atom langchain_openai.ChatOpenAI))
        #    !(bind! ChatPromptTemplate (py-atom langchain_core.prompts.ChatPromptTemplate))
        #    !(bind! StrOutputParser (py-atom langchain_core.output_parsers.StrOutputParser))
        #    !(bind! model (ChatOpenAI (Kwargs (temperature 0) (model "gpt-3.5-turbo"))))
        #    !(bind! prompt ((py-dot ChatPromptTemplate from_template) "tell me a joke about cat"))
        #    !((py-dot (py-chain (prompt model (StrOutputParser))) invoke) (py-dict ()))
        # but we test with bitwise operation | (1 | 2 | 3 | 4 = 7)
        self.assertEqual(metta.run('!(py-chain (1 2 3 4))'), [[ValueAtom( 7 )]])

        # test when we except errors (just in case we reset metta after each exception)
        self.assertRaises(Exception, metta.run('!(py-dict (("a" "b" "c") ("b" "c")))'))
        metta = MeTTa(env_builder=Environment.test_env())

        self.assertRaises(Exception, metta.run('!(py-dict (("a") ("b" "c")))'))
        metta = MeTTa(env_builder=Environment.test_env())

        self.assertRaises(Exception, metta.run('!(py-dict ("a" "b") ("b" "c"))'))
        metta = MeTTa(env_builder=Environment.test_env())

        self.assertRaises(Exception, metta.run('!(py-list 1 2)'))
        metta = MeTTa(env_builder=Environment.test_env())

        self.assertRaises(Exception, metta.run('!(py-list 1)'))
        metta = MeTTa(env_builder=Environment.test_env())

if __name__ == "__main__":
    unittest.main()
