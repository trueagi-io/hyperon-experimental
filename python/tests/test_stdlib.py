import unittest

from hyperon import *
from hyperon.stdlib import Char
from test_common import *

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
                                           [[(ValueAtom("A"))]])

        #self.assertEqualMettaRunnerResults(metta.run('!(parse "(func (Cons $x (Cons $xs $xss))) ")'),
        #                                   [[E(S("func"), E(S("Cons"), V("x"), E(S("Cons"), V("xs"), V("xss"))))]])

        self.assertEqualMettaRunnerResults(metta.run('!(parse "(A 2 \'S\')")'),
                                   [[E(S("A"), ValueAtom(2), ValueAtom(Char("S")))]])

        # Check that (stringToChars "ABC") == ('A' 'B' 'C')
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
        # Check that py-dot accepts a nested path as a second argument
        self.assertEqual([[ValueAtom("/usr")]],
            metta.run('''
                ! ((py-dot (py-atom os) path.commonpath)
                   (py-atom "['/usr/lib', '/usr/local/lib']"))
        '''))


    def test_number_parsing(self):
        metta = MeTTa(env_builder=Environment.test_env())
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1 2)"), [[ValueAtom(3)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5.0 -2.0)"), [[ValueAtom(3.0)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1.0e3 2.0e3)"), [[ValueAtom(3e3)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5e-3 -2e-3)"), [[ValueAtom(3e-3)]])

    def test_string_parsing(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run("(= (id' $x) $x)")
        self.assertEqualMettaRunnerResults(metta.run("!(id' \"test\")"), [[ValueAtom("test")]])
        self.assertEqualMettaRunnerResults(metta.run("!(id' \"te st\")"), [[ValueAtom("te st")]])
        self.assertEqualMettaRunnerResults(metta.run("!(id' \"te\\\"st\")"), [[ValueAtom("te\"st")]])
        self.assertEqualMettaRunnerResults(metta.run("!(id' \"\")"), [[ValueAtom("")]])
        self.assertEqualMettaRunnerResults(metta.run("!(id' \"te\\nst\")"), [[ValueAtom("te\nst")]])

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

        # We need py-chain for langchain, but we test with bitwise operation | (1 | 2 | 3 | 4 = 7)
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
