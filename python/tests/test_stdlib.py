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

    def test_number_parsing(self):
        metta = MeTTa(env_builder=Environment.test_env())
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1 2)"), [[ValueAtom(3)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5.0 -2.0)"), [[ValueAtom(3.0)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1.0e3 2.0e3)"), [[ValueAtom(3e3)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5e-3 -2e-3)"), [[ValueAtom(3e-3)]])

    def test_regex(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run('''(= (intent regex:"^Hello[[\.|!]]?$") (Intent Hello))
                    (= (intent regex:"Good~morning.*[[\\.|!]]?") (Intent Hello))
                    (= (intent $x) (empty))''')
        self.assertEqual(metta.run('!(intent "hello")', True), [E(S("Intent"), S("Hello"))])

        self.assertEqual(metta.run('!(intent "Good morning my friend!")', True),
                                           [E(S("Intent"), S("Hello"))])

        self.assertEqual(metta.run('!(intent "Hi")', True), [])


if __name__ == "__main__":
    unittest.main()
