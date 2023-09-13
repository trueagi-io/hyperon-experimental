import unittest

from hyperon import *
from hyperon.stdlib import Char
from test_common import *

class StdlibTest(HyperonTestCase):
    def test_text_ops(self):
        metta = MeTTa()
        # Check that (repr (my atom)) == "(my atom)"
        self.assertEqualMettaRunnerResults(metta.run("!(repr (my atom))"),
                                           [[ValueAtom("(my atom)")]])

        # Check that (parse "(my atom)") == (my atom)
        self.assertEqualMettaRunnerResults(metta.run("!(parse \"(my atom)\")"),
                                           [[ValueAtom(E(S("my"), S("atom")))]])

        # Check that (stringToChars "ABC") == ('A' 'B' 'C')
        self.assertEqualMettaRunnerResults(metta.run("!(stringToChars \"ABC\")"),
                                           [[ValueAtom(E(ValueAtom(Char("A")), ValueAtom(Char("B")), ValueAtom(Char("C"))))]])

        # Check that (charsToString ('A' 'B' 'C')) == "ABC"
        self.assertEqualMettaRunnerResults(metta.run("!(charsToString ('A' 'B' 'C'))"),
                                           [[ValueAtom("ABC")]])

    def test_number_parsing(self):
        metta = MeTTa()
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1 2)"), [[ValueAtom(3)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5.0 -2.0)"), [[ValueAtom(3.0)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1.0e3 2.0e3)"), [[ValueAtom(3e3)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5e-3 -2e-3)"), [[ValueAtom(3e-3)]])


if __name__ == "__main__":
    unittest.main()
