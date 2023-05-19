import unittest

from hyperon import *
from test_common import *

class StdlibTest(HyperonTestCase):
    def test_number_parsing(self):
        metta = MeTTa()
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1 2)"), [[ValueAtom(3)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5.0 -2.0)"), [[ValueAtom(3.0)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1.0e3 2.0e3)"), [[ValueAtom(3e3)]])
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5e-3 -2e-3)"), [[ValueAtom(3e-3)]])


if __name__ == "__main__":
    unittest.main()
