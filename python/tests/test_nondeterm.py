import unittest

from hyperon import *
from common import MeTTa

class NondetermTest(unittest.TestCase):

    def test_collapse(self):
        metta = MeTTa()
        metta.run('''
            (= (f) a)
            (= (f) b)
        ''')
        self.assertEqual(metta.run("!(collapse (f))"),
                         [metta.parse_all("(a b)")])

    def test_superpose(self):
        metta = MeTTa()
        metta.run('''
            (= (f $x) (+ $x 1))
        ''')
        self.assertEqual(metta.run("!(f (superpose (1 2)))"),
                         [metta.parse_all("2 3")])

