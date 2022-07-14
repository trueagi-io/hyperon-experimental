import unittest

from hyperon import *
from common import MeTTa

class NondetermTest(unittest.TestCase):

    def test_collapse(self):
        metta = MeTTa()
        metta.add_parse('''
            (= (f) a)
            (= (f) b)
        ''')
        self.assertEqual(metta.interpret("(collapse (f))"),
                         metta.parse_all("(a b)"))

    def test_superpose(self):
        metta = MeTTa()
        metta.add_parse('''
            (= (f $x) (+ $x 1))
        ''')
        self.assertEqual(metta.interpret("(f (superpose (1 2)))"),
                         metta.parse_all("2 3"))

