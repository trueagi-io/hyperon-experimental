import unittest
import re

from hyperon import *
from common import MeTTa

class UnificationTest(unittest.TestCase):

    def test_factorial_via_unification(self):
        metta = MeTTa()
        metta.add_parse('''
            (= (if True $then $else) $then)
            (= (if False $then $else) $else)
            (= (fact $n) (if (== $n 0) 1 (* (fact (- $n 1)) $n)))
        ''')
        result = metta.interpret('(fact 5)')

        self.assertEqual(result, [ValueAtom(120)])
