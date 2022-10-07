import unittest
import re

from hyperon import *
from common import MeTTa

class UnificationTest(unittest.TestCase):

    def test_factorial_via_unification(self):
        metta = MeTTa()
        metta.run('''
            (: if (-> Bool Atom Atom Atom))
            (= (if True $then $else) $then)
            (= (if False $then $else) $else)
            (= (fact $n) (if (== $n 0) 1 (* (fact (- $n 1)) $n)))
        ''')
        self.assertEqual(metta.run('!(fact 5)'), [[ValueAtom(120)]])
