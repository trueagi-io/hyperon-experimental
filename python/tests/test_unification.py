import unittest
import re

from hyperon import *
from common import interpret_until_result, Atomese

class UnificationTest(unittest.TestCase):

    def test_factorial_via_unification(self):
        atomese = Atomese()
        kb = atomese.parse('''
            (= (if True $then $else) $then)
            (= (if False $then $else) $else)
            (= (fact $n) (if (== $n 0) 1 (* (fact (- $n 1)) $n)))
        ''')
        target = atomese.parse_single('(fact 5)')
        result = interpret(kb, target)

        self.assertEqual(result, [ValueAtom(120)])
