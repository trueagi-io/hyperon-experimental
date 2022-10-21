import unittest

from hyperon import *
from common import MeTTa

class PLNTVTest(unittest.TestCase):

    def test_fuzzy_conjunction_fn(self):
        metta = MeTTa()
        # `stv` as a mix of function and constructor
        # working through ordinary equalities
        metta.run('''
                (= (if True $then $else) $then)
                (= (if False $then $else) $else)
                (= (min $a $b) (if (< $a $b) $a $b))
                (= (s-tv (stv $s $c)) $s)
                (= (c-tv (stv $s $c)) $c)
                (= (stv (And $a $b))
                   (stv (min (s-tv (stv $a)) (s-tv (stv $b)))
                        (min (c-tv (stv $a)) (c-tv (stv $b)))))
                (= (stv (P A)) (stv 0.5 0.8))
                (= (stv (P B)) (stv 0.3 0.9))
        ''')
        # JIC, if somebody wants "type annotations"
        metta.run('''
                (: A Concept)
                (: B Concept)
                (: P Predicate)
        ''')
        self.assertEqual(
            metta.run('!(stv (And (P A) (P B)))'),
            [metta.parse_all('(stv 0.3 0.8)')])
        metta.run('''
                (= (pln $expr) ($expr (stv $expr)))
        ''')
        # Here, we successfully retrieve $x. It doesn't work for "declarative" .tv
        # in c3_pln_stv.metta , but "functional" stv cannot process implications
        # without `match` (see also b2_backchain.metta)`
        # (would actually count (stv (P A)) twice for probabilistic version)
        self.assertEqual(
            metta.run('!(pln (And (P A) (P $x)))'),
            [metta.parse_all('''
                ((And (P A) (P A)) (stv 0.5 0.8))
                ((And (P A) (P B)) (stv 0.3 0.8))
            ''')])


if __name__ == "__main__":
    unittest.main()
