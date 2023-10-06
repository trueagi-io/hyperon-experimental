import unittest

from hyperon import *
from test_common import HyperonTestCase

class PLNTVTest(HyperonTestCase):

    def test_fuzzy_conjunction_fn(self):
        metta = MeTTa(env_builder=Environment.test_env())
        # `stv` as a mix of function and constructor
        # working through ordinary equalities
        metta.run('''
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
        self.assertEqualMettaRunnerResults(
            metta.run('!(stv (And (P A) (P B)))'),
            [metta.parse_all('(stv 0.3 0.8)')])
        metta.run('''
                (= (pln $expr) ($expr (stv $expr)))
        ''')
        # Here, we successfully retrieve $x. It doesn't work for "declarative" .tv
        # in c3_pln_stv.metta (e.g. `(pln (green $x))` will not substitute $x in the result`),
        # but "functional" stv cannot process implications without `match` (see also b2_backchain.metta)`
        # (would actually count (stv (P A)) twice for probabilistic version)
        self.assertEqualMettaRunnerResults(
            metta.run('!(pln (And (P A) (P $x)))'),
            [metta.parse_all('''
                ((And (P A) (P A)) (stv 0.5 0.8))
                ((And (P A) (P B)) (stv 0.3 0.8))
            ''')])


if __name__ == "__main__":
    unittest.main()
