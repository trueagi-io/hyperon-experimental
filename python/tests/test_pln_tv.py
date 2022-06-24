import unittest

from hyperon import *
from common import MeTTa

class PLNTVTest(unittest.TestCase):

    def test_fuzzy_conjunction_oc(self):
        metta = MeTTa()
        # Reproduce examples/pln/conjunction from github.com/opencog/pln
        # in OpenCog Classic style... kind of
        # .tv serves as a special type of equality that requires
        # `get-tv` "metarule" based on `match` to run
        # FIXME? `(PA)` and `(PB)` are used because otherwise
        # substitution wasn't invoked (atm of test creation)
        metta.add_parse('''
                (= (if True $then $else) $then)
                (= (if False $then $else) $else)
                (= (min $a $b) (if (< $a $b) $a $b))
                (= (s-tv (stv $s $c)) $s)
                (= (c-tv (stv $s $c)) $c)
                (= (get-tv $x)
                   (match &self (.tv $x $tv) $tv))
                (.tv (AndLink $a $b)
                     (stv (min (s-tv (get-tv $a)) (s-tv (get-tv $b)))
                          (min (c-tv (get-tv $a)) (c-tv (get-tv $b)))))
                (.tv (Evaluation (Predicate P) (Concept A))
                     (stv 0.5 0.8))
                (.tv (Evaluation (Predicate P) (Concept B))
                     (stv 0.3 0.9))
                (= (PA) (Evaluation (Predicate P) (Concept A)))
                (= (PB) (Evaluation (Predicate P) (Concept B)))
        ''')
        self.assertEqual(
            metta.interpret('(get-tv (AndLink (PA) (PB)))'),
            metta.parse_all('(stv 0.3 0.8)'))

        metta.add_parse('''
            (= (get-tv $x)
               (match &self (.tv (Implication $y $x) (stv $str $conf))
                            (stv (* (s-tv (get-tv $y)) $str)
                                 (* (c-tv (get-tv $y)) $conf))))
            (.tv (Implication (Evaluation (Predicate P) (Concept A))
                              (Evaluation (Predicate F) (Concept A)))
                 (stv 0.9 0.9))
        ''')
        self.assertEqual(
            metta.interpret('(get-tv (PA))'),
            metta.parse_all('(stv 0.5 0.8)'))

    def test_fuzzy_conjunction_fn(self):
        metta = MeTTa()
        # `stv` as a mix of function and constructor
        # working through ordinary equalities
        metta.add_parse('''
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
        metta.add_parse('''
                (: A Concept)
                (: B Concept)
                (: P Predicate)
        ''')
        self.assertEqual(
            metta.interpret('(stv (And (P A) (P B)))'),
            metta.parse_all('(stv 0.3 0.8)'))
        metta.add_parse('''
                (= (pln $expr) ($expr (stv $expr)))
        ''')
        # (would actually count (stv (P A)) twice for probabilistic version)
        self.assertEqual(
            metta.interpret('(pln (And (P A) (P $x)))'),
            metta.parse_all('''
                ((And (P A) (P A)) (stv 0.5 0.8))
                ((And (P A) (P B)) (stv 0.3 0.8))
            '''))


if __name__ == "__main__":
    unittest.main()
