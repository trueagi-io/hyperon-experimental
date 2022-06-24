import unittest

from hyperon import *
from common import MeTTa

class HigherFuncTypeTest(unittest.TestCase):

    def test_curry(self):
        # NOTE: `curry` implementation without `lambda`
        metta = MeTTa()
        metta.add_parse('''
            (: curry (-> (-> $a $b $c) $a (-> $b $c)))
            (= ((curry $f $a) $b) ($f $a $b))
            (: Socrates Entity)
            (: Human Entity)
            (: True Bool)
            (: is (-> Entity Entity Bool))
            (= (is Socrates Human) True)
            (= (is-socrates) (curry is Socrates))
        ''')
        self.assertEqual(
            metta.interpret("(is-socrates)"),
            metta.parse_all("(curry is Socrates)"))
        self.assertEqual(
            metta.interpret("((curry is Socrates) Human)"),
            metta.interpret("(is Socrates Human)"))
        self.assertEqual(
            metta.interpret("((is-socrates) Human)"),
            metta.interpret("True"))

    def test_fmap(self):
        metta = MeTTa()
        # FIXME? We have to define `Nothing` as a functional constructor
        #        Otherwise, we'd have to define `(= (fmap $f $C0) $C0)`,
        #        and `$C0` would match with `(Something 2)`
        # NOTE: `fmap` has the same signature, but different implementations
        #       for different functors.
        # TODO: In order to be able to implement
        #       different `fmap`s, we need to support inline type definitions,
        #       or implicit type specifications/restrictions, e.g.
        #           (= (fmap $f ((: $C0 (Maybe $t)))) ($C0))
        #           (with $F as (Maybe $t)
        #             (= (fmap $f ($C0)) ($C0)))
        #       or something else. For example, another option would be to
        #       define `fmap_maybe`, and then define `fmap` as a non-deterministic
        #       symbol, which will be resolved to different functions via
        #       type-checking
        metta.add_parse('''
            (: fmap (-> (-> $a $b) ($F $a) ($F $b)))
            (= (fmap $f ($C0)) ($C0))
            (= (fmap $f ($C $x)) ($C ($f $x)))
            (: Nothing (-> (Maybe $t)))
            (: Something (-> $t (Maybe $t)))
            (: inc (-> Number Number))
            (= (inc $x) (+ $x 1))
        ''')
        self.assertEqual(
            metta.interpret("(fmap inc (Something 2))"),
            metta.interpret("(Something 3)"))
        self.assertEqual(
            metta.interpret("(fmap inc (Nothing))"),
            metta.interpret("(Nothing)"))
