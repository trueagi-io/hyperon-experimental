import unittest

from hyperon import *

class CaseTest(unittest.TestCase):

    def test_case(self):
        metta = MeTTa()
        self.assertEqual(
            metta.run('''
                ; cases are processed sequentially
                !(case (+ 1 5)
                  ((5 Error)
                   (6 OK)
                   (6 Error)))

                ; we can use variables as cases
                !(case (+ 1 5)
                  (($x (+ 1 $x))))

                ; it is non-deterministic: each value is matched against all cases
                !(case (+ 1 (superpose (1 2 3)))
                  ((3 OK-3)
                   (4 OK-4)))

                ; one case can produce multiple results
                !(case (+ 1 (superpose (1 2 3)))
                  (($x (+ 1 $x))))

                ; cases are not necessarily exhaustive,
                ; and the result can be empty
                !(case 5
                  ((6 OK)))
            '''),
            [[S('OK')],
             [ValueAtom(7)],
             [S('OK-3'), S('OK-4')],
             [ValueAtom(3), ValueAtom(4), ValueAtom(5)],
             []])

        self.assertEqual(
            metta.run('''
                (Rel-P A B)
                (Rel-Q A C)

                ; cases can be used for deconstruction
                !(case (match &self ($rel A $x) ($rel $x))
                  (((Rel-P $y) (P $y))
                   ((Rel-Q $y) (Q $y))))

                ; %void% can be used to capture empty results
                !(case (match &self ($rel B $x) ($rel $x))
                  (((Rel-P $y) (P $y))
                   ((Rel-Q $y) (Q $y))
                   (%void% no-match)))

                ; a functional example
                (= (maybe-inc $x)
                   (case $x
                    (((Just $v) (Just (+ 1 $v)))
                     (Nothing Nothing)))
                )
                !(maybe-inc Nothing)
                !(maybe-inc (Just 2))
            '''),
            [[E(S('P'), S('B')), E(S('Q'), S('C'))],
             [S('no-match')],
             [S('Nothing')],
             [E(S('Just'), ValueAtom(3))]])
