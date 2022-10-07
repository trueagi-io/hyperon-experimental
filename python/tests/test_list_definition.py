import unittest

from hyperon import *
from common import MeTTa

class ListDefinitionTest(unittest.TestCase):

    # As per issue
    #
    # https://github.com/trueagi-io/hyperon-experimental/issues/104
    #
    # Test if adding type declaration to List data structure does
    # not interfere with executing functions operating on List.
    def test_list_definition(self):
        metta = MeTTa()
        metta.run('''
                ;; Define conditional
                (= (if True $x $y) $x)
                (= (if False $x $y) $y)

                ;; Declaration of List data structure
                (: List (-> $a Type))
                (: Nil (List $a))
                (: Cons (-> $a (List $a) (List $a)))

                ;; Insert an element in a presumably sorted list
                (= (insert $x Nil) (Cons $x Nil))
                (= (insert $x (Cons $head $tail))
                  (if (< $x $head)
                      (Cons $x (Cons $head $tail))
                      (Cons $head (insert $x $tail))))

                ;; Sort a list
                (= (sort Nil) Nil)
                (= (sort (Cons $head $tail)) (insert $head (sort $tail)))
        ''')

        # Test insert
        self.assertEqual(metta.run('''
            !(insert 1 Nil)
            !(insert 2 (insert 1 Nil))
            !(insert 3 (insert 2 (insert 1 Nil)))
            ''', flat=True),
            metta.parse_all('''
             (Cons 1 Nil)
             (Cons 1 (Cons 2 Nil))
             (Cons 1 (Cons 2 (Cons 3 Nil)))
            '''))

        # Test sort
        self.assertEqual(metta.run('''
            !(sort (Cons 1 Nil))
            !(sort (Cons 2 (Cons 1 Nil)))
            !(sort (Cons 3 (Cons 1 (Cons 2 Nil))))
            ''', flat=True),
            metta.parse_all('''
             (Cons 1 Nil)
             (Cons 1 (Cons 2 Nil))
             (Cons 1 (Cons 2 (Cons 3 Nil)))
            ''')
        )


if __name__ == "__main__":
    unittest.main()
