import unittest

from hyperon import *

class ExtendTest(unittest.TestCase):

    def test_extend(self):
        metta = MeTTa()
        self.assertEqual(
            metta.run('''
              !(extend-py! extention)
              !(get-by-key &my-dict "A")
              !(get-by-key &my-dict 6)
            '''),
            [[],
             [ValueAtom(5)],
             [ValueAtom('B')]])
        self.assertEqual(
              metta.run('! &runner')[0][0].get_object().value,
              metta)


