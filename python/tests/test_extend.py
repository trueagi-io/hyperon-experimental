import unittest

from hyperon import *

class CaseTest(unittest.TestCase):

    def test_case(self):
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

