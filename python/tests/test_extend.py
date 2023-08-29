import unittest

from hyperon import *

class ExtendTest(unittest.TestCase):

    def test_extend(self):
        '''
        This test verifies that extend-py! along with @register_atoms and @register_tokens works
        '''
        metta = MeTTa()
        self.assertEqual(
            metta.run('''
              !(extend-py! extension)
              !(get-by-key &my-dict "A")
              !(get-by-key &my-dict 6)
            '''),
            [[],
             [ValueAtom(5)],
             [ValueAtom('B')]])
        self.assertEqual(
              metta.run('! &runner')[0][0].get_object().value,
              metta)


class ExtendGlobalTest(unittest.TestCase):

    def test_extend_global(self):
        '''
        This test is intended to check that if the extension uses internal states,
        they are not lost between metta.run's and are also accessible from Python
        '''
        from extension import g_object
        # Sanity check
        self.assertEqual(g_object, None)
        metta = MeTTa()
        metta.run('''
          !(extend-py! extension)
          !(set-global! 42)
        ''')
        # Checking that the object is accessible and its value is correct
        self.assertEqual(
          metta.run('!(get-global)'),
          metta.run('! 42'))
        # We may have to reimport g_object, but its value should be set
        # We also can use import with extension.g_object instead of from to make it work without re-importing
        from extension import g_object
        self.assertEqual(g_object, 42)


if __name__ == "__main__":
    unittest.main()
