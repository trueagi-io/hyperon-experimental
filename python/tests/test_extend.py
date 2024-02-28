import unittest

from hyperon import *

class ExtendTest(unittest.TestCase):

    def test_extend(self):
        '''
        This test verifies that importing from a python-implemnted module along with @register_atoms and @register_tokens works
        '''
        metta = MeTTa(env_builder=Environment.custom_env(working_dir=os.getcwd(), disable_config=True, is_test=True))
        self.assertEqual(
            metta.run('''
              !(import! &self extension)
              !(get-by-key &my-dict "A")
              !(get-by-key &my-dict 6)
            '''),
            [[E()],
             [ValueAtom(5)],
             [ValueAtom('B')]])
        self.assertEqual(
              metta.run('! &runner')[0][0].get_object().value, metta)

class ExtendTestDirMod(unittest.TestCase):

    def test_extend_dir_pymod(self):
        '''
        This test verifies that importing from a python module directory also works
        '''
        metta = MeTTa(env_builder=Environment.custom_env(working_dir=os.getcwd(), disable_config=True, is_test=True))
        self.assertEqual(
            metta.run('''
              !(import! &self ext_dir)
              !(get-by-key &my-dict "A")
              !(get-by-key &my-dict 6)
            '''),
            [[E()],
             [ValueAtom(5)],
             [ValueAtom('B')]])
        self.assertEqual(
              metta.run('! &runner')[0][0].get_object().value, metta)

    #LP-TODO-Next:  Temporarily disabled as I change the way modules that are in the process of being
    # loaded are accessed by name.  See comments around `relative_submodule_import_test`
    #
    # def test_extend_subdir_pymod(self):
    #     '''
    #     This test verifies that importing from a module that imports its own sub-module also works
    #     '''
    #     metta = MeTTa(env_builder=Environment.custom_env(working_dir=os.getcwd(), disable_config=True, is_test=True))
    #     self.assertEqual(
    #         metta.run('''
    #           !(import! &self ext_sub)
    #           !(get-by-key &my-dict "A")
    #           !(get-by-key &my-dict 6)
    #         '''),
    #         [[E()],
    #          [ValueAtom(5)],
    #          [ValueAtom('B')]])
    #     self.assertEqual(
    #           metta.run('! &runner')[0][0].get_object().value, metta)

    def test_extend_recursive_pymod(self):
        '''
        This test verifies that importing from a sub-module will cause the necessary parents to be imported as well
        '''
        metta = MeTTa(env_builder=Environment.custom_env(working_dir=os.getcwd(), disable_config=True, is_test=True))
        self.assertEqual(
            metta.run('''
              !(import! &self ext_recursive:level-2:ext_nested)
              !(get-by-key &my-dict "A")
              !(get-by-key &my-dict 6)
            '''),
            [[E()],
             [ValueAtom(5)],
             [ValueAtom('B')]])
        self.assertEqual(
              metta.run('! &runner')[0][0].get_object().value, metta)

class ExtendGlobalTest(unittest.TestCase):

    def test_extend_global(self):
        '''
        This test is intended to check that if the extension uses internal states,
        they are not lost between metta.run's and are also accessible from Python
        '''
        from extension import g_object
        # Sanity check
        self.assertEqual(g_object, None)
        metta = MeTTa(env_builder=Environment.custom_env(working_dir=os.getcwd(), disable_config=True, is_test=True))
        metta.run('''
          !(import! &self extension)
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

class ExtendErrorTest(unittest.TestCase):

    def test_error_pyext(self):
        '''
        This test verifies that an error from a Python extension is properly propagated
        '''
        metta = MeTTa(env_builder=Environment.custom_env(working_dir=os.getcwd(), disable_config=True, is_test=True))
        try:
          metta.run("!(import! &self error_pyext)")
        except Exception as err:
            pass
        else:
            raise Exception('error_pyext.py should raise an error when loading, so no-err is an error')

if __name__ == "__main__":
    unittest.main()
