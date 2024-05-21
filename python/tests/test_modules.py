import unittest
from test_common import *

from hyperon import *

class ModulesTest(HyperonTestCase):

    def test_python_file_mod_format(self):
        """
        Tests that a MeTTa module, implemented as a '.py', file will be correctly identified when the
        module system searches in the include directory, and then that it will be sucessfully loaded
        """
        runner = MeTTa(env_builder=Environment.custom_env(working_dir=os.getcwd(), disable_config=True, is_test=True))

        #Make sure the `import!` operation finds the pyfile_test_mod.py file, recognizes it as a
        # MeTTa module using the PythonFileModuleFormat, and loads the MeTTa module it sucessfully
        result = runner.run("!(import! &self pyfile_test_mod)")

        #Validate that we didn't get an error loading nor importing the module
        self.assertFalse(atom_is_error(result[0][0]))

        #Validate that we can access an atom from the module, and it's the atom we expect
        result = runner.parse_all("pi_test")
        self.assertEqual(result[0].get_object().content, 3.14159)

    def test_include(self):
        metta = MeTTa(env_builder=Environment.custom_env(working_dir=os.getcwd(), disable_config=True, is_test=True))
        result = metta.run("""
            (three isprime)
            !(match &self ($x isprime) $x)
            !(include test_include)
            !(match &self ($x isprime) $x)
        """)
        self.assertTrue(areEqualNoOrder(result[0], [S("three")]))
        self.assertTrue(areEqualNoOrder(result[2], [S("three"), S("five"), S("seven")]))

        result = metta.run("!(match &self ($x notprime) $x)")
        self.assertEqual(result[0], [S("six")])
