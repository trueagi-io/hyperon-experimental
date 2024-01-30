import unittest

from hyperon import *

from pathlib import Path
pwd = Path(__file__).parent

class BuiltinModTest(unittest.TestCase):

    def process_exceptions(self, results):
        for result in results:
            self.assertEqual(result, [E()])

    #TODO: This follows the pattern in test_run_metta.py, and the API is slightly different when the module system is integrated
    def test_dasgate(self):
        '''
        This test tests the dasgate module that is included with Hyperon
        '''
        pass
        #TODO: This test will fail if the "hyperon_das" module isn't installed
        # self.process_exceptions(MeTTa(env_builder=Environment.test_env()).import_file(f"{pwd}/../hyperon/metta_mods/das_gate/test_das.metta"))

if __name__ == "__main__":
    unittest.main()
