import unittest

from test_common import *
from hyperon import *


class LoadAsciiTest(HyperonTestCase):

    def setUp(self):
        change_dir_to_parent_of(__file__)

    def test_load(self):
        metta = MeTTa()
        test_file = "test_load.metta"
        metta.run(f'''
            !(bind! &space (new-space))
            !(load-ascii &space {test_file})
        ''')
        content = metta.run("!(match &space $x $x)")[0]
        with open(test_file) as f:
            self.assertEqualNoOrder(metta.parse_all(f.read()),
                                    content)


if __name__ == "__main__":
    unittest.main()
