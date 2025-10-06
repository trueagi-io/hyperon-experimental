import unittest

from hyperon import Environment

class HyperonTestCase(unittest.TestCase):

    def __init__(self, methodName):
        super().__init__(methodName)

    def testEnvironment(self):
        self.assertTrue(Environment.init_common_env(config_dir = "/tmp/hyperon-test", create_config = True))
        self.assertEqual(Environment.config_dir(), "/tmp/hyperon-test")

        self.assertFalse(Environment.init_common_env())
