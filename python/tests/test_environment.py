import unittest

from hyperon import Environment

class HyperonTestCase(unittest.TestCase):

    def __init__(self, methodName):
        super().__init__(methodName)

    def testEnvironment(self):
        Environment.init_platform_env(config_dir = "/tmp/test_dir")
        self.assertEqual(Environment.config_dir(), "/tmp/test_dir")
