from mock_tts_say_with_cancel import MockTTSSayWithCancel
from mock_auto_conv_history import MockAutoConvHistory
from hyperon.exts.agents.events.mock_ros2_node import MockROS2Node
import random
import unittest
import time
import logging

logging.basicConfig(level=logging.INFO)

class Test(unittest.TestCase):
    def test(self):
        node = MockROS2Node()

        tts_interface = MockTTSSayWithCancel(node)
        ach = MockAutoConvHistory(node)

        # test of normal say
        self.assertEqual(tts_interface.say_if_not_canceled(f"Hi!", "en-US", "test"), True)
        self.assertEqual(tts_interface.say_if_not_canceled(f"My name is Sergey! How are you?", "en-US", "test"), True)
        self.assertEqual(tts_interface.wait_stop_speaking_or_canceled(), True)
        time.sleep(0.5)
        self.assertEqual(ach.get_history()[0][0], 'robot')
        self.assertEqual(ach.get_history()[0][1], 'Hi!')
        self.assertEqual(ach.get_history()[1][0], 'robot')
        self.assertEqual(ach.get_history()[1][1], 'My name is Sergey! How are you?')

        ach.clear()
        tts_interface.reset()

        # Test of hard stop (should say something like "My name is Sergey! How...")
        self.assertEqual(tts_interface.say_if_not_canceled(f"My name is Sergey! How are you? My name is Sergey! How are you? My name is Sergey! How are you?", "en-US", "test"), True)
        time.sleep(2)
        tts_interface.hard_stop()
        self.assertEqual(tts_interface.wait_stop_speaking_or_canceled(), False)
        time.sleep(0.5)
        self.assertEqual(ach.get_history()[0][0], 'robot')
        self.assertTrue(ach.get_history()[0][1].startswith('My name'))
        self.assertTrue(ach.get_history()[0][1].endswith('...'))

        # test that continuation will not be possible
        self.assertEqual(tts_interface.say_if_not_canceled(f"Hi!", "en-US", "test"), False)
        self.assertEqual(tts_interface.wait_stop_speaking_or_canceled(), False)
        time.sleep(0.5)
        self.assertEqual(len(ach.get_history()), 1)



        # Test of Canceling
        ach.clear()
        tts_interface.reset()
        self.assertEqual(tts_interface.say_if_not_canceled(f"My name is Sergey! {random.randint(1, 100000)}", "en-US", "test"), True)
        self.assertEqual(tts_interface.say_if_not_canceled(f"Second Phrase", "en-US", "test"), True)
        time.sleep(0.1)
        tts_interface.cancel_if_possible()
        self.assertEqual(tts_interface.wait_stop_speaking_or_canceled(), False)
        time.sleep(0.5)
        self.assertEqual(len(ach.get_history()), 0)




if __name__ == '__main__':
    unittest.main()

