from mock_tts_interface_waci import MockTTSInterfaceWACI
from mock_ros2_node import MockROS2Node
from mock_auto_conv_history import MockAutoConvHistory
from mock_messages import MockStrMessage

import random
import unittest
from queue import Queue
import time
import logging

logging.basicConfig(level=logging.INFO)

class Test(unittest.TestCase):
    def test(self):
        node = MockROS2Node()
        tts_interface = MockTTSInterfaceWACI(node)
        ach = MockAutoConvHistory(node)

        self.assertEqual(tts_interface.is_user_speaking(), False)

        # operation without canceling
        tts_interface.reset(False, False, 0.2)
        self.assertEqual(tts_interface.say_if_not_canceled(f"My name is Sergey! How are you?", "en-US", "test"), True)
        time.sleep(0.1)

        node.publish("/stt/event_topic", MockStrMessage("speechcont"))

        time.sleep(0.5)
        self.assertEqual(tts_interface.is_user_speaking(), True)

        node.publish("/stt/event_topic", MockStrMessage("speechcont"))

        self.assertEqual(tts_interface.wait_stop_speaking_or_canceled(), True)
        time.sleep(0.5)
        self.assertEqual(ach.get_history()[0][0], 'robot')
        self.assertEqual(ach.get_history()[0][1], 'My name is Sergey! How are you?')

        node.publish("/stt/event_topic", MockStrMessage("speechstop"))

        time.sleep(0.1)
        self.assertEqual(tts_interface.is_user_speaking(), False)
        # test interrupution
        tts_interface.reset(enable_canceling = False, enable_interruption = True, interruption_time_sec = 0.2)
        ach.clear()
        self.assertEqual(tts_interface.say_if_not_canceled(f"My name is Sergey! How are you?", "en-US", "test"), True)
        time.sleep(1)
        node.publish("/stt/event_topic", MockStrMessage("speechcont"))
        time.sleep(0.5)
        node.publish("/stt/event_topic", MockStrMessage("speechcont"))
        self.assertEqual(tts_interface.wait_stop_speaking_or_canceled(), False)
        time.sleep(0.5)

        self.assertEqual(ach.get_history()[0][0], 'robot')
        self.assertTrue(ach.get_history()[0][1].startswith('My name'))
        self.assertTrue(ach.get_history()[0][1].endswith('...'))


        # test continuation (should be canceled, so nothing should happen)
        self.assertEqual(tts_interface.say_if_not_canceled(f"Hi!", "en-US", "test"), False)
        self.assertEqual(tts_interface.wait_stop_speaking_or_canceled(), False)
        time.sleep(0.5)
        self.assertEqual(len(ach.get_history()), 1)

        # test canceling
        tts_interface.reset(enable_canceling = True, enable_interruption = False, interruption_time_sec = 0.2)
        ach.clear()

        self.assertEqual(tts_interface.say_if_not_canceled(f"My name is Sergey! How are you? {random.randint(0,10000)}", "en-US", "test"), True)
        time.sleep(0.5)

        node.publish("/stt/event_topic", MockStrMessage("speechcont"))

        self.assertEqual(tts_interface.wait_stop_speaking_or_canceled(), False)
        time.sleep(0.5)
        self.assertEqual(len(ach.get_history()), 0)

        self.assertEqual(tts_interface.is_user_speaking(), True)
        #test MAX_SPEECHSTOP_WAIT_TIME timeout
        time.sleep(6)
        self.assertEqual(tts_interface.is_user_speaking(), False)
if __name__ == '__main__':
    unittest.main()

