from hyperon.exts.agents.events.mock_ros2_node import MockROS2Node
from mock_stt import MockSTT

import unittest
from queue import Queue

import time
import logging

class EventReceiver:
    def __init__(self, node):
        node.create_subscription("/stt/event_topic", self.cb_event)
        node.create_subscription("/stt/sentence_topic", self.cb_sentence)
        self.last_event = ""
    def cb_event(self, msg):
        event = msg.data
        print(f"receive event {event}")
        self.last_event = event
    def cb_sentence(self, msg):
        print(f"receive sentence: {msg.lang} {msg.utterance}")

class Test(unittest.TestCase):
    def test(self):
        node = MockROS2Node()
        stt = MockSTT(node)
        er = EventReceiver(node)

        stt.publish_realtime("hi how are you?", "en-US")

        time.sleep(2)
        self.assertEqual(er.last_event, "speechstop")

if __name__ == '__main__':
    unittest.main()

