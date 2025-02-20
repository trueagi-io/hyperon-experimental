from mock_messages import MockChatMessage, MockStrMessage
import threading
from queue import Queue
import time

SPEECH_PACE = 20

class MockSTT:
    def __init__(self, node):
        self.node = node

        self.request_queue = Queue()
        self.thread_requests = threading.Thread(target=self.process_requests, daemon = True)
        self.thread_requests.start()

    def publish_realtime(self, utterance, lang = "en-US"):
        self.request_queue.put((utterance, lang))
    def publish_direct(self, utterance, lang = "en-US"):
        self.publish_chat_message(utterance, lang)

    def process_requests(self):
        while self.node.is_running:
            utterance, lang = self.request_queue.get()
            t_length = len(utterance) / SPEECH_PACE
            time.sleep(0.2)
            t_start = time.time()

            self.publish_event("speechstart")

            while time.time() < t_start + t_length:
                time.sleep(0.2)
                self.publish_event("speechcont")

            time.sleep(0.2)
            self.publish_event("speechstop")
            self.publish_chat_message(utterance, lang)

    def publish_event(self, event):
        self.node.publish("/stt/event_topic", MockStrMessage(event))

    def publish_chat_message(self, utterance, lang = "en-US"):
        self.node.publish("/stt/sentence_topic", MockChatMessage(utterance = utterance, lang = lang))

