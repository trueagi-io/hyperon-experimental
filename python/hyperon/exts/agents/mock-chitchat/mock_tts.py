from queue import Queue
import threading
from mock_messages import MockStrMessage, MockTimeMessage
import time
import logging

SPEECH_PACE = 20

class MockTTS:
    def __init__(self, node):
        self.log = logging.getLogger(__name__ + '.' + type(self).__name__)
        self.node = node
        self.node.create_subscription("/tts/request_topic", self.cb_request)
        #self.node.create_subscription("/tts/command_topic", self.cb_command)
        self.request_queue = Queue()

        self.t_stop_before = 0
        self.t_last = None
        self.is_speaking = False
        self.thread_requests = threading.Thread(target=self.process_requests, daemon = True)
        self.thread_requests.start()

    def cb_request(self, msg):
        self.log.info(f"Receive TTS request: {msg.utterance}")
        self.request_queue.put((msg.stamp, msg.utterance))

    def process_requests(self):
        while self.node.is_running:
            if self.request_queue.empty():
                self.set_is_speaking(False)

            t_stamp, phrase = self.request_queue.get()
            if t_stamp <= self.t_stop_before:
                self.log.info(f"Cancel1 phrase: {phrase}")
                continue
            time.sleep(0.5)


            if t_stamp <= self.t_stop_before:
                self.log.info(f"Cancel2 phrase: {phrase}")
                continue

            self.set_is_speaking(True)
            self.node.publish("/tts/start_speaking_topic", MockTimeMessage(t_stamp))
            self.log.info(f"Start speaking: {phrase}")
            self.t_last = t_stamp
            said_buffer = ""
            for c in phrase:
                if t_stamp > self.t_stop_before:
                    said_buffer += c
                    time.sleep(1/SPEECH_PACE)
            self.log.info(f"Stop speaking: {said_buffer}")
            if said_buffer == phrase:
                self.node.publish("/tts/said_utterances", MockStrMessage(said_buffer))
            elif len(said_buffer) > 0:
                self.node.publish("/tts/said_utterances", MockStrMessage(said_buffer + "..."))
            self.node.publish("/tts/stop_speaking_topic", MockTimeMessage(t_stamp))


    def set_is_speaking(self, new_is_speaking):
        if self.is_speaking == new_is_speaking:
            return
        if self.is_speaking == False and new_is_speaking == True:
            self.node.publish("/tts/robot_speaking_event_topic", MockStrMessage("speechstart"))
        if self.is_speaking == True and new_is_speaking == False:
            self.node.publish("/tts/robot_speaking_event_topic", MockStrMessage("speechstop"))
        self.is_speaking = new_is_speaking
    def hard_stop(self):
        self.t_stop_before = time.time()

    def try_cancel(self):
        if not self.is_speaking:
            self.t_stop_before = time.time()
            return True
        return False

