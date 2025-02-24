from mock_messages import MockTTSRequest
from mock_tts import MockTTS
import time


class MockTTSSayWithCancel:
    def __init__(self, node):
        self.node = node
        self.mock_tts = MockTTS(node)
        self.reset()
        self.node.create_subscription("/tts/start_speaking_topic", self.tts_start_speaking_cb)
        self.node.create_subscription("/tts/stop_speaking_topic", self.tts_stop_speaking_cb)

    def reset(self):
        self.t_first = None
        self.t_last = None
        self._is_canceled = False
        self.last_tts_start_speaking_t = None
        self.last_tts_stop_speaking_t = None

    def hard_stop(self):
        self._is_canceled = True
        self.mock_tts.hard_stop()
    def cancel_if_possible(self):
        self._is_canceled = self.mock_tts.try_cancel()
    def is_canceled(self):
        return self._is_canceled
    def say_if_not_canceled(self, utterance, lang, tag = "tag"):
        if self.is_canceled():
            return False

        t = time.time()
        message = MockTTSRequest(stamp = t, utterance = utterance, lang = lang, source = tag)
        self.node.publish("/tts/request_topic", message)
        if self.t_first is None:
            self.t_first = t
        self.t_last = t
        return True

    def tts_start_speaking_cb(self, msg):
        self.last_tts_start_speaking_t = msg.stamp

    def tts_stop_speaking_cb(self, msg):
        self.last_tts_stop_speaking_t = msg.stamp

    def wait_start_speaking_or_canceled(self):
        if self.t_first is None: # we haven't tried to say something == canceled
            return False

        while True:
            if self.is_canceled():
                return False
            if self.last_tts_start_speaking_t is not None and self.last_tts_start_speaking_t >= self.t_first:
                return True
            time.sleep(0.01)

    def wait_stop_speaking_or_canceled(self):
        if not self.wait_start_speaking_or_canceled():
            # was canceled before start speaking
            return False
        while True:
            if self.is_canceled():
                return False
            if self.last_tts_stop_speaking_t is not None and self.last_tts_stop_speaking_t >= self.t_last:
                return True
            time.sleep(0.01)
