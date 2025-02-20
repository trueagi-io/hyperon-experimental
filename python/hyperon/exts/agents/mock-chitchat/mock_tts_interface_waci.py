from mock_tts_say_with_cancel import MockTTSSayWithCancel
import time

MAX_SPEECHSTOP_WAIT_TIME = 5.0

# tts interface with automatic canceling and interrupution
class MockTTSInterfaceWACI():
    def __init__(self, node):
        self.tts = MockTTSSayWithCancel(node)
        self.node = node
        self.log = node.get_logger()
        self.reset(enable_canceling = False, enable_interruption = False)
        self._is_user_speaking = False
        node.create_subscription("/stt/event_topic", self.recv_callback_hear_event)

    def reset(self, enable_canceling, enable_interruption, interruption_time_sec = 0.2):
        self.enable_canceling = enable_canceling
        self.enable_interruption = enable_interruption
        self.interruption_time_sec = interruption_time_sec
        self.t_other_start_talking = None
        self.tts.reset()

    def hard_stop(self):
        self.tts.hard_stop()

    def initiate_canceling_if_enable(self):
        if self.enable_canceling:
            self.tts.cancel_if_possible()
            self.log.debug(f"Initiate canceling")

    def initiate_interruption_if_needed(self):
        if self.enable_interruption:
            self.tts.hard_stop()
            self.log.debug(f"Initiate interruption")

    def recv_callback_hear_event(self, msg):
        self.log.debug(f"Speech event recieved: msg={msg}")
        if msg.data == "speechstart" or msg.data == "speechcont":
            if self.t_other_start_talking == None:
                self.t_other_start_talking = time.time()
            if time.time() - self.t_other_start_talking >= self.interruption_time_sec:
                self.initiate_interruption_if_needed()
            self.initiate_canceling_if_enable()
            self._is_user_speaking = True
            self.t_last_speech_event = time.time()

        if msg.data == "speechstop":
            self._is_user_speaking = False

    def is_user_speaking(self):
        if self._is_user_speaking and time.time() - self.t_last_speech_event > MAX_SPEECHSTOP_WAIT_TIME:
            self._is_user_speaking = False
        return self._is_user_speaking

    def say_if_not_canceled(self, utterance, lang, tag):
        return self.tts.say_if_not_canceled(utterance, lang, tag)

    def wait_start_speaking_or_canceled(self):
        return self.tts.wait_start_speaking_or_canceled()

    def wait_stop_speaking_or_canceled(self):
        return self.tts.wait_stop_speaking_or_canceled()

    def is_canceled(self):
        return self.tts.is_canceled()
