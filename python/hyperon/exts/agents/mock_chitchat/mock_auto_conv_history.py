import time


def add_period_if_needed(sentence):
    if not sentence.endswith(('.', '!', '?')):
        return sentence + '.'
    return sentence

class MockAutoConvHistory:
    def __init__(self, node):
        self.node = node
        self.history = []
        node.create_subscription("/tts/said_utterances", self.said_utterance_cb)

    def add_user_message(self, user_name, text):
        self.history.append((user_name, text, time.time()))

    def said_utterance_cb(self, msg):
        self.node.get_logger().debug(f"add robot utterance to history: {msg.data}")
        self.history.append(("robot", msg.data, time.time()))

    def clear(self):
        self.history.clear()

    def get_history(self):
        return list(self.history)

    def get_history_joined(self):
        rez = []
        for r,u,t in self.history:
            if len(rez) == 0 or rez[-1][0] != r:
                rez.append((r,u,t))
            else:
                rez[-1] = (r, add_period_if_needed(rez[-1][1]) + " " + u, t)
        return rez
