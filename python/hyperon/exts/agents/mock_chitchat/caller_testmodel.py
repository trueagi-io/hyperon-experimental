class CallerTestModel:
    def call_stream(self, background, history, situation_awareness, timeout):
        phrase = history[-1][1]
        return [phrase, phrase]
