from hyperon import MeTTa
from hyperon.exts.agents import AgentObject
from queue import Queue
from time import sleep

# =================================

class Agnt(AgentObject):
    def __init__(self):
        self.messages = Queue()
        self.running = False
        self.output = Queue()
        self.daemon = True
    def __call__(self):
        self.running = True
        cnt = 0
        while self.running:
            if self.messages.empty():
                self.output.put(f"Waiting {cnt}")
                sleep(2)
                cnt += 1
            else:
                m = self.messages.get()
                self.output.put(m[::-1])
    def stop(self):
        self.running = False
    def input(self, msg):
        self.messages.put(msg)
    def response(self):
        if self.output.empty():
            return None
        return self.output.get()

m = MeTTa()
m.register_atom('agnt', Agnt.agent_creator_atom())
print(m.run('''
  ! (bind! &a1 (agnt))
  ! (&a1)
  ! (println! "Agent is running")
  ! ((py-atom time.sleep) 1)
  ! (println! ("First response:" (&a1 .response)))
  ! (&a1 .input "Hello")
  ! (println! "Agent is receiving messages")
  ! ((py-atom time.sleep) 2)
  ! (println! ("Second response:" (&a1 .response)))
  ! (&a1 .stop)
'''))
