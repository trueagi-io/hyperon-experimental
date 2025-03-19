from hyperon import MeTTa, E
from hyperon.exts.agents import AgentObject
from queue import Queue
from time import sleep

# A simple purely Python custom daemon agent
# All calls to its methods run in separate threads
# and do not return results directly
class DaemonAgent(AgentObject):
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
                sleep(0.05)
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
m.register_atom('agnt', DaemonAgent.agent_creator_atom())
# NOTE: the test may occasionally fail
result = m.run('''
  ! (bind! &a1 (agnt))
  ! (&a1) ; run the agent
  ; ! (println! "Agent is running")
  ! ((py-atom time.sleep) 0.03)
  ! (assertEqual (&a1 .response) "Waiting 0")
  ! (&a1 .input "Hello")
  ; ! (println! "Agent is receiving messages")
  ! ((py-atom time.sleep) 0.03)
  ! (assertEqual (&a1 .response) "olleH")
  ! (&a1 .stop)
''')

for r in result:
    assert r == [E()], r
