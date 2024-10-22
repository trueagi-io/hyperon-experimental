from hyperon import MeTTa
from hyperon.exts.agents import AgentObject
from queue import Queue
from time import sleep

class MyAgent(AgentObject):
    '''
    Purely Python agent
    '''
    def __init__(self, x):
        self.x = x
    def subs(self, a):
        return self.x - a
    def __call__(self, a, b):
        return a + b + self.x

m = MeTTa()
atom = MyAgent.agent_creator_atom()
m.register_atom('new-agent', atom)
print(m.run('''
  ! (bind! &agent (new-agent 5))
  ! (&agent 7 8)
  ! (&agent .subs 4)
'''))

# =================================

class Agent1(AgentObject):
    def __call__(self):
        for x in range(10):
            yield x
            sleep(1)
class Agent2(AgentObject):
    def __call__(self, xs):
        for x in xs:
            yield x*2
class Agent3(AgentObject):
    def __call__(self, xs):
        for x in xs:
            print("Result: ", x)

m = MeTTa()
m.register_atom('new-agent-1', Agent1.agent_creator_atom())
m.register_atom('new-agent-2', Agent2.agent_creator_atom())
m.register_atom('new-agent-3', Agent3.agent_creator_atom())
print(m.run('''
  ! ((new-agent-3) ((new-agent-2) ((new-agent-1))))
'''))

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
  ! ("First response:" (&a1 .response))
  ! (&a1 .input "Hello")
  ! (println! "Agent is receiving messages")
  ! ((py-atom time.sleep) 2)
  ! ("Second response:" (&a1 .response))
  ! (&a1 .stop)
'''))
