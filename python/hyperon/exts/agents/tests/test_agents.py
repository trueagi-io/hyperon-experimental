from hyperon import MeTTa
from hyperon.exts.agents import AgentObject

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
