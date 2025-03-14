from hyperon import MeTTa, E
from hyperon.exts.agents import AgentObject

# =========== Test 1 ===========
# This is an example of purely Python agent, which accepts
# an argument on __init__ and has two methods
class MyAgent(AgentObject):
    def __init__(self, x):
        self.x = x
    def subs(self, a):
        return self.x - a
    def __call__(self, a, b):
        return a + b + self.x

m = MeTTa()
atom = MyAgent.agent_creator_atom()
m.register_atom('new-agent', atom)
result = m.run('''
  ! (bind! &agent (new-agent 5))
  ! (assertEqual (&agent 7 8) 20)
  ! (assertEqual (&agent .subs 4) 1)
  ! (import! &self agents)
; Also check that we can compose it with the metta agent
  ! (bind! &agent-metta (create-agent agent.metta))
  ! (assertEqual
      (&agent-metta (g (&agent .subs 3)))
      4)
  ! (assertEqual
      (&agent (&agent-metta (g 2)) 3)
      12)
''')

for r in result:
    assert r == [E()], r

# =========== Test 2 ===========
# This is an example of agents producing and consuming
# (non-cuncurrent) streams of each other
class Agent1(AgentObject):
    def __call__(self):
        for x in range(10):
            yield x
            #sleep(1)
class Agent2(AgentObject):
    def __call__(self, xs):
        for x in xs:
            yield x*2
class Agent3(AgentObject):
    def __call__(self, xs):
        #for x in xs:
        #    print("Result: ", x)
        return list(xs)

m = MeTTa()
m.register_atom('new-agent-1', Agent1.agent_creator_atom())
m.register_atom('new-agent-2', Agent2.agent_creator_atom())
m.register_atom('new-agent-3', Agent3.agent_creator_atom())
result = m.run('''
  ! ((new-agent-3) ((new-agent-2) ((new-agent-1))))
''')[0][0].get_object().value
assert result == [0, 2, 4, 6, 8, 10, 12, 14, 16, 18], result

