from hyperon.exts.agents import EventAgent
from hyperon.exts.agents.events.basic_bus import BasicEventBus
from time import sleep

'''
These tests check for the capability of metta agents to
subscribe and reasct to events in the event bus.
* direct-subscription doesn't utilize the agent itself and puts
nothing to the agent's output; it also doesn't put events into
a queue but calls feedback functions immediately
* queue-subscription puts the event into the queue and also
puts the feedback function result into the agent's output
'''

class FeedbackCatch:
   def __init__(self, node):
        self.catched = False
        node.create_subscription("agent-event", self.check_feedback)
   def check_feedback(self, e):
       assert e == "Pong", e
       self.catched = True

node = BasicEventBus()
fc = FeedbackCatch(node)

metta_agent = EventAgent(code=
'''
(= (on_event $arg)
   ((py-dot &event_bus publish) "agent-event"
    (if (== $arg "Ping") "Pong" "Wrong")))
(= (on_queued $arg)
   ((py-dot $arg swapcase))
)
!(direct-subscription &event_bus "event-topic-1" on_event)
!(queue-subscription "event-topic-2" on_queued)
''', event_bus=node)

metta_agent.start()

sleep(0.02)
node.publish("event-topic-1", "Ping")
sleep(0.02)
res = list(metta_agent.get_output())
node.publish("event-topic-2", "World")
sleep(0.02)
res2 = list(metta_agent.get_output())
metta_agent.stop()

assert res == [], f"got {res} for direct-subscription"
assert fc.catched, "No feedback event received"
assert res2[0].get_object().value == "wORLD", f"got {res2} for queue-subscription"
