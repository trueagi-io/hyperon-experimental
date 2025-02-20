from hyperon import MeTTa
from hyperon.exts.agents import EventAgent
from hyperon.exts.agents.events.mock_ros2_node import MockROS2Node
from hyperon.exts.agents.events.mock_messages import MockChatMessage, MockStrMessage
from queue import Queue
from time import sleep

node = MockROS2Node()

m = MeTTa()
print(m.run('''
!(import! &self agents)
!((event-agent agent.metta) (g 3))
'''))


metta_agent = EventAgent(code=
'''
!(import! &self agents)
(= (on_stt $arg)
   (println! (UTTERANCE: (py-dot $arg utterance))))
; !(direct-subscription &event_bus "/stt/sentence_topic" on_stt)
!(queue-subscription "/stt/sentence_topic" on_stt)
''', event_bus=node)

metta_agent.start()

sleep(2)
node.publish("/stt/sentence_topic", MockChatMessage(utterance = "Hellow World", lang = "MeTTa"))
sleep(2)
metta_agent.stop()
#node.create_subscription("/stt/sentence_topic", chitchat.recv_callback_sentence)
#node.create_subscription("/tts/command_topic", chitchat.recv_callback_command)
