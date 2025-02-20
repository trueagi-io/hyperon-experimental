import yaml
from mock_tts_say_with_cancel import MockTTSSayWithCancel
from mock_ros2_node import MockROS2Node
from mock_tts import MockTTS
from mock_stt import MockSTT
from mock_config import MockConfig
from mock_auto_conv_history import MockAutoConvHistory
from chitchat_class import ChitChatClass
from caller_selector import CallerSelector
from mock_image_handler import MockImageHandler
import logging
import time

logging.basicConfig(level=logging.INFO)


# Initialize mock enviroment
# if you need image handler
image_handler = MockImageHandler()
node = MockROS2Node()
stt = MockSTT(node)
tts = MockTTSSayWithCancel(node)

# After this point your can use mock environment
# You can use the following classes:
# tts - TTS interface with non-automatic canceling and interruption
# image_handler - image handler
# You can subscribe for the following topics using node.create_subscription:
#     - "/stt/sentence_topic" to get sentences from STT
#     - "/stt/event_topic" events from STT (speechstart speechcont speechstop)
#     - "/tts/said_utterances" - to get utterances which was said by TTS
# You should use:
# stt.publish_realtime function to simulate receiving sentence via STT approximately in realtime with all corresponding events
# stt.publish_direct function to directly send sentence to "/stt/sentence_topic" (simulation of user typing something in GUI)

# EXAMPLE how to subscribe for topics
def stt_sentence_handler(msg):
    print(f"receive STT sentece {msg.lang} {msg.utterance}")

def stt_event_handler(msg):
    print(f"receive STT event: {msg.data}")

def tts_said_utterance(msg):
    print(f"receive TTS said utterance: {msg.data}")

node.create_subscription("/stt/sentence_topic", stt_sentence_handler)
node.create_subscription("/stt/event_topic", stt_event_handler)
node.create_subscription("/tts/said_utterances", tts_said_utterance)

# EXAMPLE how to use tts class

# Reset before send series of TTS requests
tts.reset()
tts.say_if_not_canceled("Hi!", "en-US")
tts.say_if_not_canceled("How are you?", "en-US")

# we've decided to cancel
tts.cancel_if_possible()
# we've decided to interrupt

# Reset before sending next series of TTS requests
tts.reset()
tts.say_if_not_canceled("Hi!", "en-US")

# wait for system to "say" Hi!
time.sleep(2)
print("\n")

# Example How to use STT for simulate STT
stt.publish_realtime("Hello! It is a simulation of STT!")


# Wait for result for some time
time.sleep(5)

image_handler.release()
