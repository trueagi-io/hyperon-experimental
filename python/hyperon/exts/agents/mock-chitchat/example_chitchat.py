import yaml
from mock_tts_interface_waci import MockTTSInterfaceWACI
from mock_ros2_node import MockROS2Node
from mock_tts import MockTTS
from mock_stt import MockSTT
from mock_config import MockConfig
from mock_auto_conv_history import MockAutoConvHistory
from chitchat_class import ChitChatClass
from caller_selector import CallerSelector
from mock_image_handler import MockImageHandler
import logging

logging.basicConfig(level=logging.INFO)


# Initialize mock enviroment
config = MockConfig("config.yaml")

if config.get("chitchat.enable_vision"):
    image_handler = MockImageHandler()
else:
    image_handler = None

node = MockROS2Node()
stt = MockSTT(node)
tts_waci = MockTTSInterfaceWACI(node)
auto_history = MockAutoConvHistory(node)

# After this point your can use mock environment
# You should use three classes:
# tts_waci - TTS interface with automatic canceling and interruption
# auto_history - class for automatic collection of conversation history from TTS
# image_handler - image handler
# You should subscribe for two topics using node.create_subscription:
#     - "/stt/sentence_topic" to get sentences from STT
#     - "/chitchat/command_topic" to get chitchat commands
# You should use:
# stt.publish_realtime function to simulate receiving sentence via STT approximately in realtime with all corresponding events
# stt.publish_direct function to directly send sentence to "/stt/sentence_topic" (simulation of user typing something in GUI)

caller = CallerSelector(node.get_logger(), config, image_handler)
commands_config = {"simple_say": {}, "directed_say" : {}}
chitchat = ChitChatClass(node.get_logger(), tts_waci, auto_history, caller, commands_config, config)

node.create_subscription("/stt/sentence_topic", chitchat.recv_callback_sentence)
node.create_subscription("/tts/command_topic", chitchat.recv_callback_command)

while True:
    s = input().strip()
    if s.startswith(":"): # it is command, we publish it immediatly
        stt.publish_direct(s)
    else:
        stt.publish_realtime(s)
