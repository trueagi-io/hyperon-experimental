This repository contains mock environment for developing and testing conversation agents for our robots.

## "Low level" environment

see `example_lowlevel_environment.py` with example of usage.
It consists of following elements:
- mock tts class interface with canceling and interruption (`MockTTSSayWithCancel` which is mock of `TTSSayWithCancel`) 
- mock stt class which simulate receiving sentences from STT in realtime with corresponding events 
- mock ros2 node class which can be used for subscribing for mock ros topics.

## "High Level" environment with example chitchat

see `example_chitchat.py`
We have example chitchat which uses two extra classes:
- `MockTTSInterfaceWACI` which is analogue of `TTSInterfaceWACI` - tts interface with automatic canceling and interruption handling
- `MockAutoConvHistory` which is analogue of `AutoConvHistory` - class which automatically build conversation history by listening `/tts/said_utterances` topic.
 
## Explanations about TTS interface.

The TTS interface assumes the following use case: a series of TTS requests that logically represent a single request. Typically, this corresponds to a response from an LLM that has been split into sentences.
This series of TTS
requests can be
canceled which means interrupted before first utterance from this
series started pronounced. This series also can be interrupted (`hard_stop`) at any
point. It is important that after successful interruption all future
messages from this series will be ignored.  

Because of this `MockTTSSayWithCancel` has the following interface

- `reset` - which has to be called in the beginning of each series of TTS
requests!
- `cancel_if_possible` - initiate canceling. It is important that this
call is non blocking and it does not return success of canceling
- `hard_stop` - interruption
- `say_if_not_canceled` - send TTS request if the current series has not been interrupted
- `is_canceled` - either the current session has been canceled or interrupted 
- `wait_start_speaking_or_canceled` - wait for TTS start pronouncing first phrase from the current series
- `wait_stop_speaking_or_canceled` - wait for TTS stop pronouncing all phrase from the current series.

