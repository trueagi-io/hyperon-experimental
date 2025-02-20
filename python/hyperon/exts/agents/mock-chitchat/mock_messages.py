from dataclasses import dataclass

@dataclass
class MockStrMessage:
    data: str

@dataclass
class MockChatMessage:
    utterance : str
    lang : str = "en-US"
    confidence: int = 0
    source: str = ""
    audio_path: str = ""
    stamp_start = None
    stamp_stop = None

@dataclass
class MockTTSRequest:
    stamp: float
    utterance: str
    lang : str = "en-US"
    source : str = ""

@dataclass
class MockTimeMessage:
    stamp: float
