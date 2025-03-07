from collections import defaultdict
import logging
from hyperon import OperationAtom
from hyperon.ext import register_atoms

class BasicEventBus:
    def __init__(self):
        self.subscriptions = defaultdict(list)
        self.is_running = True
    def terminate(self):
        self.is_running = False
    def create_subscription(self, topic, cb):
        self.subscriptions[topic].append(cb)
    def publish(self, topic, msg):
        for cb in self.subscriptions[topic]:
            cb(msg)
    def get_logger(self):
        return logging.getLogger("MockNode")

@register_atoms
def event_atoms():
    return {
        r"basic-event-bus": OperationAtom('basic-event-bus', BasicEventBus)
    }
