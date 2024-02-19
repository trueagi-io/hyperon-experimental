from hyperon import *
from hyperon.ext import *

@register_tokens
def type_tokens():
    return {
        r"pi_test": lambda token: ValueAtom(3.14159),
    }
