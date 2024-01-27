from hyperon.ext import *

@register_tokens()
def my_get_runner():
    raise Exception('This MeTTa module intentionally fails to load')
