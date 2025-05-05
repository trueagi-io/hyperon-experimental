from .base import Tokenizer

class MettaModRef:
    """Class represents a reference to the MeTTa module structure. It is a
    wrapper of the reference to the corresponding Rust class."""

    def __init__(self, cmodref):
        """Initialize wrapper"""
        self.cmodref = cmodref

    def tokenizer(self):
        """Returns module's tokenizer instance"""
        return Tokenizer._from_ctokenizer(self.cmodref.tokenizer())
