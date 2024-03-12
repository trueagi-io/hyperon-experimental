from .atoms import Serializer, SerialResult

class ConvertingSerializer(Serializer):
    """A serializer to convert value from other runtime into a Python value"""

    def __init__(self):
        """Construct new serializer"""
        super().__init__()
        self.value = None

    def serialize_bool(self, v):
        """Accept bool value"""
        self.value = v
        return SerialResult.OK

    def serialize_int(self, v):
        """Accept int value"""
        self.value = v
        return SerialResult.OK

    def serialize_float(self, v):
        """Accept float value"""
        self.value = v
        return SerialResult.OK
