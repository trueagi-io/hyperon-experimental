from .atoms import Serializer, SerialResult

class ConvertingSerializer(Serializer):

    def __init__(self):
        super().__init__()
        self.value = None

    def serialize_bool(self, v):
        self.value = v
        return SerialResult.OK

    def serialize_int(self, v):
        self.value = v
        return SerialResult.OK

    def serialize_float(self, v):
        self.value = v
        return SerialResult.OK
