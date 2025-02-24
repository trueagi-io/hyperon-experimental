import cv2
import base64
import threading

class MockImageHandler:
    def __init__(self):
        self.cap = cv2.VideoCapture(0)
        self.latest_frame = None
        self.running = True
        self.lock = threading.Lock()
        threading.Thread(target=self._capture_loop, daemon=True).start()

    def _capture_loop(self):
        while self.running:
            ret, frame = self.cap.read()
            if ret:
                with self.lock:
                    self.latest_frame = frame

    def get_image_base64(self):
        with self.lock:
            if self.latest_frame is not None:
                return self._encode_to_base64(self.latest_frame)

    def _encode_to_base64(self, frame):
        _, buffer = cv2.imencode('.jpg', frame)
        return base64.b64encode(buffer).decode('utf-8')

    def release(self):
        self.running = False
        self.cap.release()

