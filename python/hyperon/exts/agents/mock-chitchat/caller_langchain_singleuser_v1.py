import os
import logging
from stream_iterators import IteratorSentenceBySentence
from langchain_core.output_parsers import StrOutputParser
from langchain_core.messages import HumanMessage, SystemMessage, AIMessage
from langchain_core.runnables import chain
from langchain_core.messages.utils import trim_messages

class CallerLangchainSingleUserV1:
    def __init__(self, model, image_handler=None):
        self.log = logging.getLogger(__name__ + '.' + type(self).__name__)
        self.model = model
        self.image_handler = image_handler

        # maximal lenght of history 5000 characters
        #(we do not count tokens since it is not an issue anymore for most of models)
        self.history_trimmer = trim_messages( max_tokens=5000, strategy="last", token_counter=len, include_system=True)

    def dialog_history_to_messages(self, history):
        main_users = [h[0] for h in history if h[0] != "robot"]
        if len(main_users) > 0:
            main_user = main_users[-1]
        else:
            main_user = None
        # remove all other users
        messages = []
        for u,s,t in history:
            if u == main_user:
                messages.append(HumanMessage(content=s))
            if u == "robot":
                messages.append(AIMessage(content=s))
        return messages

    def insert_image(self, messages):
        self.log.debug("Including image in messages")
        image_base64 = self.image_handler.get_image_base64()
        if image_base64 is None:
            self.log.error("Error, image_base64 is None, Image is not included")
            return messages
        last_message = messages.pop()

        # Reformat text content to fit image format
        image_content = {
            "type": "image_url",
            "image_url": {
                "url": f"data:image/jpeg;base64,{image_base64}",
                "detail": "low"}}
        new_message = HumanMessage(content = [image_content, last_message.content])
        system_message = SystemMessage(content = "The images you are getting are coming from your vision sensors. Don't mention them unless necessary, and treat them not as images, but rather as what you see.")
        # Insert image in messages
        messages.append(system_message)
        messages.append(new_message)
        return messages

    def call_stream(self, full_system, history,timeout, direction):
        systemm = SystemMessage(content = full_system)
        history_messages = self.history_trimmer.invoke(self.dialog_history_to_messages(history))

        messages = [systemm] + history_messages

        # Add image if received
        if self.image_handler is not None:
            messages = self.insert_image(messages)

        if direction is not None:
            messages.append(SystemMessage(content = f"{direction}"))

        chain = self.model.with_retry(stop_after_attempt = 3) | StrOutputParser() | IteratorSentenceBySentence

        return chain.stream(messages)
