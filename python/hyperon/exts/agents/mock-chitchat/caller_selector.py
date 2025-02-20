from caller_langchain_singleuser_v1 import CallerLangchainSingleUserV1
from caller_testmodel import CallerTestModel
from langchain_openai import ChatOpenAI
import datetime
import locale
from inline_animations_prompt import generate_ia_prompt

class CallerSelector:
    def __init__(self, log, config, image_handler=None):
        self.log = log
        self.config = config
        self.model_name = config.get("chitchat.model_name")
        self.image_handler = image_handler
        if self.model_name == "test":
            self.caller = CallerTestModel()
        elif self.model_name in ["gpt-4o-mini", "gpt-4o"]:
            model = ChatOpenAI(model = self.model_name, temperature = config.get("chitchat.temperature"))
            self.caller = CallerLangchainSingleUserV1(model, image_handler=image_handler)
        else:
            raise Exception("Invalid mode")

    @property
    def situation_awareness(self):
        return self.config.get("chitchat_character.situation_awareness")

    @property
    def background(self):
        return self.config.get("chitchat_character.background")

    @property
    def timeout(self):
        return self.config.get("chitchat.timeout")

    @property
    def add_inline_animations(self):
        return self.config.get("chitchat.timeout")

    @property
    def permantent_response_direction(self):
        return self.config.get("chitchat_character.permanent_response_direction").strip()

    def call_stream(self, history, direction = None):
        full_sa = self.get_full_situation_awareness()
        if self.add_inline_animations:
            ia_prompt = generate_ia_prompt(self.config)
            full_system = self.background + "\n" + full_sa + "\n" + ia_prompt
        else:
            full_system = self.background + "\n" + full_sa

        if self.permantent_response_direction == "":
            full_direction = direction
        else:
            if direction is None:
                full_direction = self.permantent_response_direction
            else:
                full_direction = self.permantent_response_direction + "\n" + direction

#        print (full_system)


        return self.caller.call_stream(full_system, history, self.timeout, full_direction), self.model_name

    def close(self):
        self.caller.close()

    def get_full_situation_awareness(self):
        # set the locale to 'C' to ensure English is used
        locale.setlocale(locale.LC_TIME, 'C')
        now = datetime.datetime.now()
        return self.situation_awareness + " " + now.strftime("Today's date is %A, %B %d, %Y. Local time is %H:%M.")
