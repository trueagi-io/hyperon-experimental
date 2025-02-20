import logging
from threading import Event
from queue import Queue
from threading import Thread, Lock
from commands_parser import CommandsParser, Command

import time


class ChitChatClass:
    def __init__(self, log, tts_waci, auto_history, caller, commands_config, config):
        self.log = log
        self.tts_waci = tts_waci
        self.auto_history = auto_history
        self.caller = caller
        self.config = config

        self.sentence_queue = Queue()
        self.commands_parser = CommandsParser(commands_config)
        self.global_command = None
        self.global_command_lock = Lock()
        self.is_terminated = False

        self.t_thread_sentence_process = Thread(target=self.thread_sentence_process)
        self.t_thread_sentence_process.start()

    @property
    def enable_canceling(self):
        return self.config.get("chitchat.enable_canceling")

    @property
    def enable_interruption(self):
        return self.config.get("chitchat.enable_interruption")

    @property
    def interruption_time_sec(self):
        return self.config.get("chitchat.interruption_time_sec")

    @property
    def enable_proactive(self):
        return self.config.get("chitchat.enable_proactive")

    def recv_callback_sentence(self, msg):
        self.log.debug(f"Recieve sentence: {msg}")
        if msg.utterance.strip().startswith(":"):
            self.process_command(msg.utterance)
            return
        if msg.utterance == "": # special case, it happen when STT does not return result
            self.sentence_queue.put(None) # try execute canceled commands
        else:
            self.sentence_queue.put(msg)

    def recv_callback_command(self, msg):
        u = msg.data
        self.process_command(u)

    def process_command(self, u):
        self.log.debug(f"Recieve command: {u}")
        if self.commands_parser.is_command(u):
            command = self.commands_parser.create_command(u)
            if command.is_unknown():
                self.log.warning(f"WARNING! {u} UNKNOWN COMMAND! It will be ignored!")
            elif command.is_system():
                self.process_system_command(command)
            else:
                with self.global_command_lock:
                    self.global_command = command
                if not self.tts_waci.is_user_speaking():
                    self.sentence_queue.put(None) # try execute command if user is not speaking
        else:
            self.log.warning(f"WARNING! {u} UNKNOWN COMMAND! It will be ignored!")

    def terminate(self):
        self.is_terminated = True
        self.hard_stop()
        self.sentence_queue.put(None)

    def process_system_command(self, command):
        if command.body == "clear":
            self.clear_history()
        elif command.body == "stop":
            self.hard_stop()
        elif command.body == "reset":
            self.clear_history()
            self.hard_stop()

    def clear_history(self):
        self.log.info("CLEAR DIALOG HISTORY")
        self.auto_history.clear()

    def hard_stop(self):
        self.log.info("HARD STOP")
        self.tts_waci.hard_stop()
        with self.global_command_lock:
            self.global_command = None

    def get_stream_for_command(self, command):
        if command.command_type == "simple_say":
            return [command.body], "simple_say"
        if command.command_type == "free_say":
            return self.caller.call_stream(self.auto_history.get_history_joined())
        if command.command_type == "directed_say":
            return self.caller.call_stream(self.auto_history.get_history_joined(), direction = command.body)
        self.log.error("unknown command in get_stream_for_command")

    def try_execute_command(self, command, current_lang):
        # from this point processing can be canceled
        if self.tts_waci.is_canceled():
            self.log.debug("PROCESSING HAS BEEN CANCELED 1!!!!")
            return
        try:
            stream, tag = self.get_stream_for_command(command)
            for i, response in enumerate(stream):
                if not self.tts_waci.say_if_not_canceled(response, current_lang, tag):
                    self.log.debug(f"PROCESSING HAS BEEN CANCELED 3 cycle={i}!!!!")
                    return
        except Exception as e:
            self.log.error(f"catch exception in try_execute_command: type={type(e)}  str={e}")
            self.tts_waci.say_if_not_canceled("I am sorry, I am having difficulties generating a response now!", "en-US", 'error')

    def get_command_from_global(self, msg):
        self.log.debug(f"get_command_from_global, global_command:{self.global_command}")
        command = None
        with self.global_command_lock:
            if self.global_command is not None:
                command = self.global_command
                self.global_command = None
            elif msg is not None and self.enable_proactive:
                command = Command("free_say", None)
        return command

    def try_put_command_back_to_global_command(self, command):
        command.is_retry = True
        with self.global_command_lock:
            if self.global_command is None:
                self.log.debug(f"Put back global command: {command}")
                self.global_command = command

    def thread_sentence_process(self):
        current_lang = "en-US"
        while not self.is_terminated:
            msg = self.sentence_queue.get()
            self.tts_waci.reset(self.enable_canceling, self.enable_interruption, self.interruption_time_sec)

            if msg is not None:
                self.log.debug(f"Start Processing: msg.utterance={msg.utterance} msg.lang={msg.lang}")
                utterance = msg.utterance
                current_lang = msg.lang
                self.auto_history.add_user_message("user", utterance)
            else:
                self.log.debug(f"Start Processing empty message, try command")

            command = self.get_command_from_global(msg)
            self.log.debug(f"Local command: {command}")
            if command is None:
                continue   # we need do nothing

            self.try_execute_command(command, current_lang)
            if not self.tts_waci.wait_start_speaking_or_canceled():
                # was canceled before start speaking
                self.try_put_command_back_to_global_command(command)
            self.tts_waci.wait_stop_speaking_or_canceled()
