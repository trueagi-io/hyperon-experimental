import yaml

class Command:
    def __init__(self, t, s):
        self.command_type = t
        self.body  = s
        self.is_retry = False
    def is_system(self):
        return self.command_type == "system"
    def is_unknown(self):
        return self.command_type == "unknown"
    def __str__(self):
        return f"{self.command_type} {self.body}"

class CommandsParser:
    def __init__(self, commands):
        self.simple_say_commands = commands["simple_say"]
        self.directed_say_commands = commands["directed_say"]
        self.system_commands = ["clear", "reset", "stop"]

    def is_command(self, u):
        return u is not None and ( (u.startswith("<") and u.endswith(">")) or u.startswith(":") )

    def _split_command(self, c):
        return [p.strip() for p in c.split(":")]

    def is_valid_simple_say_command(self, c):
        commands = self._split_command(c)
        return all(s in self.simple_say_commands for s in commands)

    def parse_command(self, u):
        c = ""
        if u.startswith("<") and u.endswith(">"):
            c = u[1:-1]
        elif u.startswith(":"):
            c = u[1:]
        c = c.strip()
        if c in self.system_commands:
            return "system", c

        if c == "s":
            return "free_say", None

        if c in self.directed_say_commands:
            return "directed_say", self.directed_say_commands[c]

        if self.is_valid_simple_say_command(c):
            commands = self._split_command(c)
            return "simple_say", " ".join(self.simple_say_commands[c2] for c2 in commands)

        if c.startswith("dd"):
            return "directed_say", c[2:].strip()

        if c.startswith("ss"):
            return "simple_say", c[2:].strip()

        return "unknown", None

    def create_command(self, u):
        return Command(*self.parse_command(u))
