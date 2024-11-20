from hyperon import *
from hyperon.ext import register_atoms

'''
This is very preliminary and incomplete PoC version.
However, it is put to exts, because metta-motto depends on it.
Reagrding threading:
- Generic threading for metta can be introduced with
  parallel and sequential composition, for-comprehension, etc.
  Agents could be built on top of this functionality. However,
  this piece of code was driven by metta-motto demands.
- Two main cases for agents are:
  -- Immediate call with inputs to get outputs
  -- Asynchronous events and responses
  Supporting both cases in one implementation is more convenient,
  because both of them can be needed simultaneously in certain
  domains (e.g. metta-motto)
- Implementation can be quite different.
  -- Agents could be started explicitly
  -- They could inherint from StreamMethod
  -- Other methods could be called directly without StreamMethod wrapper
  All these nuances are to be fleshed out
'''

import threading
from queue import Queue
class StreamMethod(threading.Thread):
    def __init__(self, method, args):
        super().__init__() #daemon=True
        self._result = Queue()
        self.method = method
        self.args = args

    def run(self):
        for r in self.method(*self.args):
            self._result.put(r)

    def __iter__(self):
        return self

    def __next__(self):
        if self._result.empty() and not self.is_alive():
            raise StopIteration
        return self._result.get()


class AgentObject:

    '''
    The base agent object class, which purpose is twofold.
    1) It allows using Metta scripts as agents.
    2) Python classes inherited from it, can be used for
       creating agents in metta (almost) without additional
       boilerplate code.
    These two purposes are combined in one base class, so one
    can inherit from it and add functionality, which will be
    shared between Python and Metta agents (as how it happens
    in Metta-motto)
    '''
    _name = None

    @classmethod
    def get_agent_atom(cls, metta, *args, unwrap=True):
        # metta and unwrap are not passed to __init__, because
        # they are needed only for __metta_call__, so children
        # classes do not need to pass them to super().__init__
        if unwrap:
            # a hacky way to unwrap args
            agent_atom = OperationObject("_", cls).execute(*args)[0]
            agent = agent_atom.get_object().content
        else:
            agent = cls(*args)
        if metta is not None:
            if hasattr(agent, '_metta') and agent._metta is not None:
                raise RuntimeError(f"MeTTa is already defined for {agent}")
            agent._metta = metta
        agent._unwrap = unwrap
        return [OperationAtom(cls.name(),
            lambda *agent_args: agent.__metta_call__(*agent_args), unwrap=False)]

    @classmethod
    def agent_creator_atom(cls, metta=None, unwrap=True):
        return OperationAtom(cls.name()+'-agent',
            lambda *args: cls.get_agent_atom(metta, *args, unwrap=unwrap),
            unwrap=False)

    @classmethod
    def name(cls):
        return cls._name if cls._name is not None else str(cls)

    def _try_unwrap(self, val):
        if val is None or isinstance(val, str):
            return val
        if isinstance(val, GroundedAtom):
            return str(val.get_object().content)
        return repr(val)

    def __init__(self, path=None, atoms={}, include_paths=None, code=None):
        if path is None and code is None:
            # purely Python agent
            return
        # The first argument is either path or code when called from MeTTa
        if isinstance(path, ExpressionAtom):# and path != E():
            code = path
        elif path is not None:
            path = self._try_unwrap(path)
            with open(path, mode='r') as f:
                code = f.read()
        # _code can remain None if the agent uses parent runner (when called from MeTTa)
        self._code = code.get_children()[1] if isinstance(code, ExpressionAtom) else \
            self._try_unwrap(code)
        self._atoms = atoms
        self._include_paths = include_paths
        self._context_space = None
        self._create_metta()

    def _create_metta(self):
        if self._code is None:
            return None
        self._init_metta()
        self._load_code()  # TODO: check that the result contains only units

    def _init_metta(self):
        ### =========== Creating MeTTa runner ===========
        # NOTE: each MeTTa agent uses its own space and runner,
        # which are not inherited from the caller agent. Thus,
        # the caller space is not directly accessible as a context,
        # except the case when _metta is set via get_agent_atom with parent MeTTa
        if self._include_paths is not None:
            env_builder = Environment.custom_env(include_paths=self._include_paths)
            metta = MeTTa(env_builder=env_builder)
        else:
            metta = MeTTa()
        # Externally passed atoms for registrations
        for k, v in self._atoms.items():
            metta.register_atom(k, v)
        self._metta = metta

    def _load_code(self):
        return self._metta.run(self._code) if isinstance(self._code, str) else \
            self._metta.space().add_atom(self._code)

    def __call__(self, atom):
        if self._unwrap or self._metta is None:
            raise NotImplementedError(
                f"__call__ for {self.__class__.__name__} should be defined"
            )
        return self._metta.evaluate_atom(atom)

    def is_daemon(self):
        return hasattr(self, 'daemon') and self.daemon is True

    def __metta_call__(self, *args):
        call = True
        method = self.__call__
        if len(args) > 0 and isinstance(args[0], SymbolAtom):
            n = args[0].get_name()
            if n[0] == '.' and hasattr(self, n[1:]):
                method = getattr(self, n[1:])
                args = args[1:]
                call = False
        if self._unwrap:
            method = OperationObject(f"{method}", method).execute
        st = StreamMethod(method, args)
        st.start()
        # We don't return the stream here; otherwise it will be consumed immediately.
        # If the agent itself would be StreamMethod, its results could be accessbile.
        # Here, they are lost (TODO?).
        if call and self.is_daemon():
            return [E()]
        return st

class BaseListeningAgent(AgentObject):
    def __init__(self, path=None, atoms={}, include_paths=None, code=None):
        super().__init__(path, atoms, include_paths, code)
        self.messages = Queue()
        self.running = False
        self._output = []
        self.lock = threading.RLock()

    def start(self,  *args):
        if not args:
            args = ()
        self.running = True
        st = StreamMethod(self.messages_processor, args)
        st.start()

    def message_processor(self, message, *args):
        return []

    def messages_processor(self, *args):
        while self.running:
            if not self.messages.empty():
                m = self.messages.get()
                with self.lock:
                    self._output = self.message_processor(m, *args)
        return []

    def stop(self):
        self.running = False
        return []

    def input(self, msg):
        self.messages.put(msg)
        return []

    def get_output(self):
        return self._output

@register_atoms(pass_metta=True)
def agent_atoms(metta):
    return {
        r"create-agent": AgentObject.agent_creator_atom(unwrap=False),
    }
