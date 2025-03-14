from hyperon import *
from hyperon.ext import register_atoms
import queue

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
        # FIXME? if we raise the exception here, the thread is not stopped
        # but should we put Error into the result?
        try:
            for r in self.method(*self.args):
                self._result.put(r)
        except Exception as e:
            self._result.put(E(S('Error'), ValueAtom(self.args), ValueAtom(e)))

    def __iter__(self):
        return self

    def __next__(self):
        try:
            while self.is_alive():
                yield self._result.get_nowait()
        except queue.Empty:
            pass
        raise StopIteration

def _try_atom2str(val):
    if isinstance(val, GroundedAtom):
        val = val.get_object().content
    elif isinstance(val, SymbolAtom):
        val = val.get_name()
    if val is None or isinstance(val, str):
        return val
    return repr(val)


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
    def get_agent_atom(cls, metta, *args, unwrap=True, **kwargs):
        # metta and unwrap are not passed to __init__, because
        # they are needed only for __metta_call__, so children
        # classes do not need to pass them to super().__init__
        # TODO: catch exceptions and turn them into error messages?
        if unwrap:
            # a hacky way to unwrap args
            agent_atom = OperationObject("_", cls).execute(*args, **kwargs)[0]
            agent = agent_atom.get_object().content
        else:
            agent = cls(*args, **kwargs)
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

    def __init__(self, path=None, atoms={}, include_paths=None, code=None):
        if path is None and code is None:
            # purely Python agent
            return
        # The first argument is either path or code when called from MeTTa
        if isinstance(path, ExpressionAtom):# and path != E():
            code = path
        elif path is not None:
            path = _try_atom2str(path)
            with open(path, mode='r') as f:
                code = f.read()
        # _code can remain None if the agent uses parent runner (when called from MeTTa)
        self._code = code.get_children()[1] if isinstance(code, ExpressionAtom) else \
            _try_atom2str(code)
        self._atoms = atoms
        self._include_paths = include_paths
        self._context_space = None
        self._create_metta()

    def _create_metta(self):
        if self._code is None:
            return None
        self._init_metta()
        result = self._load_code()
        for r in result:
            if r != [E()]:
                # TODO: catch only errors? unwrap error messages?
                raise RuntimeError(r)

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
        if isinstance(self._code, str):
            return self._metta.run(self._code)
        else:
            self._metta.space().add_atom(self._code)
            return []

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
        unwrap = self._unwrap
        method = self.__call__
        if len(args) > 0 and isinstance(args[0], SymbolAtom):
            n = args[0].get_name()
            if n[0] == '.' and hasattr(self, n[1:]):
                method = getattr(self, n[1:])
                args = args[1:]
                call = False
                # FIXME? Python methods called via . are supposed to be purely Python
                unwrap = True
        if unwrap:
            method = OperationObject(f"{method}", method).execute
        if call and self.is_daemon():
            st = StreamMethod(method, args)
            st.start()
            # We don't return the stream here; otherwise it will be consumed immediately.
            # If the agent itself would be StreamMethod, its results could be accessbile.
            # Here, they are lost (TODO?).
            return [E()]
        # NOTE: previously, `StreamMethod` object was created always and returned here
        # instead of calling `method` directly. The idea was that agents could consume
        # a part of the stream from other agents they are calling, but this cannot work
        # without support from MeTTa, because StreamMethod also calls `method` (just in
        # a separate thread), thus, it should be MeTTa itself, which turns `match` results
        # into a stream. Thus, we return the result directly now
        return method(*args)

class EventAgent(AgentObject):

    StopEvent = object()

    def __init__(self, path=None, atoms={}, include_paths=None, code=None, event_bus=None):
        if event_bus is not None:
            # EventAgent is not a daemon by default: although its `event_processor` runs in a thread
            # and should be stopped, other non-event methods are more convenient to call directly to
            # get their results back in a caller
            # self.daemon = True
            atoms = {**atoms}
            atoms['&event_bus'] = event_bus if isinstance(event_bus, Atom) else ValueAtom(event_bus)
            atoms['queue-subscription'] = OperationAtom('queue-subscription', self.queue_subscription, unwrap=False)
            atoms['has-event-bus'] = OperationAtom('has-event-bus',
                lambda: self.event_bus is not None and \
                        hasattr(self.event_bus, "create_subscription"))
        self.event_bus = event_bus.get_object().value if isinstance(event_bus, GroundedAtom) else event_bus
        super().__init__(path, atoms, include_paths, code)
        # Even if there is no event bus, events can be submitted by child class methods
        self.events = Queue()
        self.running = False
        self.outputs = Queue()

    def _init_metta(self):
        # NOTE: atm, there is no utility for the base agent to import `agents` by default,
        # but event agents in metta typically need it
        super()._init_metta()
        if self._metta is not None:
            self._metta.run("! (import! &self agents)")

    def start(self, *args):
        if self.running:
            raise RuntimeError("Currently, EventAgent is supposed to be running in one thread")
        if not args:
            args = ()
        self.running = True
        st = StreamMethod(self.event_processor, args)
        st.start()

    # TODO? it's similar to `input` in former BaseListeningAgent, but
    # it uses event_id and func, which might be inconvenient...
    # either event_id or func can be made excessive, so we could
    # turn this function into a queued call to an agent method...
    def recv_queue_event(self, event_id, func, *args):
        # TODO? we can keep {event_id: func} dict in self
        # instead of having `func` as an argument here
        self.events.put((event_id, func, args))

    def queue_subscription(self, event_id, func):
        '''
        Subscribing `recv_queue_event` to the given event channel
        to put incoming events to queue processed in a separate
        thread instead of subscribing `func` directly
        '''
        self.event_bus.create_subscription(_try_atom2str(event_id),
            lambda *args: self.recv_queue_event(event_id, func, *args))
        return [E()]

    def event_processor(self, *args):
        # `*args` received on `start`
        while self.running:
            # TODO? func can be a Python function?
            (event_id, func, args) = self.events.get()
            if event_id is self.StopEvent:
                break
            # Wrapping into ValueAtom if arg is not an atom yet
            resp = self._metta.evaluate_atom(E(func,
                *[a if isinstance(a, Atom) else ValueAtom(a) for a in args]))
            # TODO? do we need `outputs` here? we may want to publish `resp` to a certain channel
            # or let `func` to do this direclty...
            # ??? self.clear_outputs()
            for r in resp:
                self.outputs.put(r)

    def stop(self):
        self.events.put((self.StopEvent, None, None))
        self.running = False

    # TODO? choose the model of dealing with outputs... do we need them at all?
    def clear_outputs(self):
        try:
            while True:
                self.outputs.get_nowait()
        except queue.Empty:
            pass

    def get_output(self):
        try:
            while True:
                yield self.outputs.get_nowait()
        except queue.Empty:
            pass

def subscribe_metta_func(metta: MeTTa, event_bus: GroundedAtom, event_id: Atom, func: Atom): #metta,
    event_bus = event_bus.get_object().content
    if not hasattr(event_bus, "create_subscription"):
        raise RuntimeError("Event bus should have create_subscription method")
    event_id = _try_atom2str(event_id)
    event_bus.create_subscription(event_id,
        lambda *args: metta.evaluate_atom(E(func, *[ValueAtom(a) for a in args])))
    return [E()]

# The function to be called from MeTTa
def publish_event(event_bus: Atom, event_id: Atom, content: Atom):
    assert isinstance(event_bus, GroundedAtom), f"{event_bus} is not a grounded object"
    event_bus = event_bus.get_object().value
    event_id = _try_atom2str(event_id)
    # FIXME? We want to be able to pass Atoms as event content, but not
    # any event bus can support this... or should we always use wrappers,
    # which provide this support?
    event_bus.publish(event_id, content)
    return [E()]

@register_atoms(pass_metta=True)
def agent_atoms(metta):
    return {
        r"create-agent": AgentObject.agent_creator_atom(unwrap=False),
        # We have to avoid generic agent_creator_atom here to pass ordered parameters
        r"event-agent": OperationAtom('event-agent',
            lambda path=None, event_bus=None: EventAgent.get_agent_atom(None, unwrap=False, path=path, event_bus=event_bus),
            unwrap=False),
        r"direct-subscription": OperationAtom('direct-subscription',
            lambda *args: subscribe_metta_func(metta, *args), unwrap=False),
        r"publish-event": OperationAtom('publish-event',
            publish_event, unwrap=False)
    }
