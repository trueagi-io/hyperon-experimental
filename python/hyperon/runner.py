import os
from importlib import import_module
import importlib.util
import sys
import hyperonpy as hp
from .atoms import Atom, AtomType, OperationAtom
from .base import GroundingSpaceRef, Tokenizer, SExprParser
from hyperonpy import EnvBuilder, ModuleId

class RunnerState:
    """
    The state for an in-flight MeTTa interpreter handling the interpretation and evaluation of atoms in a given grounding space.
    """
    def __init__(self, metta, program):
        """Initialize a RunnerState with a MeTTa object and a program to run"""
        parser = SExprParser(program)
        #WARNING the C parser object has a reference to the text buffer, and hyperonpy's CSExprParser
        #  copies the buffer into an owned string.  So we need to make sure this parser isn't freed
        #  until the RunnerState is done with it.
        self.parser = parser
        self.cstate = hp.runner_state_new_with_parser(metta.cmetta, parser.cparser)

    def __del__(self):
        """Frees a RunnerState and all associated resources."""
        hp.runner_state_free(self.cstate)

    def run_step(self):
        """
        Executes the next step in the interpretation plan, or begins interpretation of the next atom in the stream of MeTTa code.
        """
        hp.runner_state_step(self.cstate)
        err_str = hp.runner_state_err_str(self.cstate)
        if (err_str is not None):
            raise RuntimeError(err_str)

    def is_complete(self):
        """
        Returns True if the runner has concluded, or False if there are more steps remaining to execute
        """
        return hp.runner_state_is_complete(self.cstate)

    def current_results(self, flat=False):
        """
        Returns the current in-progress results from an in-flight program evaluation
        """
        results = hp.runner_state_current_results(self.cstate)
        if flat:
            return [Atom._from_catom(catom) for result in results for catom in result]
        else:
            return [[Atom._from_catom(catom) for catom in result] for result in results]

class ModuleDescriptor:
    """
    An object that uniquely describes a module, including the module's name, optionally a version
    """
    def __init__(self, c_module_descriptor):
        """Wraps the underlying ModuleDescriptor object from the core"""
        self.c_module_descriptor = c_module_descriptor

class RunContext:
    """
    An accessor object for the API used by the executable atoms inside a MeTTa program
    """
    def __init__(self, c_run_context):
        """Wraps the underlying RunContext object from the core"""
        self.c_run_context = c_run_context

    def init_self_module(self, descriptor, space, working_dir):
        """Must be called exactly once from within a module loader to initialize the module being loaded"""
        hp.run_context_init_self_module(self.c_run_context, descriptor.c_module_descriptor, space.cspace, working_dir)
        #LP-TODO-NEXT Handle errors that happen inside hp.run_context_init_self_module

    def metta(self):
        """Access the MeTTa runner that the RunContext is running within"""
        return MeTTa(cmetta = hp.run_context_get_metta(self.c_run_context))

    def space(self):
        """Access the space for the currently running module"""
        return GroundingSpaceRef._from_cspace(hp.run_context_get_space(self.c_run_context))

    def tokenizer(self):
        """Access the tokenizer for the currently running module"""
        return Tokenizer._from_ctokenizer(hp.run_context_get_tokenizer(self.c_run_context))

    def register_token(self, regexp, constr):
        """Registers a token in the currently running module's Tokenizer"""
        self.tokenizer().register_token(regexp, constr)

    def register_atom(self, name, symbol):
        """Registers an Atom with a name in the currently running module's Tokenizer"""
        self.register_token(name, lambda _: symbol)

    def import_dependency(self, mod_id):
        """Imports a loaded module as a dependency of the running module"""
        hp.run_context_import_dependency(self.c_run_context, mod_id)

class MeTTa:
    """This class represents the runner to execute MeTTa programs"""

    def __init__(self, cmetta = None, space = None, env_builder = None):

        if cmetta is not None:
            self.cmetta = cmetta
        else:
            if space is None:
                space = GroundingSpaceRef()
            if env_builder is None:
                env_builder = hp.env_builder_use_default()
            self.cmetta = hp.metta_new(space.cspace, env_builder)

    def __del__(self):
        hp.metta_free(self.cmetta)

    def __eq__(self, other):
        """Checks if two MeTTa runner handles point to the same runner."""
        return (hp.metta_eq(self.cmetta, other.cmetta))

    def space(self):
        """Gets the space for the runner's top-level module"""
        return GroundingSpaceRef._from_cspace(hp.metta_space(self.cmetta))

    def tokenizer(self):
        """Gets the tokenizer for the runner's top-level module"""
        return Tokenizer._from_ctokenizer(hp.metta_tokenizer(self.cmetta))

    def register_token(self, regexp, constr):
        """Registers a token"""
        self.tokenizer().register_token(regexp, constr)

    def register_atom(self, name, symbol):
        """Registers an Atom"""
        self.register_token(name, lambda _: symbol)

    def _parse_all(self, program):
        parser = SExprParser(program)
        while True:
            atom = parser.parse(self.tokenizer())
            if atom is None:
                break
            yield atom

    def parse_all(self, program):
        """Parse an entire program from text into atoms, using the Tokenizer of the runner's top module"""
        return list(self._parse_all(program))

    def parse_single(self, program):
        """Parse the next single token from the text program"""
        return next(self._parse_all(program))

    def load_core_stdlib(self, mod_name, private_to):
        """Loads the core stdlib into the runner, with the specified name and scope"""
        return hp.metta_load_core_stdlib(self.cmetta, mod_name, private_to.c_module_descriptor)

    def load_module_direct_from_func(self, mod_name, private_to, py_loader_func):
        """Loads a module into the runner using a loader function, with the specified name and scope"""
        def loader_func(c_run_context, c_descriptor):
            run_context = RunContext(c_run_context)
            descriptor = ModuleDescriptor(c_descriptor)
            py_loader_func(run_context, descriptor)

        return hp.metta_load_module_direct(self.cmetta, mod_name, private_to.c_module_descriptor, loader_func)

    def load_module_direct_from_pymod(self, mod_name, private_to, pymod_name):
        """Loads a module into the runner directly from a Python module, with the specified name and scope"""
        if not isinstance(pymod_name, str):
            pymod_name = repr(pymod_name)

        loader_func = _priv_make_module_loader_func_for_pymod(pymod_name)

        return self.load_module_direct_from_func(mod_name, private_to, loader_func)

    #LP-TODO-Next Need to call the python file format, so that a .py file dropped into a dir catalog
    # will be found and loaded.
    #LP-TODO-Next Test for python module file format

    # #LP-TODO-Next, OLD Code below.  Since the module system has unified the loading behavior and allows
    # # searching to be under the control of the bom, the question is how much of this we want to keep, and
    # # how much is totally unnecessary.
    # #
    # # Specifically, whether we want to keep the `extend-py!` operation at all.  If a metta module is
    # # already implemented as a Python module, then `MeTTa.load_module_direct_from_pymod` already provides
    # # the ability to load it from Python directly.  `extend-py!` *could* carry this functionality forward
    # # into MeTTa.
    # #
    # # On the other hand we could also implement a catalog that can query python modules that implement
    # # MeTTa modules, and totally eliminate `extend-py!`.
    # #
    # def load_py_module(self, py_mod_name):
    #     """Loads the given python module as a MeTTa module"""
    #     if not isinstance(py_mod_name, str):
    #         py_mod_name = repr(py_mod_name)
    #     try:
    #         mod = import_module(py_mod_name)
    #         for n in dir(mod):
    #             obj = getattr(mod, n)
    #             if '__name__' in dir(obj) and obj.__name__ == 'metta_register':
    #                 obj(self)
    #         return mod
    #     except:
    #         return None

    # #LP-TODO-Next, See discussion above, regarding whether we want to keep the `extend-py!` operation 
    # def load_py_module_from_mod_or_file(self, mod_name):
    #     """Loads the given python-implemented MeTTa module, first using python's module-namespace logic,
    #     then by searching for files in the MeTTa environment's search path"""

    #     # First, see if the module is already available to Python
    #     if not isinstance(mod_name, str):
    #         mod_name = repr(mod_name)
    #     mod = MeTTa.load_py_module(self, mod_name)
    #     if mod is None:
    #         # If that failed, try and load the module from a file
    #         file_name = mod_name if ".py" in mod_name else \
    #                     mod_name.replace('.', os.sep) + ".py"

    #         # Check each search path directory in order, until we find the module we're looking for
    #         num_search_paths = hp.metta_search_path_cnt(self.cmetta)
    #         search_path_idx = 0
    #         found_path = None
    #         while search_path_idx < num_search_paths:
    #             search_path = hp.metta_nth_search_path(self.cmetta, search_path_idx)
    #             test_path = os.path.join(search_path, file_name)
    #             if os.path.exists(test_path):
    #                 found_path = test_path
    #                 break
    #             search_path_idx += 1

    #         if found_path is not None:
    #             MeTTa.load_py_module_from_path(self, mod_name, found_path)
    #         else:
    #             raise RuntimeError("Failed to load module " + mod_name + "; could not locate file: " + file_name)

    # #LP-TODO-Next, See discussion above, regarding whether we want to keep the `extend-py!` operation 
    # def load_py_module_from_path(self, mod_name, path):
    #     """Loads the given python-implemented MeTTa module from a file at the specified path"""

    #     spec = importlib.util.spec_from_file_location(mod_name, path)
    #     module = importlib.util.module_from_spec(spec)
    #     mod_name = mod_name.split(os.sep)[-1]
    #     sys.modules[mod_name] = module
    #     spec.loader.exec_module(module)
    #     MeTTa.load_py_module(self, mod_name)

    # #LP-TODO-Next Import *should* put all MeTTa modules on equal footing, so I think this is dead code.
    # # Just keeping for reference until it's closer to merge-time
    # def import_file(self, fname):
    #     """Loads the program file and runs it"""
    #     path = fname.split(os.sep)
    #     if len(path) == 1:
    #         path = ['.'] + path
    #     f = open(os.sep.join(path), "r")
    #     program = f.read()
    #     f.close()
    #     # changing cwd
    #     # TODO: Changing the working dir will not be necessary when the stdlib ops can access the correct runner context.  See https://github.com/trueagi-io/hyperon-experimental/issues/410
    #     prev_cwd = os.getcwd()
    #     os.chdir(os.sep.join(path[:-1]))
    #     result = self.run(program)
    #     # restoring cwd
    #     os.chdir(prev_cwd)
    #     return result

    def run(self, program, flat=False):
        """Runs the program"""
        parser = SExprParser(program)
        results = hp.metta_run(self.cmetta, parser.cparser)
        err_str = hp.metta_err_str(self.cmetta)
        if (err_str is not None):
            raise RuntimeError(err_str)
        if flat:
            return [Atom._from_catom(catom) for result in results for catom in result]
        else:
            return [[Atom._from_catom(catom) for catom in result] for result in results]

class Environment:
    """This class contains the API for configuring the host platform interface used by MeTTa"""

    def config_dir():
        """Returns the config dir in the common environment"""
        path = hp.environment_config_dir()
        if (len(path) > 0):
            return path
        else:
            return None

    def init_common_env(working_dir = None, config_dir = None, create_config = False, disable_config = False, is_test = False, include_paths = []):
        """Initialize the common environment with the supplied args"""
        builder = Environment.custom_env(working_dir, config_dir, create_config, disable_config, is_test, include_paths)
        return hp.env_builder_init_common_env(builder)

    def test_env():
        """Returns an EnvBuilder object specifying a unit-test environment, that can be used to init a MeTTa runner"""
        return hp.env_builder_use_test_env()

    def custom_env(working_dir = None, config_dir = None, create_config = False, disable_config = False, is_test = False, include_paths = []):
        """Returns an EnvBuilder object that can be used to init a MeTTa runner, if you need multiple environments to coexist in the same process"""
        builder = hp.env_builder_start()
        if (working_dir is not None):
            hp.env_builder_set_working_dir(builder, working_dir)
        if (config_dir is not None):
            hp.env_builder_set_config_dir(builder, config_dir)
        if (create_config):
            hp.env_builder_create_config_dir(builder)
        if (disable_config):
            hp.env_builder_disable_config_dir(builder)
        if (is_test):
            hp.env_builder_set_is_test(True)
        for path in reversed(include_paths):
            hp.env_builder_add_include_path(builder, path)
        return builder

def _priv_load_py_stdlib(c_run_context, c_descriptor):
    """
    Private function called indirectly to load the Python stdlib during Python runner initialization
    """

    run_context = RunContext(c_run_context)
    descriptor = ModuleDescriptor(c_descriptor)

    stdlib_loader = _priv_make_module_loader_func_for_pymod("hyperon.stdlib")
    stdlib_loader(run_context, descriptor)

    # #LP-TODO-Next Make a test for loading a metta module from a python module using load_module_direct_from_pymod
    # #LP-TODO-Next Also make a test for a module that loads another module
    # py_stdlib_id = run_context.metta().load_module_direct_from_pymod("stdlib-py", descriptor, "hyperon.stdlib")
    # run_context.import_dependency(py_stdlib_id)

    # #LP-TODO-Next See discussion above about whether we want to keep `extend-py!`
    # # If we keep it, it belongs in the Python stdlib, and not in runner
    # self.register_atom('extend-py!',
    #     OperationAtom('extend-py!',
    #                     lambda name: self.load_py_module_from_mod_or_file(name) or [],
    #                     [AtomType.UNDEFINED, AtomType.ATOM], unwrap=False))

def _priv_make_module_loader_func_for_pymod(pymod_name):
    """
    Private function to return a loader function to load a module into the runner directly from the specified Python module
    """

    def loader_func(run_context, descriptor):
        try:
            mod = import_module(pymod_name)

            # LP-TODO-Next, I should create an entry point that allows the python module to initialize the
            #  space before the rest of the init code runs
            space = GroundingSpaceRef()
            run_context.init_self_module(descriptor, space, None)

            #Load and import the core stdlib using "import *" behavior
            #QUESTION: Should the core stdlib be optional for python modules?
            core_stdlib_id = run_context.metta().load_core_stdlib("stdlib-core", descriptor)
            run_context.import_dependency(core_stdlib_id)

            for n in dir(mod):
                obj = getattr(mod, n)
                if '__name__' in dir(obj) and obj.__name__ == 'metta_register':
                    obj(run_context)

        except:
            # LP-TODO-Next, need to create error pathway through C interface 
            raise RuntimeError("Error loading Python module: ", pymod_name)

    return loader_func
