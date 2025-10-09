import os
from importlib import import_module
import importlib.util
import sys
import site
import hyperonpy as hp
from .atoms import Atom, AtomType, OperationAtom
from .base import GroundingSpaceRef, Tokenizer, SExprParser
from .ext import RegisterType
from .module import MettaModRef
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

    def init_self_module(self, space, resource_dir):
        """Must be called exactly once from within a module loader to initialize the module being loaded"""
        hp.run_context_init_self_module(self.c_run_context, space.cspace, resource_dir)
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

    def load_module(self, mod_name):
        """Resolves a module by name in the context of the running module, and loads it into the runner"""
        return hp.run_context_load_module(self.c_run_context, mod_name)

    def register_token(self, regexp, constr):
        """Registers a token in the currently running module's Tokenizer"""
        self.tokenizer().register_token(regexp, constr)

    def register_atom(self, name, symbol):
        """Registers an Atom with a name in the currently running module's Tokenizer"""
        self.register_token(name, lambda _: symbol)

    def import_dependency(self, mod_id):
        """Imports a loaded module as a dependency of the running module"""
        if mod_id.is_valid():
            hp.run_context_import_dependency(self.c_run_context, mod_id)
        else:
            raise RuntimeError("Invalid ModuleId")

class MeTTa:
    """This class represents the runner to execute MeTTa programs"""

    def __init__(self, cmetta = None, space = None, env_builder = None):

        if cmetta is not None:
            self.cmetta = cmetta
        else:
            if space is None:
                space = GroundingSpaceRef()
            if env_builder is None:
                env_builder = hp.env_builder_start()
                hp.env_builder_set_default_config_dir(env_builder)
            hp.env_builder_push_fs_module_format(env_builder, _PyFileMeTTaModFmt)
            #LP-TODO-Next, add an fs_module_fmt arg to the standardized way to init environments, so that
            # the Python user can define additional formats without tweaking any hyperon files.  To make
            # this convenient it probably means making a virtual ModuleFormat base class

            builtin_mods_path = os.path.join(os.path.dirname(__file__), 'exts')
            hp.env_builder_push_include_path(env_builder, builtin_mods_path)

            py_site_packages_paths = site.getsitepackages()
            for path in py_site_packages_paths:
                hp.env_builder_push_include_path(env_builder, path)

            self.cmetta = hp.metta_new_with_stdlib_loader(_priv_load_module_stdlib, space.cspace, env_builder)

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

    def working_dir(self):
        """Returns the working dir from the environment associated with the runner"""
        return hp.metta_working_dir(self.cmetta)

    def register_token(self, regexp, constr):
        """Registers a token"""
        self.tokenizer().register_token(regexp, constr)

    # FIXME: for operation atoms name is passed twice: first argument and
    # field of the OperationAtom
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

    def load_module_direct_from_func(self, mod_name, loader_func):
        """Loads a module into the runner using a loader function, with the specified name and scope"""
        mod_id = hp.metta_load_module_direct(self.cmetta, mod_name, loader_func)
        err_str = hp.metta_err_str(self.cmetta)
        if (err_str is not None):
            raise RuntimeError(err_str)
        return mod_id

    def load_module_direct_from_pymod(self, mod_name, pymod_name):
        """Loads a module into the runner directly from a Python module, with the specified name and scope"""
        if not isinstance(pymod_name, str):
            pymod_name = repr(pymod_name)
        def loader_func(tokenizer, metta):
            _priv_register_module_tokens(pymod_name, tokenizer, metta)
        return self.load_module_direct_from_func(mod_name, loader_func)

    def load_module_at_path(self, path, mod_name=None):
        """
        Loads a module into the runner directly from resource at a file system path, trying the formats
        from the runner's environment in succession
        """
        mod_id = hp.metta_load_module_at_path(self.cmetta, path, mod_name)
        err_str = hp.metta_err_str(self.cmetta)
        if (err_str is not None):
            raise RuntimeError(err_str)
        return mod_id

    def run(self, program, flat=False):
        """Runs the MeTTa code from the program string containing S-Expression MeTTa syntax"""
        parser = SExprParser(program)
        results = hp.metta_run(self.cmetta, parser.cparser)
        self._run_check_for_error()
        if flat:
            return [Atom._from_catom(catom) for result in results for catom in result]
        else:
            return [[Atom._from_catom(catom) for catom in result] for result in results]

    def evaluate_atom(self, atom):
        result = hp.metta_evaluate_atom(self.cmetta, atom.catom)
        self._run_check_for_error()
        return [Atom._from_catom(catom) for catom in result]

    def _run_check_for_error(self):
        err_str = hp.metta_err_str(self.cmetta)
        if (err_str is not None):
            raise RuntimeError(err_str)

class Environment:
    """This class contains the API for configuring the host platform interface used by MeTTa"""

    def config_dir():
        """Returns the config dir in the common environment"""
        path = hp.environment_config_dir()
        if (len(path) > 0):
            return path
        else:
            return None

    def init_common_env(working_dir = None, config_dir = None, create_config = None, is_test = None, include_paths = []):
        """Initialize the common environment with the supplied args

        Keyword arguments:
            working_dir -- working directory for the environment (default None)
            config_dir -- path to the configuration directory, None - no
            configuration directory, "" - default configuration directory
            (default None)
            create_config -- create configuration directory if it doesn't exist
            (default None)
            is_test -- is environment used in unit-test flag (default None)
            include_paths -- additional search paths to search for MeTTa
            modules in the file system (default [])
        """
        builder = Environment.custom_env(working_dir, config_dir, create_config, is_test, include_paths)
        return hp.env_builder_init_common_env(builder)

    def test_env():
        """Returns an EnvBuilder object specifying a unit-test environment, that can be used to init a MeTTa runner"""
        return hp.env_builder_use_test_env()

    def custom_env(working_dir = None, config_dir = None, create_config = None, is_test = None, include_paths = []):
        """Returns an EnvBuilder object that can be used to init a MeTTa runner, if you need multiple environments to coexist in the same process

        Keyword arguments:
            working_dir -- working directory for the environment (default None)
            config_dir -- path to the configuration directory, None - no
            configuration directory, "" - default configuration directory
            (default None)
            create_config -- create configuration directory if not found
            (default None)
            is_test -- is environment used in unit-test flag (default None)
            include_paths -- additional search paths to search for MeTTa
            modules in the file system (default [])
        """
        builder = hp.env_builder_start()
        if working_dir is not None:
            hp.env_builder_set_working_dir(builder, working_dir)
        if config_dir is not None:
            if config_dir == "":
                hp.env_builder_set_default_config_dir(builder)
            else:
                hp.env_builder_set_config_dir(builder, config_dir)
        if create_config is not None:
            hp.env_builder_create_config_dir(builder, create_config)
        if is_test is not None:
            hp.env_builder_set_is_test(builder, is_test)
        for path in include_paths:
            hp.env_builder_push_include_path(builder, path)
        return builder

class _PyFileMeTTaModFmt:
    """This private class implements the loader for Python modules that implement MeTTa modules. 
    This logic covers both "*.py" files and directories that contain an "__init__.py"."""

    def path_for_name(parent_dir, metta_mod_name):
        """Construct a file path name based on the metta_mod_name"""
        file_name = metta_mod_name if metta_mod_name.endswith(".py") else metta_mod_name + ".py"
        return os.path.join(parent_dir, file_name)

    def try_path(path, metta_mod_name):
        """Load the file as a Python module if it exists"""

        #See if we have a ".py" file first, and if not, check for a directory-based python mod
        path = path if path.endswith(".py") else os.path.splitext(path)[0] + ".py"
        if not os.path.exists(path):
            dir_path = os.path.join(os.path.splitext(path)[0], "__init__.py")
            if os.path.exists(dir_path):
                path = dir_path
            else:
                return None

        #QUESTION: What happens if two modules in different files have the same name?
        # E.g. two different versions of the same module.  The MeTTa module system can
        # handle it, but it looks like there might be a collision at the Python level.
        # Should we try and get around this by making the python-module names unique
        # by mangling them when we load the python mods in this function?  Can we do
        # that or will it mess up other stuff Python programmers might be expecting?
        spec = importlib.util.spec_from_file_location(metta_mod_name, path)

        try:
            module = importlib.util.module_from_spec(spec)
            sys.modules[metta_mod_name] = module
            spec.loader.exec_module(module)

            #TODO: Extract the version here, when it's time to implement versions
            def loader_func(tokenizer, metta):
                _priv_register_module_tokens(metta_mod_name, tokenizer, metta)

            return {
                'pymod_name': metta_mod_name,
                'path': path,
                'fmt_id': 5000, #5000 is an arbitrary number unlikely to conflict with the arbitrary number chosen by other formats
                'loader_func': loader_func,
            }
        except Exception as e:
            hp.log_error("Python error loading MeTTa module '" + metta_mod_name + "'. " + repr(e))
            return None

def _priv_load_module(loader_func, path, c_run_context):
    """Initializes module, and loads the items from the python module into the runner as a MeTTa module"""
    run_context = RunContext(c_run_context)
    if path is not None:
        resource_dir = os.path.dirname(path)
    else:
        resource_dir = None
    space = GroundingSpaceRef()
    run_context.init_self_module(space, resource_dir)
    loader_func(run_context.tokenizer(), run_context.metta())

def _priv_load_module_tokens(loader_func, cmettamod, cmetta):
    """Loads the items from the python module into the tokenizer"""
    target = MettaModRef(cmettamod)
    tokenizer = target.tokenizer()
    metta = MeTTa(cmetta=cmetta)
    loader_func(tokenizer, metta)

def _priv_load_module_stdlib(tokenizer, metta):
    """Loads stdlib module"""
    _priv_register_module_tokens("hyperon.stdlib", tokenizer, metta)


# #LP-TODO-Next Make a test for loading a metta module from a python module using load_module_direct_from_pymod

# #LP-TODO-Next Also make a test for a module that loads another module
# py_stdlib_id = run_context.metta().load_module_direct_from_pymod("stdlib-py", descriptor, "hyperon.stdlib")
# run_context.import_dependency(py_stdlib_id)

# #LP-TODO-Next Implement a Catalog that uses the Python module-space, so that any module loaded via `pip`
# can be found by MeTTa.  NOTE: We may want to explicitly give priority hyperon "exts" by first checking if Python
# has a module at `"hyperon.exts." + mod_name` before just checking `mod_name`, but it's unclear that will
# matter since we'll also search the `exts` directory with the include_path / fs_module_format logic
# #UPDATE: If we implement a Python module-space Catalog in the future, then the code to search site packages
#  directories directly, in the 'MeTTa.__init__' method, needs to be removed

def _priv_register_module_tokens(pymod_name, tokenizer, metta):
    """
    Private function to load tokens from a Python module by its name
    """
    mod = import_module(pymod_name)
    for n in dir(mod):
        obj = getattr(mod, n)
        if 'metta_type' in dir(obj):
            typ = obj.metta_type
            if obj.metta_pass_metta:
                items = obj(metta)
            else:
                items = obj()
            if typ == RegisterType.ATOM:
                def register(r, a):
                    tokenizer.register_token(r, lambda _: a)
                for rex, atom in items.items():
                    register(rex, atom)
            elif typ == RegisterType.TOKEN:
                for rex, lam in items.items():
                    tokenizer.register_token(rex, lam)

