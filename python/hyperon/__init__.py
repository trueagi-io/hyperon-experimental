from .atoms import *
from .base import *
from .runner import MeTTa

from .atoms import _priv_call_execute_on_grounded_atom
from .atoms import _priv_call_match_on_grounded_atom
from .base import _priv_call_query_on_python_space
from .base import _priv_call_add_on_python_space
from .base import _priv_call_remove_on_python_space
from .base import _priv_call_replace_on_python_space
from .base import _priv_call_atom_count_on_python_space
from .base import _priv_call_new_iter_state_on_python_space

def _version(dist_name):
    try:
        import sys
        if sys.version_info[:2] >= (3, 8):
            # TODO: Import directly (no need for conditional) when `python_requires = >= 3.8`
            from importlib.metadata import PackageNotFoundError, version  # pragma: no cover
        else:
            from importlib_metadata import PackageNotFoundError, version  # pragma: no cover
        return version(dist_name)
    except:
        return "unknown"

__version__ = _version(__name__)

