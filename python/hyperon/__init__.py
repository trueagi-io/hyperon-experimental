from .atoms import *
from .base import *
from .runner import MeTTa

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

