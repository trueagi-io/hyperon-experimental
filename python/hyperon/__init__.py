from .atoms import *
from .base import *
from .runner import *

try:
    from ._version import __version__ as _ver
    __version__ = _ver
except Exception:
    from pathlib import Path
    path = Path(__file__).parent / "../VERSION"
    with path.open() as f: ver = f.read().splitlines()[0].split("'")[1]
    __version__ = ver + "+localbuild"