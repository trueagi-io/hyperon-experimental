import sys
import os
package_path = os.path.dirname(__file__)
sys.path.insert(0, package_path)

from .atoms import *
from .base import *
from .runner import MeTTa

if sys.version_info[:2] >= (3, 8):
    # TODO: Import directly (no need for conditional) when `python_requires = >= 3.8`
    from importlib.metadata import PackageNotFoundError, version  # pragma: no cover
else:
    from importlib_metadata import PackageNotFoundError, version  # pragma: no cover

try:
    # Change here if project is renamed and does not equal the package name
    dist_name = __name__
    __version__ = version(dist_name)
except PackageNotFoundError:  # pragma: no cover
    __version__ = "unknown"
finally:
    del version, PackageNotFoundError

def main():
    import argparse
    parser = argparse.ArgumentParser(description='Metta script interpreter')
    parser.add_argument(
        'file', help='metta script')
    args = parser.parse_args()

    metta = MeTTa()
    for result in metta.import_file(args.file):
        print(result)
