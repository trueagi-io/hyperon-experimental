import os
import codecs
import sys
import shutil
import subprocess
from pathlib import Path

from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext
import setuptools_scm as scm
import setuptools_scm.git as scmgit

def get_git_revision_short_hash() -> str:
    try:
        sha = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode('ascii').strip()
        return f"+{sha}"
    except Exception:
        return ''

def read(rel_path):
    here = os.path.abspath(os.path.dirname(__file__))
    with codecs.open(os.path.join(here, rel_path), 'r') as fp:
        return fp.read()

def get_version(rel_path):
    new_ver = None
    with open(rel_path) as f:  lines = f.read().splitlines()
    with open(rel_path, 'w') as f:
        for line in lines:
            if line.startswith('__version__'):
                delim = '"' if '"' in line else "'"
                new_ver = line.split(delim)[1].split("+")[0] + get_git_revision_short_hash()
                f.write(line.replace(line.split(delim)[1], new_ver))
            else:
                f.write(line)
    if new_ver is None:
        raise RuntimeError("Unable to find version string.")
    return new_ver

def resolve_path(path: str) -> str:
    return os.path.abspath(path)

class CMakeExtension(Extension):
    def __init__(self, name: str, sourcedir: str = '.', **kwa) -> None:
        super().__init__(name, sources=[], **kwa)
        self.sourcedir = resolve_path(sourcedir)

class CMakeBuild(build_ext):

    def build_extension(self, ext: CMakeExtension) -> None:
        if not self._copy_externally_built(ext):
            self._build_with_cmake(ext)

    def _copy_externally_built(self, ext: CMakeExtension) -> bool:
        target_path = resolve_path(self.get_ext_fullpath(ext.name))
        ext_filename = os.path.basename(target_path)
        source_path = os.path.join(ext.sourcedir, ext_filename)
        if os.path.isfile(source_path):
            os.makedirs(os.path.dirname(target_path), exist_ok=True)
            shutil.copyfile(source_path, target_path)
            return True
        else:
            return False

    def _build_with_cmake(self, ext: CMakeExtension) -> None:
        # Must be in this form due to bug in .resolve() only fixed in Python 3.10+
        ext_fullpath = Path.cwd() / self.get_ext_fullpath(ext.name)
        extdir = ext_fullpath.parent.resolve()
        debug = int(os.environ.get("DEBUG", 0)) if self.debug is None else self.debug
        cfg = "Debug" if debug else "Release"

        cmake_args = [
            f"-DCMAKE_LIBRARY_OUTPUT_DIRECTORY={extdir}{os.sep}",
            f"-DPython3_EXECUTABLE={sys.executable}",
            f"-DCMAKE_BUILD_TYPE={cfg}"
        ]
        build_args = []
        # Adding CMake arguments set as environment variable
        if "CMAKE_ARGS" in os.environ:
            cmake_args += [item for item in os.environ["CMAKE_ARGS"].split(" ") if item]

        # Set CMAKE_BUILD_PARALLEL_LEVEL to control the parallel build level
        # across all generators.
        if "CMAKE_BUILD_PARALLEL_LEVEL" not in os.environ:
            # self.parallel is a Python 3 only way to set parallel jobs by hand
            # using -j in the build_ext call, not supported by pip or PyPA-build.
            if hasattr(self, "parallel") and self.parallel:
                # CMake 3.12+ only.
                build_args += [f"-j{self.parallel}"]


        build_temp = Path(self.build_temp) / ext.name
        if not build_temp.exists():
            build_temp.mkdir(parents=True)

        subprocess.run(
            ["cmake", ext.sourcedir, *cmake_args], cwd=build_temp, check=True
        )
        subprocess.run(
            ["cmake", "--build", ".", *build_args], cwd=build_temp, check=True
        )


def parse(root, config):
    ver = scmgit.parse(root, config)
    if (ver == '0.0') or (ver is None):
        version = get_version("hyperon/__version__.py")
        return scm.version.meta(tag=version, config=config, preformatted=True, dirty=True)
    else:
        return ver

setup(
    ext_modules=[CMakeExtension("hyperonpy")],
    cmdclass={"build_ext": CMakeBuild},
    version=scm.get_version(parse=parse)
 )