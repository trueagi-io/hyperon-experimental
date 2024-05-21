import os
import sys
import shutil
import subprocess
from pathlib import Path

from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext

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
        local_prefix = os.path.join(os.environ["HOME"], ".local")

        cmake_args = [
            f"-DCMAKE_LIBRARY_OUTPUT_DIRECTORY={extdir}{os.sep}",
            f"-DPython3_EXECUTABLE={sys.executable}",
            f"-DCMAKE_BUILD_TYPE={cfg}",
            f"-DCMAKE_PREFIX_PATH={local_prefix}"
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


def get_version(rel_path):
    try:
        with open(rel_path) as f:  ver = f.read().splitlines()[0].split("'")[1]
        return ver
    except Exception:
        print(f"Error reading file {rel_path}", file=sys.stderr)


def version_scheme(*args):
    return get_version("./VERSION")

setup(
    ext_modules=[CMakeExtension("hyperonpy")],
    cmdclass={"build_ext": CMakeBuild},
    use_scm_version={'root': '..',
                     'version_scheme': version_scheme,
                     'write_to': 'python/hyperon/_version.py'},
 )
