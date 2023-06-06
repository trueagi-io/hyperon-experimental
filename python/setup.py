import shlex
from setuptools import setup, Command

class CoverageCommand(Command):
    user_options = []

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self):
        import sys, subprocess

        result = subprocess.run(
            shlex.split("coverage run --source hyperon -m unittest discover"),
            check=False,
            cwd="./tests",
        )
        subprocess.run(shlex.split("coverage html"), check=True, cwd="./tests")
        raise SystemExit(result.returncode)

try:

    from wheel.bdist_wheel import bdist_wheel as _bdist_wheel

    class bdist_wheel(_bdist_wheel):
        def finalize_options(self):
            _bdist_wheel.finalize_options(self)
            self.root_is_pure = False

except ImportError:
    bdist_wheel = None

setup(
    cmdclass={
        "coverage": CoverageCommand,
        "bdist_wheel": bdist_wheel,
    },
)
