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
        result = subprocess.run(shlex.split("coverage run --source hyperon -m unittest discover"), check=False,
                       cwd='./tests')
        subprocess.run(shlex.split("coverage html"), check=True, cwd='./tests')
        raise SystemExit(result.returncode)

from pathlib import Path
this_directory = Path(__file__).parent
long_description = (this_directory / "../README.md").read_text()


setup(name='hyperon',
      version='0.1.2',
      description='Hyperon API in Python',
      long_description_content_type="text/markdown",
      long_description=long_description,
      packages=['hyperon'],
      install_requires=[],
      extras_require={
          'dev': [
              'coverage'
          ]
      },
      cmdclass={
          'coverage': CoverageCommand,
          }
     )
