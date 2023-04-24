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

setup(name='hyperon',
      setup_requires=['setuptools_scm'],
      use_scm_version = {
          "root": "..",
          "relative_to": __file__,
          "local_scheme": "node-and-timestamp"
      },
      description='Hyperon API in Python',
      long_description_content_type="text/markdown",
      long_description='Hyperon API in Python',
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
