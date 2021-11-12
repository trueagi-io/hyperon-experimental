from setuptools import setup, Command

class CoverageCommand(Command):
    user_options = []

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self):
        import sys, subprocess
        raise SystemExit(subprocess.run([ sys.executable, "-m", "nose",
            "--with-coverage", "--cover-erase", "--cover-package=hyperon",
            "--cover-html", "./tests" ]))

setup(name='hyperon',
      version='0.1',
      description='Hyperon API in Python',
      packages=['hyperon'],
      install_requires=[],
      extras_require={
          'dev': [
              'nose',
              'coverage'
          ]
      },
      cmdclass={
          'coverage': CoverageCommand,
          }
     )
