import subprocess

def get_git_revision_short_hash() -> str:
    return subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode('ascii').strip()

def current_version():
    sha = get_git_revision_short_hash()
    return '0.1.8'+'+'+sha

__version__ = current_version()