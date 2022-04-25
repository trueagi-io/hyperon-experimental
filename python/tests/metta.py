import os
import sys

from common import MeTTa

if __name__ == "__main__":
    os.system('clear')
    print("\n========= MeTTa version 0.0 =========\n\n")
    metta = MeTTa()
    metta.import_file(sys.argv[1])
