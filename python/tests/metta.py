import os
import sys
from hyperonpy import init_logger


from common import MeTTa

if __name__ == "__main__":
    os.system('clear')
    init_logger()
    print("\n========= MeTTa version 0.0 =========\n\n")
    metta = MeTTa()
    metta.import_file(sys.argv[1])
