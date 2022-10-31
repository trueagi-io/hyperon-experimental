import os
import sys

from common import MeTTa

if __name__ == "__main__":
    os.system('clear')
    print("\n========= MeTTa version 0.0 =========\n\n")
    metta = MeTTa()
     #for result in metta.import_file(sys.argv[1]):
    filename = '/home/aleksei/Downloads/hyperon-experimental/python/tests/scripts/test1.metta'
    for result in metta.import_file(filename):
        print(result)
