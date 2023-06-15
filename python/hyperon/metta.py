import sys
import argparse

from hyperon import MeTTa

def main():
    parser = argparse.ArgumentParser(description='Metta script interpreter')
    parser.add_argument(
        'file', metavar="metta file", help='metta script')
    if len(sys.argv) > 1:
        args = parser.parse_args()
        metta = MeTTa()
        for result in metta.import_file(args.file):
            print(result)
    else:
        parser.print_usage()
