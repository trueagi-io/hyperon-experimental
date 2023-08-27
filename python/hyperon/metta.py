"""
This is the MeTTa entrypoint
"""
import sys
import argparse

from hyperon import MeTTa

def main():
    """
	usage: metta.py [-h] metta file

	Metta script interpreter

	positional arguments:
		metta file  metta script

	optional arguments:
		-h, --help  show this help message and exit

    """
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

if __name__ == '__main__':
    main()
