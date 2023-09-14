"""
This is the MeTTa entrypoint
"""
import sys
import argparse
import hyperon

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
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        '--version', action='store_true', help='prints the version')
    group.add_argument(
        'file', nargs='?', metavar="MeTTa script", help='the MeTTa script')
    args = parser.parse_args()
    if args.version:
        print(hyperon.__version__)
    elif args.file:
        metta = hyperon.MeTTa()
        for result in metta.import_file(args.file):
            print(result)
    else:
        parser.print_usage()

if __name__ == '__main__':
    main()
