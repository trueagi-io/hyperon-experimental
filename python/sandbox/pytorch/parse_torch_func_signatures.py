import torch
import re
import os
import site
import json


def parse_pyi_file(file_path):
    with open(file_path, "r") as f:
        content = f.read()

    all_func_names = re.findall(r'def (.*?)\(.*?\)', content)
    # remove duplicates
    all_func_names = list(dict.fromkeys(all_func_names))

    return all_func_names


def extract_signature(doc_string):
    # Regular expression pattern for Args and Keyword args
    pattern = r"(Args|Keyword args):(.*?)(?=(Args|Keyword args):|$)"
    try:
        matches = re.findall(pattern, doc_string, re.DOTALL)
    except TypeError as e:
        print(f"TypeError: {e}")

    arg_info = {}
    for keyword, match, _ in matches:
        sigs = match.split("\n")[1:-1]

        for sig in sigs:
            if not sig.strip():
                continue
            try:
                arg_name, arg_description = re.split(r"\s?\([A-Za-z ,]+\):", sig.strip())
                arg_info[arg_name] = {"description": arg_description.strip(), "type": keyword}
            except ValueError as e:
                print(f"ValueError: {e}")
                print(f"Could not split the argument signature: {sig}")
    return arg_info


def parse():
    site_packages_path = site.getsitepackages()[0]
    file_path = os.path.join(site_packages_path, 'torch/_C/_VariableFunctions.pyi')
    all_functions = parse_pyi_file(file_path)

    # Filter out names that don't start with "_" or "__", and don't end with "_" or "__"
    filtered_functions = []
    for f in all_functions:
        if not (f.startswith('_') or f.endswith('_')):
            filtered_functions.append(f)

    signatures = []
    for f in filtered_functions:
        try:
            func = getattr(torch, f)
        except AttributeError as e:
            print(f"AttributeError: {e}")
            continue

        d = func.__doc__
        if d is not None:
            s = extract_signature(d)
            signatures.append({'func_name': f, 'signature': s})
            print(s)

    json_data = json.dumps(signatures)
    with open('python/sandbox/pytorch/torch_func_signatures.json', 'w') as fp:
        fp.write(json_data)

    print(len(signatures))


if __name__ == "__main__":
    parse()
