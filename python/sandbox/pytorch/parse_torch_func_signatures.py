import torch
import re
import os
import site
import json

# Modify this file path according to your environment
TORCH_FUNC_SIGNATURES_SAVE_PATH = 'python/sandbox/pytorch/torch_func_signatures.json'


def parse_pyi_file(file_path):
    with open(file_path, "r") as f:
        content = f.read()

    all_func_names = re.findall(r'def (.*?)\(.*?\)', content)
    # remove duplicates
    all_func_names = list(dict.fromkeys(all_func_names))

    return all_func_names


def extract_signature(doc_string):
    # Regular expression pattern for Args and Keyword args
    # Stop matching when meet 'Returns:' or 'Example:'
    pattern = r"(Arguments|Args|Keyword args|Keyword arguments):(.*?)(?=(Arguments|Args|Keyword args|Keyword arguments):|Returns:|Example::|$)"
    ret_type = None
    matches=[]
    try:
        matches = re.findall(pattern, doc_string, re.DOTALL)
        m = re.search('-> (.+?)\n\n', doc_string)
        if m:
            ret_type = m.group(1)
            if ret_type != 'Tensor':
                print(ret_type)
    except TypeError as e:
        print(f"TypeError: {e}")

    arg_info = {}
    for keyword, match, _ in matches:
        sigs = match.split("\n")[1:-1]

        for sig in sigs:
            if not sig.strip():
                continue
            try:
                i = sig.rfind('Default:')
                if i > 0:
                    sig=sig[:i]
                arg_name, arg_description = re.split(r"\s?\([A-Za-z_:,`\. ]+\):| - |: ", sig.strip())
                arg_name = arg_name.split(" ")[0] # for cases like 'other (Tensor or Number)'
                arg_info[arg_name] = {"description": arg_description.strip(), "type": keyword}
            except ValueError as e:
                print(f"ValueError: {e}")
                print(f"Could not split the argument signature: {sig}")
    return arg_info, ret_type


def parse():
    signatures = []
    modules = [torch, torch.Tensor]

    for m in modules:
        for i in dir(m):
            attr = getattr(m, i)
            if hasattr(attr, '__name__') and hasattr(attr, '__doc__'):
                if attr.__doc__ is not None:
                # if (not (attr.__name__.startswith('_') or attr.__name__.endswith('_'))) and attr.__doc__ is not None:
                    s, ret_type = extract_signature(attr.__doc__)
                    if len(s) > 0:
                        signatures.append({'func_name': attr.__name__, 'ret_type': ret_type, 'signature': s, 'module': m.__name__})

    json_data = json.dumps(signatures)
    with open(TORCH_FUNC_SIGNATURES_SAVE_PATH, 'w') as fp:
        fp.write(json_data)

    print(f'Number of parsed signatures: {len(signatures)}')


if __name__ == "__main__":
    parse()
