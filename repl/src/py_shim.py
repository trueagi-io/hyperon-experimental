
from hyperon import *

def init_metta(working_dir, include_paths):
    Environment.init_common_env(working_dir = working_dir, include_paths = include_paths)
    return MeTTa()

def load_metta_module(metta, mod_path):
    metta.import_file(mod_path)

def start_run(metta, program):
    return metta.start_run(program)

def run_is_complete(runner_state):
    return runner_state.is_complete()

def run_step(runner_state):
    runner_state.run_step()
    return runner_state.current_results()

def parse_line(metta, line):
    tokenizer = metta.tokenizer()
    parser = SExprParser(line)
    while True:
        try:
            parsed_atom = parser.parse(tokenizer)
            if (parsed_atom is None):
                return None
        except SyntaxError as e:
            return e.args[0]

def parse_line_to_syntax_tree(line):
    leaf_node_types = [];
    parser = SExprParser(line)
    while True:
        syntax_node = parser.parse_to_syntax_tree()
        if syntax_node is None:
            break
        else:
            leaf_node_list = syntax_node.unroll()
            for node in leaf_node_list:
                leaf_node_types.append((node.get_type(), node.src_range()))
    return leaf_node_types

def get_config_dir():
    return Environment.config_dir()

def get_config_atom(metta, config_name):
    result = metta.run("!(get-state " + config_name + ")")
    try:
        atom = result[0][0]
        if (atom_is_error(atom)):
            return None
        else:
            return atom
    except:
        return None

def get_config_expr_vec(metta, config_name):
    try:
        atom = get_config_atom(metta, config_name)
        return atom.get_children()
    except:
        return None

def get_config_string(metta, config_name):
    atom = get_config_atom(metta, config_name)
    if atom is not None:
        return atom.__repr__()
    else:
        return None
