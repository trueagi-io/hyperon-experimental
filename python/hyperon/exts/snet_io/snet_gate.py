from hyperon import *
from hyperon.ext import register_atoms
import os
from snet import sdk
from hyperon import *

class SNetSDKWrapper:

    def __init__(self):
        self.snet_sdk = None

    def init_sdk(self,
                 private_key=os.getenv("SNET_PRIVATE_KEY", '0'*32),
                 eth_rpc_endpoint=os.getenv("ETH_RPC_ENDPOINT"),
                 email=os.getenv("SNET_EMAIL"),
                 identity_name="hyperon",
                 network="mainnet",
                 identity_type="key",
                 concurrency=False,
                 force_update=False):
        config = {
            "private_key": private_key,
            "eth_rpc_endpoint": eth_rpc_endpoint,
            "email": email,
            "concurrency": concurrency,
            "identity_name": identity_name,
            "network": network,
            "identity_type": identity_type,
            "force_update": force_update
        }
        self.snet_sdk = sdk.SnetSDK(config)

    def organization_list(self):
        return self.snet_sdk.get_organization_list()
    
    def service_list(self, org_id):
        return self.snet_sdk.get_services_list(org_id)

    def create_service_client(self, org_id, service_id,
                              free_call_auth_token_bin=os.getenv("FREE_CALL_AUTH_TOKEN_BIN", None),
                              free_call_token_expiry_block=os.getenv("FREE_CALL_TOKEN_EXPIRE_BLOCK", None)):
        if free_call_token_expiry_block is not None:
            free_call_token_expiry_block = int(free_call_token_expiry_block)
        service_client = self.snet_sdk.create_service_client(
            org_id=org_id, service_id=service_id,
            #group_name="default_group",
            free_call_auth_token_bin=free_call_auth_token_bin,
            free_call_token_expiry_block=free_call_token_expiry_block)
        return ServiceCall(service_client)

    def _unwrap_atom(self, atom):
        if isinstance(atom, GroundedAtom):
            return atom.get_object().content
        return repr(atom)

    def create_service_space(self, org_id, service_id, **kwargs):
        space = GroundingSpaceRef()
        service_client = self.create_service_client(org_id, service_id, **kwargs)
        space.add_atom(E(S('='), E(S(org_id), S(service_id)),
                   service_client.get_operation_atom()))
        atoms = service_client.generate_callers()
        for atom in atoms:
            space.add_atom(atom)
            if isinstance(atom, ExpressionAtom) and atom.get_children()[0] == S(':'):
                space.add_atom(E(S('='), E(S(org_id), S(service_id), S('methods')), atom))
        return [G(space)]

    def __call__(self, command_a, *args_a):
        command = self._unwrap_atom(command_a)
        args = []
        kwargs = {}
        try:
            for arg_a in args_a:
                if isinstance(arg_a, ExpressionAtom):
                    ch = arg_a.get_children()
                    k = ch[0].get_name()
                    v = self._unwrap_atom(ch[1])
                    kwargs[k] = v
                else:
                    args += [self._unwrap_atom(arg_a)]
        except:
            return [E(S('Error'), E(S('snet-sdk'), command_a, *args_a),
                      ValueAtom(f'argument error'))]
        if command == 'init':
            self.init_sdk(*args, **kwargs)
            return [E()]
        if self.snet_sdk is None:
            self.init_sdk()
        if command == 'get_service_callers':
            return args[0].generate_callers()
        if command == 'get_service_callers_text':
            return args[0].generate_callers_text()
        if command == 'create_service_space':
            return self.create_service_space(*args, **kwargs)
        if command == 'organization_list':
            return list(map(lambda x: ValueAtom(x), self.organization_list()))
        if command == 'service_list':
            return list(map(lambda x: ValueAtom(x), self.service_list(*args, **kwargs)))
        if command == 'create_service_client':
            service_client = self.create_service_client(*args, **kwargs)
            return [service_client.get_operation_atom()]
        return [E(S('Error'), E(S('snet-sdk'), command_a, *args_a),
                    ValueAtom(f'unknown command {repr(command_a)}'))]

class ServiceCall:

    def __init__(self, service_client):
        self.service_client = service_client
        self.message_info = self.service_client.get_services_and_messages_info()
        self.service_details = self.service_client.get_service_details()
        methods = list(self.message_info[0].values())[0]
        self.inputs = []
        self.outputs = []
        self.io_types = []
        self.func_names = []
        for method in methods:
            self.func_names += [method[0]]
            types = method[1:]
            self.io_types += [types]
            self.inputs += [self.message_info[1][types[0]]]
            self.outputs += [self.message_info[1][types[1]]]

    def __call__(self, method, input_type, **kwargs):
        method_index = self.func_names.index(method)
        service_result = self.service_client.call_rpc(method, input_type, **kwargs)
        res_list = []
        for output in self.outputs[method_index]:
            res_list += [getattr(service_result, output[1])]
        if len(res_list) > 1:
            return res_list
        return res_list[0]

    def get_service_details(self):
        return self.service_details

    def get_service_messages(self):
        return self.message_info

    def get_operation_atom(self):
        return OperationAtom(self.service_details[1], self)

    def __pretty_print__(self, input_str):
        list_of_functions = ["if", "match", "let", "let*", "unify",
                             "assertEqualToResult", "assertEqual", "remove-atom", "add-reduct",
                             "case"] # list should be possibly extended if new functions with complicated body appears in the future
        str_symbols = ["'", '"', "'''"] # if we met function name, but it was in quotes, we shouldn't treat it like real function
        special_symbols = [" ", "\t", "\n"] # to read word symbol by symbol till special symbol appears
        line_threshold = 30 #threshold after which line break will be inserted
        len_to_previous_break = 0  # to prevent duplicate \n symbols
        def pp_func(str, num_of_tabs_outer):
            pp_func_res = ""
            _num_of_brackets = 0
            _num_of_symbols_in_line = 0
            j = 0
            _len_to_previous_break = len_to_previous_break
            while j < len(str):
                _symbol = str[j]
                _add_symbol = _symbol
                _num_of_symbols_in_line += 1
                if (_symbol == '('):
                    if (_num_of_brackets == 0) and (_len_to_previous_break > (num_of_tabs_outer + 2)):
                        _add_symbol = "\n" + "\t" * num_of_tabs_outer + _add_symbol
                        _num_of_symbols_in_line = 0
                        _len_to_previous_break = 0
                    _num_of_brackets += 1
                elif (_symbol == ')'):
                    _num_of_brackets -= 1
                    if _num_of_brackets < 0:  # we are out of function's body
                        pp_func_res += str[j:]
                        break
                    #in this case num_of_brackets means that we have reached end of inner (...). For example, in 'if' at leas condition is in brackets so we're putting line break there
                    elif (_num_of_brackets == 0) and (_len_to_previous_break > (num_of_tabs_outer + 2)):
                        if str[j + 1] in special_symbols:
                            j += 1
                        _add_symbol += "\n" + "\t" * num_of_tabs_outer
                        _num_of_symbols_in_line = 0
                        _len_to_previous_break = 0
                pp_func_res += _add_symbol
                j += 1
                _len_to_previous_break += 1
            return pp_func_res

        num_of_tabs = 0 # currently num of tabs is strictly depends on number of '(' symbols
        num_of_symbols_in_line = 0
        pretty_res = ""
        str_detected = {"'": False, '"': False, "'''": False} # to check if we are inside of quotes or not
        word_memory = ""
        input_str = ' '.join(input_str.split()) # to remove every possible spaces duplication
        i = 0 # since input_str can be altered during parsing process I need while not for loop
        while i < len(input_str):
            symbol = input_str[i]
            add_symbol = symbol
            num_of_symbols_in_line += 1
            if symbol in str_symbols:
                word_memory = ""
                str_detected[symbol] = not str_detected[symbol]
            elif symbol in special_symbols: # word ended so we need to check if this is function
                if word_memory in list_of_functions:
                    pp_func_res = pp_func(input_str[i:], num_of_tabs) # every function separately treated
                    input_str = input_str.replace(input_str[i + 1:], pp_func_res) # though we have ran pp_func over current function's body it is not full so we need to loop through renewed str_input
                    num_of_symbols_in_line = 0
                word_memory = ""
                if symbol == "\n":
                    num_of_symbols_in_line = 0
                    len_to_previous_break = 0
                elif symbol == "\t":
                    num_of_symbols_in_line -= 1
            elif symbol == '(':
                word_memory = ""
                num_of_tabs += 1
                # we will put a line break not for every '(', only if number of symbols in line exceeded threshold. PLus, to prevent several consequent line breaks, we're checking distance to previous line break
                if (num_of_symbols_in_line > line_threshold) and (len_to_previous_break > (num_of_tabs + 2)):
                    add_symbol = "\n" + "\t" * (num_of_tabs - 1) + add_symbol
                    num_of_symbols_in_line = 0
                    len_to_previous_break = 0
            elif symbol == ')':
                word_memory = ""
                num_of_tabs -= 1
                # if num_of_tabs == 0 then it should be the end of the current expression (for example, we have function's type and function definition in one string).
                if (num_of_tabs == 0) and (len_to_previous_break > (num_of_tabs + 2)):
                    add_symbol = add_symbol + "\n"
                    num_of_symbols_in_line = 0
                    len_to_previous_break = 0
            else:
                str_flag = False
                for key in str_detected:
                    str_flag = str_flag or str_detected[key]
                # we are adding something to a memory only if this is not a part of string
                word_memory += symbol * (not str_flag)
            pretty_res += add_symbol
            i += 1
            len_to_previous_break += 1
        pretty_res = pretty_res.replace("  ", " ")
        pretty_res = pretty_res.replace("\t ", "\t")
        pretty_res = pretty_res.replace("\n ", "\n")
        return pretty_res

    def generate_callers_text(self):
        res = "\n".join([repr(e) for e in self.generate_callers()])
        return self.__pretty_print__(res)

    def _map_type(self, t):
        type_map = {'bool': 'Bool',
                    'string': 'String',
                    'int32': 'Number',
                    'float': 'Number'}
        return type_map[t] if t in type_map else t

    def generate_callers(self):
        atoms = []
        for i in range(len(self.func_names)):
            func_name = self.func_names[i]
            metta_fun_type = []
            metta_fun_type.extend([S(':'), S(f'{func_name}')])
            type_symbols_in = []
            type_symbols_out = []
            fun_header = [S(f'{func_name}')]
            kwargs = [S('Kwargs')]
            for var_tuple in self.inputs[i]:
                type_symbols_in.append(S(self._map_type(var_tuple[0])))
                fun_header.append(V(f'{var_tuple[1]}'))
                kwargs.append(E(S(f'{var_tuple[1]}'), V(f'{var_tuple[1]}')))
            for var_tuple in self.outputs[i]:
                type_symbols_out.append(S(self._map_type(var_tuple[0])))
            if (len(self.outputs[i]) > 1):
                metta_fun_type.extend([E(S('->'), *(type_symbols_in), E(*(type_symbols_out)))])
            else:
                metta_fun_type.extend([E(S('->'), *(type_symbols_in), *(type_symbols_out))])
            metta_fun_type = E(*(metta_fun_type))
            kwargs = E(*kwargs)
            fun_header = E(*fun_header)
            function_expr = E(S('='), fun_header,
                              E(self.get_operation_atom(),
                                ValueAtom(func_name), ValueAtom(self.io_types[i][0]), kwargs))
            atoms += [metta_fun_type, function_expr]
        return atoms

@register_atoms()
def snet_atoms():
    defaultSDKAtom = OperationAtom("snet-sdk", SNetSDKWrapper(), unwrap=False)
    # TODO: new-sdk-atom
    return {
        'snet-sdk': defaultSDKAtom,
    }
