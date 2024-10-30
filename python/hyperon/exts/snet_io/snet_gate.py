from hyperon import *
from hyperon.ext import register_atoms
import os
from snet import sdk
from hyperon import *


class SNetSDKWrapper:

    def __init__(self):
        self.snet_sdk = None

    def init_sdk(self,
                 private_key=os.getenv("SNET_PRIVATE_KEY", '0' * 32),
                 eth_rpc_endpoint=os.getenv("ETH_RPC_ENDPOINT"),
                 email=os.getenv("SNET_EMAIL"),
                 concurrency=False,
                 force_update=False):
        self.email = email
        config = sdk.config.Config(private_key=private_key,
                                   eth_rpc_endpoint=eth_rpc_endpoint,
                                   concurrency=concurrency,
                                   force_update=force_update)
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
            # group_name="default_group",
            email=self.email,
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
        elif command == 'create_service_space':
            return self.create_service_space(*args, **kwargs)
        elif command == 'open_channel_and_deposit':
            return args[0].open_channel_and_deposit(*args[1:], **kwargs)
        elif command == 'organization_list':
            return list(map(lambda x: ValueAtom(x), self.organization_list()))
        elif command == 'service_list':
            return list(map(lambda x: ValueAtom(x), self.service_list(*args, **kwargs)))
        elif command == 'create_service_client':
            service_client = self.create_service_client(*args, **kwargs)
            return [service_client.get_operation_atom()]
        return [E(S('Error'), E(S('snet-sdk'), command_a, *args_a),
                  ValueAtom(f'unknown command {repr(command_a)}'))]

def pretty_print_atoms(input_atoms):
    len_threshold = 50
    current_len = 0
    def process_svg_atom(atom):
        nonlocal len_threshold
        nonlocal current_len
        repr_atom = repr(atom)
        current_len += len(repr_atom)
        return repr_atom

    def check_len(depth):
        nonlocal len_threshold
        nonlocal current_len
        if current_len > len_threshold:
            current_len = 0
            return "\n" + "\t" * (depth - 1)
        else:
            return ""

    def process_atom(atom, depth):
        nonlocal len_threshold
        nonlocal current_len
        process_res = ""
        metatype = atom.get_metatype()
        if metatype == AtomKind.EXPR:
            len_to_last_eol_flag = current_len > 5
            current_len *= (depth <= 1) * (not len_to_last_eol_flag)
            process_res += ("\n" + "\t" * depth) * (
                    depth > 0) * len_to_last_eol_flag + f"({process_expr_atom(atom, depth + 1)})"
        elif (metatype == AtomKind.SYMBOL) or (metatype == AtomKind.VARIABLE) or (metatype == AtomKind.GROUNDED):
            process_res += process_svg_atom(atom) + check_len(depth)
        else:
            raise Exception(f"Unexpected type of the Atom: {str(metatype)}")
        return process_res

    def process_expr_atom(expr_atom, depth):
        sub_atoms = expr_atom.get_children()
        process_res = ""
        for sub_atom in sub_atoms:
            process_atom_res = process_atom(sub_atom, depth)
            process_res += process_atom_res + check_len(depth)
            process_res += " "
        return process_res[:-1]

    res_string = "(" * (not (input_atoms[0].get_metatype() == AtomKind.EXPR))
    for atom in input_atoms:
        res_string += process_atom(atom, 0)
        res_string += "\n\n"
        current_len = 0
    return res_string

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

    def open_channel_and_deposit(self, amount, expiration):
        self.service_client.deposit_and_open_channel(amount, expiration)
        return [E()]

    def generate_callers_text(self):
        # TODO: pretty print
        return "\n".join([repr(e) for e in self.generate_callers()])

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