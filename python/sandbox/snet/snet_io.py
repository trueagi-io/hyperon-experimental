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
        if command == 'organization_list':
            return list(map(lambda x: ValueAtom(x), self.organization_list()))
        if command == 'service_list':
            return list(map(lambda x: ValueAtom(x), self.service_list(*args, **kwargs)))
        if command == 'create_service_client':
            service_client = self.create_service_client(*args, **kwargs)
            return [OperationAtom(service_client.get_service_details()[1], service_client)]
        return [E(S('Error'), E(S('snet-sdk'), command_a, *args_a),
                    ValueAtom(f'unknown command {repr(command_a)}'))]

class ServiceCall:
    def __init__(self, service_client):
        self.service_client = service_client
        self.message_info = self.service_client.get_services_and_messages_info()
        self.inputs = []
        self.outputs = []
        self.keys = []
        self.func_name = ""
        self.service_details = self.service_client.get_service_details()
        for key in self.message_info[0]:
            val = self.message_info[0][key]
            self.func_name = val[0][0]
            self.keys = list(val[0])
            self.keys.pop(0)
            self.inputs.extend(self.message_info[1][self.keys[0]])
            self.outputs.extend(self.message_info[1][self.keys[1]])

    def __call__(self, method, input_type, **kwargs):
        service_result = self.service_client.call_rpc(method, input_type, **kwargs)
        res_list = []
        for output in self.outputs:
            res_list += [getattr(service_result, output[1])]
        if (len(res_list) > 1):
            return res_list
        return res_list[0]

    def get_service_details(self):
        return self.service_details

    def get_service_messages(self):
        return self.message_info

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
        service_id = self.service_details[1]
        metta_fun_type = []
        metta_fun_type.extend([S(':'), S(f'{self.func_name}')])
        type_symbols_in = []
        type_symbols_out = []
        fun_header = [S(f'{self.func_name}')]
        kwargs = [S('Kwargs')]
        for var_tuple in self.inputs:
            type_symbols_in.append(S(self._map_type(var_tuple[0])))
            fun_header.append(V(f'{var_tuple[1]}'))
            kwargs.append(E(S(f'{var_tuple[1]}'), V(f'{var_tuple[1]}')))
        for var_tuple in self.outputs:
            type_symbols_out.append(S(self._map_type(var_tuple[0])))
        if (len(self.outputs) > 1):
            metta_fun_type.extend([E(S('->'), *(type_symbols_in), E(*(type_symbols_out)))])
        else:
            metta_fun_type.extend([E(S('->'), *(type_symbols_in), *(type_symbols_out))])
        metta_fun_type = E(*(metta_fun_type))
        kwargs = E(*kwargs)
        fun_header = E(*fun_header)
        function_expr = E(S('='), fun_header,
                          E(E(S(service_id)), ValueAtom(self.func_name), ValueAtom(self.keys[0]), kwargs))
        return [metta_fun_type, function_expr]

@register_atoms()
def snet_atoms():
    defaultSDKAtom = OperationAtom("snet-sdk", SNetSDKWrapper(), unwrap=False)
    # TODO: new-sdk-atom
    return {
        'snet-sdk': defaultSDKAtom,
    }
