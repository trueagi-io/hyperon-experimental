from hyperon.atoms import *
from hyperon.ext import register_atoms
import os
from snet import sdk

class ServiceCall:
    def __init__(self, service_client):
        self.service_client = service_client
    def __call__(self, method, input_type, **kwargs):
        return self.service_client.call_rpc(method, input_type, **kwargs)

def import_service(org_id, service_id,
        private_key=os.getenv("SNET_PRIVATE_KEY"),
        eth_rpc_endpoint=os.getenv("ETH_RPC_ENDPOINT"),
        email=os.getenv("SNET_EMAIL", None),
        free_call_auth_token_bin=os.getenv("FREE_CALL_AUTH_TOKEN_BIN", None),
        free_call_token_expiry_block=os.getenv("FREE_CALL_TOKEN_EXPIRE_BLOCK", None)
      ):
    if free_call_token_expiry_block is not None:
        free_call_token_expiry_block = int(free_call_token_expiry_block)
    config = {
        "private_key": private_key,
        "eth_rpc_endpoint": eth_rpc_endpoint,
        "email": email,
        "concurrency": False,
        "identity_name": "hyperon",
        "network": "mainnet",
        "identity_type": "key",
        "force_update": False
    }
    snet_sdk = sdk.SnetSDK(config)

    service_client = snet_sdk.create_service_client(
        org_id=org_id, service_id=service_id,
        #group_name="default_group",
        free_call_auth_token_bin=free_call_auth_token_bin,
        free_call_token_expiry_block=free_call_token_expiry_block)
    return ServiceCall(service_client)

@register_atoms()
def snet_atoms():
    serviceAtom = OperationAtom("snet-service", import_service)
    return { 'snet-service': serviceAtom }
