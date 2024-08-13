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
        email=os.getenv("SNET_EMAIL", "test@mail.test.com"),
        free_call_auth_token_bin=os.getenv("FREE_CALL_AUTH_TOKEN_BIN", None),
        free_call_token_expiry_block=os.getenv("FREE_CALL_TOKEN_EXPIRE_BLOCK", 20685151)
      ):
    # group_name="default_group"
    config = {
        "private_key": private_key,
        "eth_rpc_endpoint": eth_rpc_endpoint,
        "email": email,
        "free_call_auth_token-bin": free_call_auth_token_bin,
        "free-call-token-expiry-block": free_call_token_expiry_block,
        "concurrency": False,
        "org_id": org_id,
        "service_id": service_id,
        "identity_name": "test",
        "identity_type": "key",
        # "force_update"=True
    }
    snet_sdk = sdk.SnetSDK(config)
    service_client = snet_sdk.create_service_client()#, group_name)
    return ServiceCall(service_client)

@register_atoms()
def snet_atoms():
    serviceAtom = OperationAtom("snet-service", import_service)
    return { 'snet-service': serviceAtom }
