from hyperon import *
from hyperon.ext import register_atoms
import uuid
import requests
import json

'''
This is the very preliminary wrapper to MORK server
'''

default_url_base = 'http://127.0.0.1:8000'

class MORKServerError(RuntimeError):
    pass

class MORKSpace: # (AbstractSpace):

    def __init__(self, metta, name=None, url=None):
        super().__init__()
        self.metta = metta
        self.name = uuid.uuid4().hex if name is None or name == E() else name
        self.url = default_url_base if url is None or url == E() else url

    def __call__(self, method: SymbolAtom, *args):
        m = getattr(self, method.get_name())
        return m(*args)

    def status(self):
        r = requests.get(f"{self.url}/status/({self.name} $v)")
        return [ValueAtom(json.loads(r.text)['status'])]

    # TODO: pattern
    def import_uri(self, uri):
        uri = uri.get_object().content
        r = requests.get(f"{self.url}/import/$v/({self.name} $v)/?uri={uri}")
        return [ValueAtom(r.status_code == 200)]

    def export(self, query, output):
        r = requests.get(f"{self.url}/export/({self.name} {query})/{output}")
        return self.metta.parse_all(r.text)

    def query(self, query_atom):
        print("query: ", query_atom)
        new_bindings_set = BindingsSet.empty()
        return new_bindings_set

    # TODO + upload
    def add(self, atom):
        #r = requests.post(url + "/upload/$v/(my_space $v)", data="(parent $x $x) (parent C $y)")
        pass

    def __repr__(self):
        return f"{self.url}/{self.name}"


def mork_space_atom(metta, *args):
    mork = MORKSpace(metta, *args)
    #s = mork.status()
    #if s != 'pathClear': ...
    #return [G(SpaceRef(mork))]
    return [OperationAtom(f"mork:{mork.name}", mork, unwrap=False)]


@register_atoms(pass_metta=True)
def morkspace_atoms(metta: MeTTa):
    MORKSpaceAtom = OperationAtom('mork-space',
        lambda *args: mork_space_atom(metta, *args), unwrap=False)
    return {
        r"mork-space": MORKSpaceAtom,
    }
