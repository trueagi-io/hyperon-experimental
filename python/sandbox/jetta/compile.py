from hyperon import MeTTa, OperationAtom, E, S, V
from hyperon.ext import register_atoms
import requests
import json

'''
This is the very preliminary wrapper to the Kotlin-based
MeTTa compiler, Jetta. For this gate to work, Jetta
should be installed via clonning https://github.com/trueagi-io/jetta
and running Application.kt of `server` subproject.
'''

default_url_base = 'http://0.0.0.0:9090/contexts'

def jetta(metta, j_space, code, url=default_url_base):
    r = requests.post(url + "/" + j_space, data=code)
    if r.status_code != 200:
        return None
    r = json.loads(r.content.decode())
    if not r['isSuccess']:
        return r['messages']
    # metta.parse_single(r['result'])
    if r['type'] == 'java.lang.Integer':
        r['result'] = int(r['result'])
    return r['result']

def compile(metta: MeTTa, j_space, func, arity=None):
    typ = metta.space().query(
        E(S(':'), S(func), V('t'))
    )
    typ = list(typ)
    assert len(typ) < 2, "Non-deterministic types are not supported yet"
    # TODO: different arities can be tried
    if len(typ) == 0:
        typ = ""
        if arity is None:
            raise RuntimeError("If type is not defined, arity should be provided")
    else:
        typ = typ[0]['t']
        arity = len(typ.get_children()) - 2
        typ = f"(: {func} {repr(typ)})"

    f_args = E(S(func), *[V(f'x{i}') for i in range(arity)])
    res = metta.space().query(
        E(S('='), f_args, V('__r'))
    )
    res = list(res)
    assert len(res) == 1, "Functions with one equality are allowed for now"
    code = "(= " + repr(f_args) + "\n   " +\
          repr(res[0]['__r']) + ")"
    code = typ + "\n" + code
    jetta(metta, j_space, code)
    funcAtom = OperationAtom(func,
        lambda *args: jetta(metta, j_space,
                            f"({func} " + " ".join([repr(a) for a in args]) + ")"))
    metta.register_atom(func+'-gnd', funcAtom)
    return None

def jetta_space(url=default_url_base):
    r = requests.post(url)
    assert r.status_code == 200, "Failed to create jetta space"
    return r.content.decode()

@register_atoms(pass_metta=True)
def jettaspace_atoms(metta: MeTTa):
    newJSpaceAtom = OperationAtom('new-jetta-space', jetta_space)
    jettaAtom = OperationAtom('jetta',
        lambda *args: jetta(metta, *args))
    compileAtom = OperationAtom('compile',
        lambda *args: compile(metta, *args))
    return {
        r"new-jetta-space": newJSpaceAtom,
        r"jetta": jettaAtom,
        r"compile": compileAtom
    }
