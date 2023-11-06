from hyperon import *
from hyperon.ext import register_atoms
import openai
import os
import json
openai.api_key = os.environ["OPENAI_API_KEY"]

def to_nested_expr(xs):
    if isinstance(xs, list):
        return E(*list(map(to_nested_expr, xs)))
    return ValueAtom(xs)

def atom2msg(atom):
    if isinstance(atom, ExpressionAtom):
        # Avoid () in Expression representation
        txt = ""
        for ch in atom.get_children():
            txt += atom2msg(ch) + " "
        return txt[:-1] + "\n"
    if isinstance(atom, GroundedAtom):
        if isinstance(atom.get_grounded_type(), ExpressionAtom):
            return repr(atom)
        if isinstance(atom.get_object(), ValueObject):
            # Parse String separately to avoid "" in its repr
            v = atom.get_object().value
            if isinstance(v, str):
                return v.replace("\\n", "\n")
    return repr(atom)

def get_llm_args(metta: MeTTa, prompt_space: SpaceRef, *args):
    messages = []
    functions = []
    msg_atoms = []
    def __msg_update(m, f, a):
        nonlocal messages, functions, msg_atoms
        messages += m
        functions += f
        msg_atoms += [a]
    for arg in args:
        if isinstance(arg, GroundedAtom) and \
           isinstance(arg.get_object(), SpaceRef):
            # FIXME? This will overwrites the current prompt_space if it is set.
            # It is convenient to have it here to successfully execute
            # (llm &prompt (Functions fn)), when fn is defined in &prompt.
            # But (function fn) can also be put in &prompt directly.
            # Depending on what is more convenient, this overriding can be changed.
            prompt_space = arg.get_object()
            __msg_update(*get_llm_args(metta, prompt_space, *prompt_space.get_atoms()))
        elif isinstance(arg, ExpressionAtom):
            ch = arg.get_children()
            if len(ch) > 1:
                name = ch[0].get_name()
                if name == 'Messages':
                    __msg_update(*get_llm_args(metta, prompt_space, *ch[1:]))
                elif name in ['system', 'user', 'assistant']:
                    # We have to interpret the message in the main space context,
                    # if the prompt template is in a separate file and contains
                    # some external symbols like (user-query)
                    msg = interpret(metta.space(), ch[1])[0]
                    messages += [{'role': name, 'content': atom2msg(msg)}]
                    msg_atoms += [arg]
                elif name in ['Functions', 'function']:
                    for fn in ch[1:]:
                        doc = None
                        if prompt_space is not None:
                            # TODO: Querying for a function description in prompt_space works well,
                            # but it is useless, because this function cannot be called
                            # from the main script, so the functional call is not reduced.
                            # Fixing this requires in general better library management in MeTTa,
                            # although it can be managed here by interpreting the functional call expression.
                            # Another approach would be to have load-template, which will import all functions to &self
                            # (or just to declare function in separate files and load to self, since we may want them
                            # to be reusable between templates)
                            r = prompt_space.query(E(S('='), E(S('doc'), fn), V('r')))
                            if not r.is_empty():
                                doc = r[0]['r']
                        if doc is None:
                            # We use `match` here instead of direct `doc` evaluation
                            # to evoid non-reduced `doc`
                            doc = metta.run(f"! (match &self (= (doc {fn}) $r) $r)")
                            if len(doc) == 0 or len(doc[0]) == 0:
                                raise RuntimeError(f"No {fn} function description")
                            doc = doc[0][0]
                        # TODO: format is not checked
                        doc = doc.get_children()
                        properties = {}
                        for par in doc[2].get_children()[1:]:
                            p = par.get_children()
                            properties.update({
                                p[0].get_name(): {
                                    "type": "string",
                                    "description": p[1].get_object().value,
                                    "enum": list(map(lambda x: x.get_object().value, p[2].get_children()))
                                }
                            })
                        functions += [{
                            "name": fn.get_name(),
                            "description": doc[1].get_children()[1].get_object().value,
                            "parameters": {
                                "type": "object",
                                "properties": properties
                            }
                        }]
                elif name == '=':
                    # We ignore equalities here: if a space is used to store messages,
                    # it can contain equalities as well (another approach would be to
                    # ignore everythins except valid roles)
                    continue
                else:
                    raise RuntimeError("Unrecognized argument: " + repr(arg))
            else:
                # Ignore an empty expression () for convenience, but we need
                # to put it back into msg_atoms to keep the structure
                msg_atoms += [arg]
        else:
            raise RuntimeError("Unrecognized argument: " + repr(arg))
    # Do not wrap a single message into Message (necessary to avoid double
    # wrapping of single Message argument)
    return messages, functions, \
        msg_atoms[0] if len(msg_atoms) == 1 else E(S('Messages'), *msg_atoms)

def llm(metta: MeTTa, *args):
    messages, functions, msgs_atom = get_llm_args(metta, None, *args)
    #print(messages)
    #return []
    if functions==[]:
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo-0613",
            messages=messages,
            temperature=0,
            timeout = 15)
    else:
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo-0613",
            messages=messages,
            functions=functions,
            function_call="auto",
            temperature=0,
            timeout = 15)
    response_message = response["choices"][0]["message"]
    #print(response_message)
    #messages.append(response_message)
    if response_message.get("function_call"):
        fs = S(response_message["function_call"]["name"])
        args = response_message["function_call"]["arguments"]
        args = json.loads(args)
        return [E(fs, to_nested_expr(list(args.values())), msgs_atom)]
    return [ValueAtom(response_message['content'])]

@register_atoms(pass_metta=True)
def llmgate_atoms(metta):
    llmAtom = OperationAtom('llm', lambda *args: llm(metta, *args), unwrap=False)
    # Just a helper function if one needs to print from a metta-script
    # the message converted from expression to text
    msgAtom = OperationAtom('atom2msg',
                 lambda atom: [ValueAtom(atom2msg(atom))], unwrap=False)
    return {
        r"llm": llmAtom,
        r"atom2msg": msgAtom
    }


def str_find_all(str, values):
    return list(filter(lambda v: v in str, values))

@register_atoms
def postproc_atoms():
    strfindAtom = OperationAtom('str-find-all', str_find_all)
    return {
        r"str-find-all": strfindAtom,
    }
