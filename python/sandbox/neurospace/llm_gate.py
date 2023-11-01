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

def get_message_list(msg_atoms):
    '''
    Convert atoms to ChatGPT messages and flatten a possibly nested message list
    '''
    messages = []
    for msg in msg_atoms:
        if isinstance(msg, ExpressionAtom):
            ch = msg.get_children()
            if len(ch) == 0:
                continue
            if ch[0].get_name() == 'Messages':
                messages += get_message_list(ch[1:])
            else:
                messages += [{"role": ch[0].get_name(), "content": atom2msg(ch[1])}]
        else:
            raise TypeError("Messages should be tagged by the role")
    return messages

def llm(metta: MeTTa, *args):
    messages = []
    functions = []
    msgs = None
    for arg in args:
        if isinstance(arg, ExpressionAtom):
            ch = arg.get_children()
            if len(ch) > 1 and ch[0].get_name() == 'Messages':
                msgs = arg
                messages += get_message_list(ch[1:])
            if len(ch) > 1 and ch[0].get_name() == 'Functions':
                for fn in ch[1:]:
                    doc = metta.run(f"! (doc {fn})")
                    if len(doc) == 0:
                        # TODO: error / warning
                        continue
                    # TODO: format is not checked
                    doc = doc[0][0].get_children()
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
        return [E(fs, to_nested_expr(list(args.values())), msgs)]
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

