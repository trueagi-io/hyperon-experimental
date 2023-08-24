from hyperon import *
from hyperon.ext import register_atoms
import openai
import os
import re
openai.api_key = os.environ["OPENAI_API_KEY"]

def _response2bindings(txt):
        res = re.findall(r'\{.*?\}', txt)
        new_bindings_set = BindingsSet.empty()
        if res == []:
            return new_bindings_set
        res = res[0][1:-1]
        _var, val = res.split(':')
        var = re.findall(r'\".*?\"', _var)
        var = var[0][1:-1] if len(var) > 0 else _var.replace(" ", "")
        if var[0] == '$':
            var = var[1:]
        var = V(var)
        try:
            val = ValueAtom(int(val))
            bindings = Bindings()
            bindings.add_var_binding(var, val)
            new_bindings_set.push(bindings)
        except ValueError:
            ss = re.findall(r'\".*?\"', val)
            if ss == []:
                ss = ['"' + val + '"']
            for s in ss:
                val = S(s[1:-1])
                bindings = Bindings()
                bindings.add_var_binding(var, val)
                new_bindings_set.push(bindings)
        return new_bindings_set

class NeuralSpace(GroundingSpace):
    def query(self, query_atom):
        tot_str = "Answer the question taking into account the following information (each fact is in brackets):\n"
        for atom in self.atoms_iter():
            tot_str += str(atom) + "\n"
        tot_str += "If the question contains letters in brackets with $ sign, for example ($x), provide the answer in the json format in curly brackets, that is { $x: your answer }.\n"
        # tot_str += "If information is not provided, return the entry to be queried in JSON {unknown value: UNKNOWN}."
        tot_str += "The question is: " + str(query_atom)[1:-1] + "?"
        response = openai.ChatCompletion.create(
                model="gpt-3.5-turbo-0613",
                messages=[{'role': 'system', 'content': 'Reason carefully about user request'},
                    {'role': "user", "content": tot_str}],
                temperature=0)
        txt = response['choices'][0]['message']['content']
        return _response2bindings(txt)

class IntentSpace(GroundingSpace):
    def query(self, query_atom):
        tot_str = "Analyze the topic of the utterance: " + str(query_atom)[1:-1] + "\n"
        tot_str += "Try to pick the most relevant topic from the following list (each topic in brackets):"
        for atom in self.atoms_iter():
            tot_str += str(atom) + "\n"
        tot_str += "If neither of the listed topics seems relevant, answer (chit-chat)."
        tot_str += "Provide the answer in the json format in curly brackets in the form { topic: your answer }.\n"
        response = openai.ChatCompletion.create(
                model="gpt-3.5-turbo-0613",
                messages=[{'role': 'system', 'content': 'Reason carefully about user request'},
                    {'role': "user", "content": tot_str}],
                temperature=0)
        txt = response['choices'][0]['message']['content']
        return _response2bindings(txt)

@register_atoms
def neuralspace_atoms():
    newNSpaceAtom = OperationAtom('new-neural-space', lambda: [G(SpaceRef(NeuralSpace()))], unwrap=False)
    newISpaceAtom = OperationAtom('new-intent-space', lambda: [G(SpaceRef(IntentSpace()))], unwrap=False)
    return {
        r"new-neural-space": newNSpaceAtom,
        r"new-intent-space": newISpaceAtom,
    }

