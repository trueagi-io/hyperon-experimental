from hyperon import *
from hyperon.ext import register_atoms
import openai
import os
import re
openai.api_key = os.environ["OPENAI_API_KEY"]

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
        res = re.findall(r'\{.*?\}', txt)
        new_bindings_set = BindingsSet.empty()
        if res == []:
            return new_bindings_set
        res = res[0][1:-1]
        _var, val = res.split(':')
        var = re.findall(r'\".*?\"', _var)
        var = var[0][1:-1]
        if var[0] == '$':
            var = var[1:]
        var = V(var)
        try:
            val = ValueAtom(int(val))
            bindings = Bindings()
            bindings.add_var_bindings(var, val)
            new_bindings_set.push(bindings)
        except ValueError:
            ss = re.findall(r'\".*?\"', val)
            if ss == []:
                ss = ['"' + val + '"']
            for s in ss:
                val = S(s[1:-1])
                bindings = Bindings()
                bindings.add_var_bindings(var, val)
                new_bindings_set.push(bindings)
        return new_bindings_set


@register_atoms
def neuralspace_atoms():
    newNSpaceAtom = OperationAtom('new-neural-space', lambda: [G(SpaceRef(NeuralSpace()))], unwrap=False)
    return {
        r"new-neural-space": newNSpaceAtom,
    }

