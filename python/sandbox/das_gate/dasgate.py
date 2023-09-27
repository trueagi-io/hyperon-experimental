from hyperon import *
from hyperon.ext import register_atoms

import os
import re
import json

from hyperon_das import DasAPI
from hyperon_das.das import QueryOutputFormat as Format
from hyperon_das.pattern_matcher import (
    Link,
    Node,
    Variable,
    And,
    Or,
    Not,
    PatternMatchingAnswer,
)

'''
def to_nested_expr(xs):
    if isinstance(xs, list):
        return E(*list(map(to_nested_expr, xs)))
    return ValueAtom(xs)

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
'''

class DASpace(AbstractSpace):

    def __init__(self, unwrap=True):
        super().__init__()
        self.das = DasAPI('hash_table')
        self.unwrap = unwrap

    def _atom2dict(self, atom):
        if isinstance(atom, ExpressionAtom):
            return {
                "type": "Expression",
                "targets": [self._atom2dict(ch) for ch in atom.get_children()]
            }
        else:
            return {
                "type": "Symbol",
                "name": repr(atom)
            }

    def _atom2query(self, atom):
        if isinstance(atom, ExpressionAtom):
            return Link("Expression", ordered=True,
                targets=[self._atom2query(ch) for ch in atom.get_children()])
        else:
            if isinstance(atom, VariableAtom):
                return Variable(repr(atom))
            else:
                return Node("Symbol", repr(atom))

    def query(self, query_atom):
        query = self._atom2query(query_atom)
        answer = PatternMatchingAnswer()
        new_bindings_set = BindingsSet.empty()
        if not query.matched(self.das.db, answer):
            return new_bindings_set
        for a in answer.assignments:
            bindings = Bindings()
            for var, val in a.mapping.items():
                # remove '$', because it is automatically added
                bindings.add_var_binding(V(var[1:]), S(self.das.get_node_name(val)))
            new_bindings_set.push(bindings)
        return new_bindings_set

        # Extract only the variables from the query atom
        query_vars = list(filter(lambda atom: atom.get_type() == AtomKind.VARIABLE, query_atom.iterate()))

        # Match the query atom against every atom in the space
        # BindingsSet() creates a binding set with the only matching result
        # We use BindingsSet.empty() to support multiple results
        for space_atom in self.atoms_list:
            match_results = space_atom.match_atom(query_atom)

            # Merge in the bindings from this match, after we narrow the match_results to
            # only include variables vars in the query atom 
            for bindings in match_results.iterator():
                bindings.narrow_vars(query_vars)
                if not bindings.is_empty():
                    # new_bindings_set.merge_into(bindings) would work with BindingsSet(), but
                    # it would return an empty result for multiple alternatives and merge bindings
                    # for different variables from alternative branches, which would be a funny
                    # modification of query, but with no real use case
                    # new_bindings_set.push(bindings) adds an alternative binding to the binding set
                    new_bindings_set.push(bindings)

        return new_bindings_set

    def add(self, atom):
        dc = self._atom2dict(atom)
        if "name" in dc:
            self.das.add_node(dc)
        else:
            self.das.add_link(dc)

    #def remove(self, atom):
    #    pass
    #def replace(self, from_atom, to_atom):
    #    pass
    #def atom_count(self):
    #    return self.das.count_atoms()
    #def atoms_iter(self):
    #    return iter(self.atoms_list)



@register_atoms(pass_metta=True)
def das_atoms(metta):
    newDASpaceAtom = OperationAtom('new-das', lambda: [G(SpaceRef(DASpace()))], unwrap=False)
    #llmAtom = OperationAtom('llm', lambda *args: llm(metta, *args), unwrap=False)
    return {
        r"new-das": newDASpaceAtom,
        #r"new-intent-space": newISpaceAtom,
        #r"llm": llmAtom
    }

