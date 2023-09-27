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
    return {
        r"new-das": newDASpaceAtom,
    }

