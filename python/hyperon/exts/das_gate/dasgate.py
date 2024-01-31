from hyperon import *
from hyperon.ext import register_atoms


from hyperon_das import DistributedAtomSpace
from hyperon_das.constants import QueryOutputFormat
import time

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

    def __init__(self, remote=False, host='localhost', port='22', unwrap=True):
        super().__init__()
        # self.das = DistributedAtomSpace('ram_only')
        if remote:
            self.das = DistributedAtomSpace(query_engine='remote', host=host, port=port)
        else:
            self.das = DistributedAtomSpace()
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

    def _atom2dict_new(self, atom):
        if isinstance(atom, ExpressionAtom):
            targets = atom.get_children()
            if isinstance(targets[0], SymbolAtom) and targets[0].get_name() == ',':
                return [self._atom2dict_new(ch) for ch in targets[1:]]
            return {"atom_type": 'link', "type": "Expression", "targets":
                [self._atom2dict_new(ch) for ch in targets]}
        else:
            if isinstance(atom, VariableAtom):
                return {"atom_type": "variable", "name": repr(atom)}
            elif isinstance(atom, SymbolAtom):
                return {"atom_type": "node", "type": "Symbol", "name": repr(atom)}
            elif isinstance(atom, GroundedAtom):
                return {"atom_type": "node", "type": "Symbol", "name": repr(atom)}

    def _get_all_vars(self, atom):
        stack = [atom]
        while stack:
            node = stack.pop()
            if isinstance(node, VariableAtom):
                yield {"atom_type": "variable", "name": repr(node)}
            if isinstance(node, ExpressionAtom):
                targets = node.get_children()
                for ch in targets:
                    stack.append(ch)

    def _atom2query(self, atom):
        if isinstance(atom, ExpressionAtom):
            targets = atom.get_children()
            if isinstance(targets[0], SymbolAtom) and targets[0].get_name() == ',':
                return And([self._atom2query(ch) for ch in targets[1:]])
            return Link("Expression", ordered=True,
                targets=[self._atom2query(ch) for ch in targets])
        else:
            if isinstance(atom, VariableAtom):
                return Variable(repr(atom))
            else:
                return Node("Symbol", repr(atom))

    def _build_link_handle(self, link_type, target_handles):
        if link_type == 'Expression':
            target_handles.sort()
        return f'<{link_type}: {target_handles}>'

    def _get_link_targets(self, handle):
        all_links = self.das.get_links('Expression')
        for link in all_links:
            if self._build_link_handle(link[0], link[1:]) == handle:
                return link[1:]
        return None

    def _handle2atom(self, h):
        try:
            return S(self.das.backend.get_node_name(h))
        except Exception as e:
            return E(*[self._handle2atom(ch) for ch in self.das.backend.get_link_targets(h)])

    def _handle2atom2(self, h):
        try:
            return S(self.das.backend.get_node_name(h))
        except Exception as e:
            return E(*[self._handle2atom2(ch) for ch in self._get_link_targets(h)])

    def _handle2atom3(self, h):
        if h['type']=='Symbol':
            return S(h['name'])
        elif h['type']=='Expression':
            return E(*[self._handle2atom3(ch) for ch in h['targets']])

    def query(self, query_atom):
        query = self._atom2dict_new(query_atom)
        vars = [x for x in self._get_all_vars(query_atom)]

        query_params = {
            "toplevel_only": False,
            "return_type": QueryOutputFormat.ATOM_INFO,
            'query_scope': 'local_only'
        }
        answer = self.das.query(query,
                                query_params)
        new_bindings_set = BindingsSet.empty()
        if not answer:
            return new_bindings_set
        for a in answer:
            bindings = Bindings()
            if isinstance(a, list):
                b = a[0]
            else:
                b = a
            var = vars[0]['name'][1:]
            bindings.add_var_binding(V(var), self._handle2atom3(b))
            new_bindings_set.push(bindings)
        return new_bindings_set

    def query_old(self, query_atom):
        query = self._atom2query(query_atom)
        answer = self.das.pattern_matcher_query(query,
            {'return_type': QueryOutputFormat.HANDLE, 'toplevel_only': True})
        new_bindings_set = BindingsSet.empty()
        if answer is None:
            return new_bindings_set
        for a in answer['mapping']:
            bindings = Bindings()
            for var, val in a.mapping.items():
                # remove '$', because it is automatically added
                bindings.add_var_binding(V(var[1:]), self._handle2atom(val))
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



def create_new_space(host, port):
    return [G(SpaceRef(DASpace(remote=True, host=host.__repr__(), port=port.__repr__())))]

@register_atoms(pass_metta=True)
def my_glob_atoms(metta):
    return {
        r"new-remote-das": OperationAtom("new-remote-das", create_new_space, unwrap=False),
        }

@register_atoms(pass_metta=True)
def das_atoms(metta):
    newDASpaceAtom = OperationAtom('new-das', lambda: [G(SpaceRef(DASpace()))], unwrap=False)
    return {
        r"new-das": newDASpaceAtom,
    }

