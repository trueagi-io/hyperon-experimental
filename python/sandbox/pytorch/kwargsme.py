from hyperon.atoms import *


class Kwargs(MatchableObject):
    def __init__(self, content=None, id=None):
        super().__init__(content, id)
        if content is None:
            self.content = {}

    def __len__(self):
        return len(self.content)

    def match_(self, other):
        new_bindings_set = BindingsSet.empty()
        p = other.get_children()
        if isinstance(p[0], SymbolAtom):
            key = p[0].get_name()
            var = p[1]
            if key in self.content:
                val = ValueAtom(self.content[key])
                bindings = Bindings()
                bindings.add_var_binding(var, val)
                new_bindings_set.push(bindings)

        return new_bindings_set


def pairs_to_kwargs(pairs):
    kwargs = Kwargs()
    pairs_children = pairs.get_children()
    for pair in pairs_children:
        p = pair.iterate()

        if isinstance(p[0], SymbolAtom):
            key = p[0].get_name()
            if isinstance(p[1], GroundedAtom):
                kwargs.content[key] = p[1].get_object().value
            elif isinstance(p[1], SymbolAtom):
                v = p[1].get_name()
                if v == 'None':
                    kwargs.content[key] = None
                else:
                    kwargs.content[key] = v

    return [G(GroundedObject(kwargs))]
