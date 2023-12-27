from hyperon import *
import re


def get_string_value(value) -> str:
    if not isinstance(value, str):
        value = repr(value)
    if len(value) > 2 and ("\"" == value[0]) and ("\"" == value[-1]):
        return value[1:-1]
    return value


class RegexMatchableObject(MatchableObject):
    ''' To match atoms with regular expressions'''
    def __init__(self, content, id=None):
        super().__init__(content, id)

        self.content = self.content.replace("[[", "(").replace("]]", ")").replace("~", " ")

    def match_text(self, text, regexpr):
        return re.search(pattern=regexpr, string=text.strip(), flags=re.IGNORECASE)

    def match_(self, atom):
        pattern = self.content
        text = get_string_value(atom)
        text = ' '.join([x.strip() for x in text.split()])
        if pattern.startswith("regex:"):
            pattern = get_string_value(pattern[6:])
            matched = self.match_text(text, pattern)
            if matched is not None:
                return [{"matched_pattern": S(pattern)}]
        return []


def RegexMatchableAtom(value, type_name=None, atom_id=None):
    return G(RegexMatchableObject(value, atom_id), AtomType.UNDEFINED)