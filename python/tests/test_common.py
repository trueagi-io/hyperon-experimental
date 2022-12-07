import unittest

from hyperon import atoms_are_equivalent

def isEqualNoOrder(a, b):
    class FakeHash:
        def __init__(self, o):
            self.o = o

        def __hash__(self):
            return 0

        def __eq__(self, other):
            return self.o.__eq__(other.o)

    mapa = dict()
    mapb = dict()
    for e in a:
        e = FakeHash(e)
        mapa[e] = mapa.get(e, 0) + 1
    for e in b:
        e = FakeHash(e)
        mapb[e] = mapb.get(e, 0) + 1
    for (k, v) in mapa.items():
        if mapb.get(k, 0) != v:
            return False
    for (k, v) in mapb.items():
        if mapa.get(k, 0) != v:
            return False
    return True

def isEqualMettaRunResults(a, b):
    if len(a) != len(b):
        return False
    for (a, b) in zip(a, b):
        if not isEqualNoOrder(a, b):
            return False
    return True

class HyperonTestCase(unittest.TestCase):

    def __init__(self, methodName):
        super().__init__(methodName)

    def assertEqualNoOrder(self, left, right):
        self.assertTrue(isEqualNoOrder(left, right),
            f"Lists differ: {left} != {right}")

    def assertEqualMettaRunnerResults(self, left, right):
        self.assertTrue(isEqualMettaRunResults(left, right),
            f"MeTTa results differ: {left} != {right}")

    def assertAtomsAreEquivalent(self, actual, expected):
        self.assertEqual(len(actual), len(expected),
                "Actual and expected contains different number of atoms:" +
                f"\n{actual}\n{expected}")
        for (actual, expected) in zip(actual, expected):
            self.assertTrue(atoms_are_equivalent(actual, expected),
                "Lists of atoms are not equivalent. " +
                f"First pair of different atoms:\n- {actual}\n+ {expected}")
