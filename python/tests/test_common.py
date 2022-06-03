from hyperon import atoms_are_equivalent

def assert_atoms_are_equivalent(test, actual, expected):
    test.assertEqual(len(actual), len(expected),
        "Actual and expected contains different number of atoms")
    for (actual, expected) in zip(actual, expected):
        test.assertTrue(atoms_are_equivalent(actual, expected),
            "Lists of atoms are not equivalent. " +
            "First pair of different atoms:\n- {}\n+ {}".format(actual, expected))
