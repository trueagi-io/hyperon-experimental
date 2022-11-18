import unittest

from hyperon import *

class MettaTest(unittest.TestCase):

    def test_adding_tokens_while_parsing(self):
        metta = MeTTa()

        atom = metta.parse_single('(A B)')
        self.assertEqual(atom, E(S('A'), S('B')))

        metta.add_atom('A', S('C'))
        atom = metta.parse_single('(A B)')
        self.assertEqual(atom, E(S('C'), S('B')))

        metta.add_atom('A', S('F'))
        atom = metta.parse_single('(A B)')
        self.assertEqual(atom, E(S('F'), S('B')))

    def test_metta_runner(self):
        program = '''
            (= (And T T) T)
            (= (frog $x)
                (And (croaks $x)
                     (eat_flies $x)))
            (= (croaks Fritz) T)
            (= (eat_flies Fritz) T)
            (= (green $x) (frog $x))
            !(green Fritz)
        '''
        runner = MeTTa()
        result = runner.run(program)

        self.assertEqual([[S('T')]], result)

    @unittest.skip("TODO: panics because error cannot be returned from interpreter")
    def test_no_successful_alternatives(self):
        program = '''
          !(+ 2 "String")
        '''
        runner = MeTTa()
        result = runner.run(program)

        self.assertEqual([[]], result)
