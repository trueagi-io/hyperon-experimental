import unittest

from hyperon import *

class MettaTest(unittest.TestCase):

    def test_adding_tokens_while_parsing(self):
        metta = MeTTa()

        atom = metta.parse_single('(A B)')
        self.assertEqual(atom, E(S('A'), S('B')))

        metta.register_atom('A', S('C'))
        atom = metta.parse_single('(A B)')
        self.assertEqual(atom, E(S('C'), S('B')))

        metta.register_atom('A', S('F'))
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

    def test_incremental_runner(self):
        program = '''
            !(+ 1 (+ 2 (+ 3 4)))
        '''
        runner = MeTTa()
        runner_state = runner.start_run(program)

        step_count = 0
        while not runner_state.is_complete():
            runner_state.run_step()
            step_count += 1

        results = runner_state.current_results()
        self.assertEqual(repr(results), "[[10]]")

    def test_gnd_type_error(self):
        program = '''
          !(+ 2 "String")
        '''
        runner = MeTTa()
        result = runner.run(program)

        self.assertEqual([[E(S('Error'), ValueAtom('String'), S('BadType'))]], result)
