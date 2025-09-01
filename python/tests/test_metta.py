import unittest

from hyperon import *

class MettaTest(unittest.TestCase):

    def test_adding_tokens_while_parsing(self):
        metta = MeTTa(env_builder=Environment.test_env())

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
        runner = MeTTa(env_builder=Environment.test_env())
        result = runner.run(program)

        self.assertEqual([[S('T')]], result)

    def test_metta_evaluate_atom(self):
        program = '''
            (= (And T T) T)
            (= (frog $x)
                (And (croaks $x)
                     (eat_flies $x)))
            (= (croaks Fritz) T)
            (= (eat_flies Fritz) T)
            (= (green $x) (frog $x))
        '''
        runner = MeTTa(env_builder=Environment.test_env())
        runner.run(program)
        result = runner.evaluate_atom(E(S('green'), S('Fritz')))

        self.assertEqual([S('T')], result)

    def test_incremental_runner(self):
        program = '''
            !(+ 1 (+ 2 (+ 3 4)))
        '''
        runner = MeTTa(env_builder=Environment.test_env())
        runner_state = RunnerState(runner, program)

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
        runner = MeTTa(env_builder=Environment.test_env())
        result = runner.run(program)

        self.assertEqual(runner.run('!(Error (+ 2 "String") (BadType argument 2 expected Number got String))'), result)

    def test_runner_error(self):
        program = '''
          !(+ 2 3
        '''
        runner = MeTTa(env_builder=Environment.test_env())
        try:
            runner.run(program)
            self.assertTrue(False, "Parse error expected")
        except RuntimeError as e:
            self.assertEqual(e.args[0], 'Unexpected end of expression')

    def test_match_with_rust_grounded_atom(self):
        program = '''
          ; True is used as a Python grounded object
          (grounded True)
          ; import! is used as a Rust grounded object which can be
          ; received from Python
          !(match &self (grounded import!) Ok)
        '''
        runner = MeTTa(env_builder=Environment.test_env())
        result = runner.run(program)

        self.assertEqual([[]], result)

    def test_metta_evaluate_atom_using_stdlib(self):
        program = '''
            (= (f) (let ($x $y) (A B) $x))
        '''
        runner = MeTTa(env_builder=Environment.test_env())
        runner.run(program)

        result = runner.run('!(f)')
        self.assertEqual([[S('A')]], result)

        result = runner.evaluate_atom(E(S('f')))
        self.assertEqual([S('A')], result)
