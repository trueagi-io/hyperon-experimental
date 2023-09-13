import unittest

from hyperon import MeTTa, Atoms
from test_common import HyperonTestCase

from pathlib import Path
pwd = Path(__file__).parent

class MeTTaTest(HyperonTestCase):

    def test_run_metta(self):
        program = '''
            (isa red color)
            (isa green color)
            (isa blue color)
            ;(isa comment color)
            !(match &self (isa $color color) $color)

            (= (f) (+ 2 3))
            !(f)
        '''

        metta = MeTTa()
        self.assertEqualMettaRunnerResults(metta.run(program),
            [metta.parse_all('red  green  blue'), metta.parse_all('5')])

    def test_run_complex_query(self):
        program = '''
            (A B)
            (C B)

            !(match &self (, (A $x) (C $x)) $x)
        '''

        result = MeTTa().run(program)
        self.assertEqual('[[B]]', repr(result))

    def test_list_concatenation(self):
        program = '''
            (= (Concat (Cons $head1 Nil) $list2)
               (Cons $head1 $list2))

            (= (Concat (Cons $head1 (Cons $t1 $t11)) $list2)
               (Cons $head1 (Concat (Cons $t1 $t11) $list2)))

            (= (lst1) (Cons a1 (Cons a2 Nil)))
            (= (lst2) (Cons b1 (Cons b2 Nil)))
            !(Concat (lst1) (lst2))
        '''

        result = MeTTa().run(program)
        self.assertEqual('[[(Cons a1 (Cons a2 (Cons b1 (Cons b2 Nil))))]]', repr(result))

    def test_comments(self):
        program = '''
                (a ; 4)
                  5)
                !(match &self (a $W) $W)
            '''

        result = MeTTa().run(program)
        self.assertEqual('[[5]]', repr(result))

        program = '''
               (a  1);
               !(match
                        &self (a $W) $W)
           '''

        result = MeTTa().run(program)
        self.assertEqual('[[1]]', repr(result))

    def process_exceptions(self, results):
        for result in results:
            self.assertEqual(result, [])

    def test_scripts(self):
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/a1_symbols.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/a2_opencoggy.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/a3_twoside.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/b0_chaining_prelim.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/b1_equal_chain.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/b2_backchain.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/b3_direct.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/b4_nondeterm.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/b5_types_prelim.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/c1_grounded_basic.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/c2_spaces.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/c3_pln_stv.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/d1_gadt.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/d2_higherfunc.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/d3_deptypes.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/d4_type_prop.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/d5_auto_types.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/e1_kb_write.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/e2_states.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/e3_match_states.metta"))
        self.process_exceptions(MeTTa().import_file(f"{pwd}/scripts/f1_imports.metta"))

