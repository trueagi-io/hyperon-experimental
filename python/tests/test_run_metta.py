import unittest

from common import MeTTa

class MeTTaTest(unittest.TestCase):

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

        result = MeTTa().run(program)
        self.assertEqual('[[red, green, blue], [5]]', repr(result))

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
        self.process_exceptions(MeTTa().import_file("scripts/a1_symbols.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/a2_opencoggy.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/a3_twoside.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/b0_chaining_prelim.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/b1_equal_chain.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/b2_backchain.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/b3_direct.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/b4_nondeterm.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/b5_types_prelim.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/c1_grounded_basic.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/c2_spaces.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/c3_pln_stv.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/d1_gadt.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/d2_higherfunc.metta"))
        self.process_exceptions(MeTTa().import_file("scripts/d3_deptypes.metta"))

