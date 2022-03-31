import unittest

from hyperon import *
from common import interpret_until_result, Atomese, MeTTa, SpaceAtom

def interpret_and_print_results(target, kb, add_results_to_kb=False):
    output = ""
    while True:
        next = interpret_until_result(target, kb)
        if next == S('eos'):
            break
        print(next)
        output = output + str(next) + "\n"
        if add_results_to_kb:
            kb.add_atom(next)
    return output

class ExamplesTest(unittest.TestCase):

    def test_show_all_color_names(self):
        metta = MeTTa()

        metta.add_parse('''
            (isa red color)
            (isa green color)
            (isa blue color)
        ''')

        result = metta.interpret('(match &self (isa $color color) $color)')
        self.assertEqual([S('red'), S('green'), S('blue')], result)

    def test_create_semantic_triple(self):
        metta = MeTTa()

        kb = metta.add_parse('''
            (obj make pottery)
            (from make clay)
        ''')
        # Test a custom symbol for the space as well
        metta.add_atom("&kb", SpaceAtom(kb, "&kb"))

        result = metta.interpret('''
            (match &kb (obj $verb $var0)
                (q match &kb (from $verb $var1) (make_from $var0 $var1)))
        ''')
        self.assertEqual([metta.parse_single('(make_from pottery clay)')], result)

    def test_grounded_arithmetics(self):
        metta = MeTTa()

        metta.add_parse('''
            (= (foo $a $b) (* (+ $a $b) (+ $a $b)))
        ''')

        self.assertEqual([ValueAtom(49)],
                metta.interpret('(foo 3 4)'))
        # self.assertEqual(ValueAtom('Hello world'),
        #         metta.interpret("(+ 'Hello ' 'world')"))

    def test_grounded_functions(self):
        metta = MeTTa()
        obj = SomeObject()
        # using & as a prefix is not obligatory, but is naming convention
        metta.add_atom("&obj", ValueAtom(obj))

        target = metta.parse_single('(call:foo &obj)')
        # interpreting this target in another space still works,
        # because substitution '&obj' -> obj is done by metta
        result = interpret(GroundingSpace(), target)

        self.assertTrue(obj.called)
        self.assertEqual(result, [])

    def test_new_object(self):
        metta = MeTTa()
        pglob = Global(10)
        ploc = 10
        metta.add_token("pglob", lambda _: ValueAtom(pglob))
        metta.add_token("ploc", lambda _: ValueAtom(ploc))
        metta.add_token("Setter", lambda token: newNewAtom(token, Setter))
        metta.add_token("SetAtom", lambda token: newNewAtom(token, Setter, False))
        kb = GroundingSpace()
        # Just checking that interpretation of "pglob" gives us
        # a grounded atom that stores 10
        self.assertEqual(metta.interpret('pglob')[0].get_object().value.get(), 10)
        # Checking that:
        # - we create an atom on fly
        # - we change the value stored by the Python object
        # - interpretation of "pglob" will also give us this new value
        metta.interpret('(call:act (Setter pglob 5))')
        self.assertEqual(pglob.get(), 5)
        self.assertEqual(metta.interpret('pglob')[0].get_object().value.get(), 5)
        # Now check that "ploc" will not change, since
        # it is passed by value - not reference
        metta.interpret('(call:let (Setter ploc 5))')
        self.assertEqual(ploc, 10)
        self.assertEqual(metta.interpret('ploc')[0].get_object().value, 10)
        # Now we try to change the grounded atom value directly
        # (equivalent to metta.interpret but keeping target)
        target = metta.parse_single('(call:latom (SetAtom ploc 5))')
        interpret(metta.space, target)
        # "ploc" value in the "target" is changed
        self.assertEqual(target.get_children()[1].get_children()[1].get_object().value, 5)
        # But it is still not changed in another target, because
        # "ploc" creates ValueAtom(ploc) on each occurrence
        self.assertEqual(metta.interpret('ploc')[0].get_object().value, 10)
        # Another way is to return the same atom each time
        ploca = ValueAtom(ploc)
        metta.add_token("ploc", lambda _: ploca)
        # It will be not affected by assigning unwrapped values:
        # we are still copying values while unwrapping
        metta.interpret('(call:let (Setter ploc 5))')
        self.assertEqual(metta.interpret('ploc')[0].get_object().value, 10)
        self.assertEqual(ploca.get_object().value, 10)
        # However, it will be affected by assigning atom values
        metta.interpret('(call:latom (SetAtom ploc 5))')
        self.assertEqual(metta.interpret('ploc')[0].get_object().value, 5)
        self.assertEqual(ploca.get_object().value, 5)

    def test_frog_reasoning(self):
        metta = MeTTa()

        metta.add_parse('''
            (= (if True $then $else) $then)
            (= (if False $then $else) $else)
            (= (Fritz croaks) True)
            (= (Tweety chirps) True)
            (= (Tweety yellow) True)
            (= (Tweety eats_flies) True)
            (= (Fritz eats_flies) True)
        ''')

        fritz_frog = metta.interpret('(if (and ($x croaks) ($x eats_flies)) (= ($x frog) True) nop)')
        self.assertEqual([metta.parse_single('(= (Fritz frog) True)')], fritz_frog)
        metta.space.add_atom(fritz_frog[0])

        self.assertEqual([metta.parse_single('(= (Fritz green) True)')],
                metta.interpret('(if ($x frog) (= ($x green) True) nop)'))

    def test_frog_unification(self):
        metta = MeTTa()

        metta.add_parse('''
           (= (if True $then) $then)
           (= (frog $x) (and (croaks $x) (eat_flies $x)))
           (= (croaks Fritz) True)
           (= (eat_flies Fritz) True)
           (= (green $x) (frog $x))
        ''')

        self.assertEqual([metta.parse_single('Fritz')],
                metta.interpret('(if (green $x) $x)'))

    def test_air_humidity_regulator(self):
        metta = MeTTa()

        metta.add_parse('''
           (= (if True $then) $then)
           (= (make $x) (if (makes $y $x) (start $y)))
           (= (make $x) (if (and (prevents (making $y) (making $x))
                                   (makes $z $y)) (stop $z)))

           (= (is (air dry)) (make (air wet)))
           (= (is (air wet)) (make (air dry)))
           (= (prevents (making (air dry)) (making (air wet))) True)
           (= (prevents (making (air wet)) (making (air dry))) True)

           (= (makes humidifier (air wet)) True)
           (= (makes kettle (air wet)) True)
           (= (makes ventilation (air dry)) True)
        ''')

        output = metta.interpret('(is (air dry))')
        self.assertEqual(output, metta.parse_all('''
                (start humidifier)
                (start kettle)
                (stop ventilation)
                '''))

        output = metta.interpret('(is (air wet))')
        self.assertEqual(output, metta.parse_all('''
                (start ventilation)
                (stop humidifier)
                (stop kettle)
                '''))

    def test_subset_sum_problem(self):
        metta = MeTTa()

        metta.add_parse('''
           (= (if True $then $else) $then)
           (= (if False $then $else) $else)

           (= (bin) 0)
           (= (bin) 1)
           (= (gen $n) (if (> $n 0) (:: (bin) (gen (- $n 1))) nil))

           (= (subsum nil nil) 0)
           (= (subsum (:: $x $xs) (:: $b $bs)) (+ (* $x $b) (subsum $xs $bs)))
        ''')

        output = metta.interpret('''
            (let $t (gen 3)
                 (if (== (subsum (:: 3 (:: 5 (:: 7 nil))) $t) 8) $t (nop ())))
            ''')
        expected = metta.parse_single('(:: 1 (:: 1 (:: 0 nil)))')
        self.assertEqual(output, [expected])

    def test_infer_function_application_type(self):
        metta = MeTTa()

        metta.add_parse('''
           (= (if True $then) $then)

           (= (: (apply $f $x) $r) (and (: $f (=> $a $r)) (: $x $a)))

           (= (: reverse (=> String String)) True)
           (= (: "Hello" String) True)
        ''')

        output = metta.interpret('(if (: (apply reverse "Hello") $t) $t)')
        self.assertEqual(output, [S('String')])

    def test_plus_reduces_Z(self):
        metta = MeTTa()

        metta.add_parse('''
           (= (eq $x $x) True)
           (= (plus Z $y) $y)
           (= (plus (S $k) $y) (S (plus $k $y)))
        ''')

        output = metta.interpret('(eq (+ 2 2) 4)')
        self.assertEqual(output, [ValueAtom(True)])

        output = metta.interpret('(eq (+ 2 3) 4)')
        self.assertEqual(output, [metta.parse_single('(eq 5 4)')])

        output = metta.interpret('(eq (plus Z $n) $n)')
        self.assertEqual(output, [ValueAtom(True)])

        output = metta.interpret('(eq (plus (S Z) $n) $n)')
        self.assertEqual(output, [metta.parse_single('(eq (S $y) $y)')])

    def test_multi_space(self):
        # REM: it is not recommended to split code into multiple spaces, because
        # query chaining by the interpreter can behave in a tricky way
        # (putting data without equalities in a separate space and querying it
        # explicitly from another space should be safe, though)
        # REM: these tests are not indended to remain valid, but are needed to
        # detect, if something is changes in the interpreter
        metta1 = MeTTa()
        metta1.add_parse('''
            (= A B)
            (= (f-in-s2) failure)
            (= (how-it-works?) (f-in-s2))
            (= (inverse $x) (match &self (= $y $x) $y))
        ''')
        metta2 = MeTTa()
        metta2.add_atom("&space1", SpaceAtom(metta1.space, "&space1"))
        metta2.add_parse('''
            (= C B)
            (= (f-in-s2) success)
            (= (find-in $s $x) (match $s (= $y $x) $y))
            (= (borrow $s $e) (match $s (= $e $r) $r))
        ''')
        self.assertEqual(metta1.interpret('(inverse B)'), [S('A')])
        self.assertEqual(metta2.interpret('(find-in &space1 B)'), [S('A')])
        self.assertEqual(metta2.interpret('(find-in &self B)'), [S('C')])
        # `inverse` is successfully found in `&space1`
        # it resolves `&self` to metta1.space and matches against `(= A B)`
        self.assertEqual(metta2.interpret('(borrow &space1 (inverse B))'), [S('A')])
        # `borrow` executes `how-it-works?` in context of `&space1` via `match`
        # but then the interpreter evaluates `(how-it-works?)` via equality query
        # in the original metta2.space
        self.assertEqual(metta2.interpret('(borrow &space1 (how-it-works?))'), [S('success')])
        self.assertEqual(metta1.interpret('(how-it-works?)'), [S('failure')])

    def test_custom_deptypes(self):
        metta = MeTTa()
        metta.add_parse('''
            (= (:? $c)
               (match &self (:= $c $t) $t))
            (= (:? ($c $a))
               (match &self (:= ($c (:? $a)) $t) $t))
            (= (:? ($c $a $b))
               (match &self (:= ($c (:? $a) (:? $b)) $t) $t))

            (= (:check $c $t)
               (match &self (:= $c $t) T))
            (= (:check ($c $a) $t)
               (match &self (:= ($c (:? $a)) $t) T))
            (= (:check ($c $a $b) $t)
               (match &self (:= ($c (:? $a) (:? $b)) $t) T))

            (:= (= $t $t) Prop)

            (:= Entity Prop)
            (:= (Human Entity) Prop)
            (:= Socrates Entity)
            (:= Plato Entity)
            (: Time NotEntity)
            (:= (Mortal Entity) Prop)
            (:= (HumansAreMortal (Human $t)) (Mortal $t))
            (:= SocratesIsHuman (Human Socrates))
            (:= SocratesIsMortal (Mortal Socrates))

            (:= Sam Entity)
            (:= (Frog Entity) Prop)
            (:= (Green Entity) Prop)
            (:= (Croaks Entity) Prop)
            (:= (GreenAndCroaksIsFrog (Green $t) (Croaks $t)) (Frog $t))
            (:= SamIsGreen (Green Sam))
            (:= SamCroaks (Croaks Sam))
        ''')
        self.assertEqual(metta.interpret("(:? (HumansAreMortal SocratesIsHuman))"),
                                        [E(S('Mortal'), S('Socrates'))])
        self.assertEqual(metta.interpret("(:check (HumansAreMortal SocratesIsHuman) (Mortal Socrates))"),
                                        [S('T')])
        self.assertEqual(metta.interpret("(:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))"),
                                        [S('Prop')])
        self.assertEqual(metta.interpret("(:check (= (Mortal Plato) (Mortal Socrates)) Prop)"),
                                        [S('T')])
        self.assertEqual(metta.interpret("(:check (= (Human Socrates) (Mortal Socrates)) Prop)"),
                                        [S('T')]) # they are both of Prop type and can be equated
        self.assertEqual(metta.interpret("(:? (GreenAndCroaksIsFrog SamIsGreen SamCroaks))"),
                                        [E(S('Frog'), S('Sam'))])
        # some negative examples
        self.assertEqual(metta.interpret("(:check (= SocratesIsHuman SocratesIsMortal) Prop)"), [])
        self.assertEqual(metta.interpret("(:? (SocratesIsHuman (Human Socrates)))"), [])
        self.assertEqual(metta.interpret("(:? (Human Time))"), [])
        # TODO: doesn't work, because the expression is matched agains type definition of HumansAreMortal
        #       with grounding $t <- Time before trying to reduce the time of (Human Time)
        #self.assertEqual(metta.interpret("(:? (HumansAreMortal (Human Time)))"),
        #                                [])
        # Another syntax
        metta = MeTTa()
        metta.add_parse('''
            (= (:? $c)
               (match &self (:: $c $t) $t))
            (= (:? ($c $a))
               (match &self (:: $c (-> (:? $a) $t)) $t))

            (:: Entity Prop)
            (:: Human (-> Entity Prop))
            (:: Socrates Entity)
            (:: Plato Entity)
            (:: Mortal (-> Entity Prop))
            (:: HumansAreMortal (-> (Human $t) (Mortal $t)))
            (:: SocratesIsHuman (Human Socrates))
        ''')
        self.assertEqual(metta.interpret("(:? (HumansAreMortal SocratesIsHuman))"),
                                        [E(S('Mortal'), S('Socrates'))])

    def _test_visit_kim(self):
        atomese = Atomese()
        kb = GroundingSpace("kb")
        atomese.add_atom("kb", SpaceAtom(kb))

        # it's questionable if the representation of (health-check Kim)
        # which can be interpreted as a functional call is correct,
        # but these tests pass for now
        program = '''
            (= (perform (visit $x)) (perform (lunch-order $x)))
            (= (perform (visit $x)) (perform (health-check $x)))

            (impl (is-achieved (visit $x))
                  (And (is-achieved (lunch-order $x)) (is-achieved (health-check $x))))

            (= (achieve $goal)
               (match kb (impl (is-achieved $goal)
                               (And (is-achieved $subgoal1) (is-achieved $subgoal2)))
                      (do $subgoal1 $subgoal2)))

            (= (achieve (health-check Kim)) True)
            (= (achieve (lunch-order Kim)) False)
            '''
        # (do $subgoal1 $subgoal2) --> (do (achieve $subgoal1) (achieve $subgoal2)))) --
        # -- will try to execute 'achieve' and produce (do True True) as output...

        atomese.parse(program, kb)

        # simple functional way to produce subgoals in target
        target = atomese.parse('(perform (visit Kim))')
        result = interpret_until_result(target, kb)
        # returned now as output because there is no further interpretation of this expression
        # it could be expanded further into subgoals or external actions
        self.assertEqual(repr(result), '(perform (health-check Kim))')
        # the next subgoal is produced in the consequent interpretation of the initial
        # nondeterministic expression
        result = interpret_until_result(target, kb)
        self.assertEqual(repr(result), '(perform (lunch-order Kim))')

        # Higher-order matching:
        # (visit Kim) -> $goal in (achieve $goal)
        # Kim -> $x in (impl (is-achieved (visit $x)) ...
        # $subgoal[1,2] <- (is-achieved ([lunch-order, health-check] Kim))
        # checking if such two-side unification works:
        target = atomese.parse('(achieve (visit Kim))')
        result = interpret_until_result(target, kb)
        self.assertEqual(repr(result), '(do (lunch-order Kim) (health-check Kim))')

        # Extending the program
        atomese.parse('(= (do $goal1 $goal2) (achieve $goal1))', kb)
        atomese.parse('(= (do $goal1 $goal2) (achieve $goal2))', kb)
        target = atomese.parse('(achieve (visit Kim))')
        # (achieve (visit Kim)) --> (do (lunch-order Kim) (health-check Kim))
        # --> (achieve (health-check Kim)) ... --> True
        result = interpret_until_result(target, kb)
        self.assertEqual(repr(result), 'True')

class SomeObject():

    def __init__(self):
        self.called = False

    def foo(self):
        self.called = True

# New object example

def new_atom_op(klass, unwrap, *params):
    if unwrap:
        unwrapped = [param.get_object().value for param in params]
        return [ValueAtom(klass(*unwrapped))]
    else:
        return [ValueAtom(klass(*params))]

def newNewAtom(token, klass, unwrap=True):
    return OperationAtom(
        token,
        lambda *params: new_atom_op(klass, unwrap, *params),
        unwrap=False)

class Global:

    def __init__(self, x):
        self.set(x)
    
    def set(self, x):
        self.x = x

    def get(self):
        return self.x

class Setter:

    def __init__(self, var, val):
        self.var = var
        self.val = val

    def act(self):
        self.var.set(self.val)

    def let(self):
        self.var = self.val

    def latom(self):
        # if var/val are not unwrapped
        self.var.get_object().value = self.val.get_object().value


init_logger()
if __name__ == "__main__":
    unittest.main()
