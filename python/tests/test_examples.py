import unittest

from hyperon import *
from test_common import *

class ExamplesTest(HyperonTestCase):

    def test_grounded_functions(self):
        metta = MeTTa(env_builder=Environment.test_env())
        obj = SomeObject()
        # using & as a prefix is not obligatory, but is naming convention
        metta.register_atom("&obj", ValueAtom(obj))

        target = metta.parse_single('((py-dot &obj foo))')
        # interpreting this target in another space still works,
        # because substitution '&obj' -> obj is done by metta
        metta2 = MeTTa(env_builder=Environment.test_env())
        result = interpret(metta2.space(), target)
        self.assertTrue(obj.called)
        self.assertEqual(result, [Atoms.UNIT])
        # But it will not work if &obj is parsed in another space
        result = metta2.run('!((py-dot &obj foo))')[0]
        self.assertEqual(repr(result), '[((py-dot &obj foo))]')

    # TODO: it's a custom implementation of State. We may not need it anymore
    def test_self_modify(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run(
        '''
            (= (remove-st $var)
               (match &self (state $var $y)
                  (remove-atom &self (state $var $y))))
            (= (change-st $var $value)
               (superpose ((remove-st $var)
                  (add-atom &self (state $var $value)))))
            (= (get-st $var)
               (match &self (state $var $value) $value))
        ''')
        metta.run('!(change-st (name id-001) Fritz)')
        self.assertEqualMettaRunnerResults(metta.run('!(get-st (name id-001))'),
                         [[S('Fritz')]])
        metta.run('!(change-st (name id-001) Sam)')
        self.assertEqualMettaRunnerResults(metta.run('!(get-st (name id-001))'),
                         [[S('Sam')]])

    def test_new_object(self):
        metta = MeTTa(env_builder=Environment.test_env())
        pglob = Global(10)
        ploc = 10
        metta.register_token("pglob", lambda _: ValueAtom(pglob))
        metta.register_token("ploc", lambda _: ValueAtom(ploc))
        metta.register_token("Setter", lambda token: newNewAtom(token, Setter))
        metta.register_token("SetAtom", lambda token: newNewAtom(token, Setter, False))
        # Just checking that interpretation of "pglob" gives us
        # a grounded atom that stores 10
        self.assertEqual(metta.run('! pglob')[0][0].get_object().value.get(), 10)
        # Checking that:
        # - we create an atom on fly
        # - we change the value stored by the Python object
        # - interpretation of "pglob" will also give us this new value
        metta.run('!((py-dot (Setter pglob 5) act))')
        self.assertEqual(pglob.get(), 5)
        self.assertEqual(metta.run('! pglob')[0][0].get_object().value.get(), 5)
        # Now check that "ploc" will not change, since
        # it is passed by value - not reference
        metta.run('!((py-dot (Setter ploc 5) let))')
        self.assertEqual(ploc, 10)
        self.assertEqual(metta.run('! ploc')[0][0].get_object().value, 10)
        # Now we try to change the grounded atom value directly
        # (equivalent to metta.run but keeping target)
        target = metta.parse_single('((py-dot (SetAtom ploc 5) latom))')
        interpret(metta.space(), target)
        t = target.get_children()[0] # unwrap outermost brackets
        # "ploc" value in the "target" is changed
        self.assertEqual(t.get_children()[1].get_children()[1].get_object().value, 5)
        # But it is still not changed in another target, because
        # "ploc" creates ValueAtom(ploc) on each occurrence
        self.assertEqual(metta.run('! ploc')[0][0].get_object().value, 10)
        # Another way is to return the same atom each time
        ploca = ValueAtom(ploc)
        metta.register_token("ploca", lambda _: ploca)
        # It will be not affected by assigning unwrapped values:
        # we are still copying values while unwrapping
        metta.run('!((py-dot (Setter ploca 5) let))')
        self.assertEqual(metta.run('! ploca')[0][0].get_object().value, 10)
        self.assertEqual(ploca.get_object().value, 10)
        # However, it will be affected by assigning atom values
        metta.run('!((py-dot (SetAtom ploca 5) latom))')
        self.assertEqual(metta.run('! ploca')[0][0].get_object().value, 5)
        self.assertEqual(ploca.get_object().value, 5)

    def test_frog_reasoning(self):
        metta = MeTTa(env_builder=Environment.test_env())

        metta.run('''
            (= (croaks Fritz) True)
            (= (chirps Tweety) True)
            (= (yellow Tweety) True)
            (= (eats_flies Tweety) True)
            (= (eats_flies Fritz) True)
        ''')

        fritz_frog = metta.run('!(if (and (croaks $x) (eats_flies $x)) (= (frog $x) True) nop)')[0]
        self.assertEqual(metta.parse_all('(= (frog Fritz) True)'), fritz_frog)
        metta.space().add_atom(fritz_frog[0])

        self.assertEqualMettaRunnerResults([metta.parse_all('(= (green Fritz) True)')],
                metta.run('!(if (frog $x) (= (green $x) True) nop)'))

    def test_infer_function_application_type(self):
        metta = MeTTa(env_builder=Environment.test_env())

        metta.run('''
           (= (: (apply\' $f $x) $r) (and (: $f (=> $a $r)) (: $x $a)))

           (= (: reverse (=> String String)) True)
           (= (: "Hello" String) True)
        ''')

        output = metta.run('!(if (: (apply\' reverse "Hello") $t) $t Wrong)')
        self.assertEqualMettaRunnerResults(output, [[S('String')]])

    def test_plus_reduces_Z(self):
        metta = MeTTa(env_builder=Environment.test_env())

        metta.run('''
           (= (eq $x $x) True)
           (= (plus Z $y) $y)
           (= (plus (S $k) $y) (S (plus $k $y)))
        ''')

        self.assertEqualMettaRunnerResults(metta.run('''
            !(eq (+ 2 2) 4)
            !(eq (+ 2 3) 4)
            !(eq (plus Z $n) $n)
            '''),
            [[ValueAtom(True)],
             metta.parse_all('(eq 5 4)'),
             [ValueAtom(True)]
            ]
        )
        output = metta.run('!(eq (plus (S Z) $n) $n)')
        self.assertAtomsAreEquivalent(output[0], metta.parse_all('(eq (S $y) $y)'))

    def test_multi_space(self):
        # NOTE: it is not recommended to split code into multiple spaces, because
        # query chaining by the interpreter can behave in a tricky way
        # (putting data without equalities in a separate space and querying it
        # explicitly from another space should be safe, though)
        # NOTE: these tests are not indended to remain valid, but are needed to
        # detect, if something is changes in the interpreter
        metta1 = MeTTa(env_builder=Environment.test_env())
        metta1.run('''
            (eq A B)
            (= (f-in-s2) failure)
            (= (how-it-works?) (f-in-s2))
            (= (inverse $x) (match &self (eq $y $x) $y))
        ''')
        metta2 = MeTTa(env_builder=Environment.test_env())
        metta2.register_atom("&space1", metta1.run("! &self")[0][0])
        metta2.run('''
            (eq C B)
            (= (f-in-s2) success)
            (= (find-in $s $x) (match $s (eq $y $x) $y))
            (= (borrow $s $e) (match $s (= $e $r) $r))
        ''')
        self.assertEqualMettaRunnerResults(metta1.run('!(inverse B)'), [[S('A')]])
        self.assertEqualMettaRunnerResults(metta2.run('!(find-in &space1 B)'), [[S('A')]])
        self.assertEqualMettaRunnerResults(metta2.run('!(find-in &self B)'), [[S('C')]])
        # `inverse` is successfully found in `&space1`
        # it resolves `&self` to metta1.space and matches against `(= A B)`
        self.assertEqualMettaRunnerResults(metta2.run('!(borrow &space1 (inverse B))'), [[S('A')]])
        # `borrow` executes `how-it-works?` in context of `&space1` via `match`
        # but then the interpreter evaluates `(how-it-works?)` via equality query
        # in the original metta2.space
        self.assertEqualMettaRunnerResults(metta2.run('!(borrow &space1 (how-it-works?))'), [[S('success')]])
        self.assertEqualMettaRunnerResults(metta1.run('!(how-it-works?)'), [[S('failure')]])

    def test_custom_deptypes(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run('''
            (= (:? $c)
               (match &self (:= $c $t) $t))
            (= (:? ($c $a))
               (let $at (:? $a)
                    (match &self (:= ($c $at) $t) $t)))
            (= (:? ($c $a $b))
               (let* (($at (:? $a))
                      ($bt (:? $b)))
                     (match &self (:= ($c $at $bt) $t) $t)))

            (= (:check $c $t)
               (match &self (:= $c $t) T))
            (= (:check ($c $a) $t)
               (let $at (:? $a)
                    (match &self (:= ($c $at) $t) T)))
            (= (:check ($c $a $b) $t)
               (let* (($at (:? $a))
                      ($bt (:? $b)))
                     (match &self (:= ($c $at $bt) $t) T)))

            (:= (= $t $t) Prop)

            (:= Entity Prop)
            (:= (Human Entity) Prop)
            (:= Socrates Entity)
            (:= Plato Entity)
            (:= Time NotEntity)
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
        self.assertEqualMettaRunnerResults(metta.run('''
            !(:? (HumansAreMortal SocratesIsHuman))
            !(:check (HumansAreMortal SocratesIsHuman) (Mortal Socrates))
            !(:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))
            !(:check (= (Mortal Plato) (Mortal Socrates)) Prop)
            !(:check (= (Human Socrates) (Mortal Socrates)) Prop)
            !(:? (GreenAndCroaksIsFrog SamIsGreen SamCroaks))
            '''),
            [[E(S('Mortal'), S('Socrates'))],
             [S('T')],
             [S('Prop')],
             [S('T')],
             [S('T')], # they are both of Prop type and can be equated
             [E(S('Frog'), S('Sam'))]
            ]
        )
        # some negative examples
        self.assertEqualMettaRunnerResults(metta.run('''
            !(:check (= SocratesIsHuman SocratesIsMortal) Prop)
            !(:? (SocratesIsHuman (Human Socrates)))
            !(:? (Human Time))
            '''),
            [[], [], []]
        )
        # The following doesn't work: `(:? (HumansAreMortal (Human Time)))` is matched against `(:? $c)`
        # Then, `(:= $c $t)` is matched against `(:= (HumansAreMortal (Human $t)) (Mortal $t))`
        # immediately resulting in `(Mortal Time)`. Thus, it doesn't matter that matching against
        # `(= (:? ($c $a))` doesn't work (since `(:? $a)` is incorrect).
        # self.assertEqualMettaRunnerResults(metta.run("!(:? (HumansAreMortal (Human Time)))"),
        #                                  [[]])
        # It should be noted that `(HumansAreMortal (Human Socrates))` is also an incorrectly typed
        # expression, since HumansAreMortal expects an element of (Human Socrates) - not the type itself

        # Another syntax
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run('''
            (= (:? $c)
               (match &self (:: $c $t) $t))
            (= (:? ($c $a))
               (let $at (:? $a)
                    (match &self (:: $c (-> $at $t)) $t)))
            (= (:? ($c $a $b))
               (let* (($at (:? $a))
                      ($bt (:? $b)))
                     (match &self (:: $c (-> $at $bt $t)) $t)))

            (:: = (-> $t $t Type))

            (:: Entity Type)
            (:: Human (-> Entity Type))
            (:: Socrates Entity)
            (:: Plato Entity)
            (:: Mortal (-> Entity Type))
            (:: HumansAreMortal (-> (Human $t) (Mortal $t)))
            (:: SocratesIsHuman (Human Socrates))
            (:: SocratesIsMortal (Mortal Socrates))
        ''')
        # :? just infers the type of the expression - not its inhabitance
        # Note: `(Human Socrates)` and `(Human Plato)` are different types, but they are
        # elements of the same Type, so they can be equated
        self.assertEqualMettaRunnerResults(metta.run('''
            !(:? (Human Plato))
            !(:? (Human Time))
            !(:? (HumansAreMortal SocratesIsHuman))
            !(:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))
            !(:? (= Human Entity))
            !(:? (= (Human Socrates) Plato))
            !(:? (= SocratesIsHuman SocratesIsMortal))
            !(:? (= (Human Socrates) (Human Plato)))
            !(:? (= Human Mortal))
            !(:? (= HumansAreMortal Mortal))
            '''),
            [[S('Type')],
             [],
             [E(S('Mortal'), S('Socrates'))],
             [S('Type')],
             [],
             [],
             [],
             [S('Type')],
             [S('Type')],
             []
            ]
        )
        # Interestingly, the following example works correctly in this syntax, because
        # application `(Human Socrates)` is not mixed up with dependent type definition
        self.assertEqualMettaRunnerResults(metta.run("!(:? (HumansAreMortal (Human Socrates)))"), [[]])

    def test_visit_kim(self):
        # legacy test
        # can be moved to b4_nondeterm.metta or removed
        metta = MeTTa(env_builder=Environment.test_env())
        metta.run('''
            (= (perform (visit $x)) (perform (lunch-order $x)))
            (= (perform (visit $x)) (perform (health-check $x)))

            (impl (is-achieved (visit $x))
                (And (is-achieved (lunch-order $x)) (is-achieved (health-check $x))))

            (= (achieve $goal)
                (match &self (impl (is-achieved $goal)
                                (And (is-achieved $subgoal1) (is-achieved $subgoal2)))
                    (do $subgoal1 $subgoal2)))

            (= (achieve (health-check Kim)) True)
            (= (achieve (lunch-order Kim)) False)
        ''')
        self.assertEqualMettaRunnerResults(metta.run('''
            !(perform (visit Kim))
            !(achieve (visit Kim))
            '''),
            [metta.parse_all('(perform (lunch-order Kim)) (perform (health-check Kim))'),
             metta.parse_all('(do (lunch-order Kim) (health-check Kim))')]
        )
        metta.run('''
            (= (do $goal1 $goal2) (achieve $goal1))
            (= (do $goal1 $goal2) (achieve $goal2))
        ''')
        self.assertEqualMettaRunnerResults(metta.run('!(achieve (visit Kim))'),
            [metta.parse_all('False True')])

    def test_char_vs_string(self):
        metta = MeTTa(env_builder=Environment.test_env())
        self.assertEqual(repr(metta.run("!('A')")), "[[('A')]]")
        self.assertEqual(repr(metta.run('!("A")')), '[[("A")]]')
        self.assertEqualMettaRunnerResults(metta.run("!(get-type 'A')"), [[S('Char')]])
        self.assertEqualMettaRunnerResults(metta.run('!(get-type "A")'), [[S('String')]])


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
        self.var.get_object().content = self.val.get_object().content


if __name__ == "__main__":
    unittest.main()
