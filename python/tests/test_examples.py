import unittest

from hyperon import *
from common import MeTTa, SpaceAtom
from test_common import *

class ExamplesTest(unittest.TestCase):

    def test_grounded_functions(self):
        metta = MeTTa()
        obj = SomeObject()
        # using & as a prefix is not obligatory, but is naming convention
        metta.add_atom("&obj", ValueAtom(obj))

        target = metta.parse_single('(call:foo &obj)')
        # interpreting this target in another space still works,
        # because substitution '&obj' -> obj is done by metta
        metta2 = MeTTa()
        result = interpret(metta2.space, target)
        self.assertTrue(obj.called)
        self.assertEqual(result, [])
        # But it will not work if &obj is parsed in another space
        result = metta2.interpret('(call:foo &obj)')
        self.assertEqual(repr(result[0]), '(call:foo &obj)')

    # TODO: when (change-state ...) inside (, (change-state ...) ...) returns
    # an empty result (for instance when variable is absent) then Interpreter
    # stops processing (, ...) because no alternatives are present to continue
    # interpretation.
    @unittest.skip("TODO")
    def test_self_modify(self):
        metta = MeTTa()
        metta.add_parse(
        '''
            (= (remove-state $var)
               (match &self (state $var $y)
                  (call:remove_atom &self (state $var $y))))
            (= (change-state $var $value)
               (, (remove-state $var)
                  (call:add_atom &self (state $var $value))))
            (= (get-state $var)
               (match &self (state $var $value) $value))
        ''')
        metta.interpret('(change-state (name id-001) Fritz)')
        self.assertEqual(metta.interpret('(get-state (name id-001))'),
                         [S('Fritz')])
        metta.interpret('(change-state (name id-001) Sam)')
        self.assertEqual(metta.interpret('(get-state (name id-001))'),
                         [S('Sam')])

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
        metta.add_token("ploca", lambda _: ploca)
        # It will be not affected by assigning unwrapped values:
        # we are still copying values while unwrapping
        metta.interpret('(call:let (Setter ploca 5))')
        self.assertEqual(metta.interpret('ploca')[0].get_object().value, 10)
        self.assertEqual(ploca.get_object().value, 10)
        # However, it will be affected by assigning atom values
        metta.interpret('(call:latom (SetAtom ploca 5))')
        self.assertEqual(metta.interpret('ploca')[0].get_object().value, 5)
        self.assertEqual(ploca.get_object().value, 5)

    def test_frog_reasoning(self):
        metta = MeTTa()

        metta.add_parse('''
            (: if (-> Bool Atom Atom $t))
            (= (if True $then $else) $then)
            (= (if False $then $else) $else)
            (= (Fritz croaks) True)
            (= (Tweety chirps) True)
            (= (Tweety yellow) True)
            (= (Tweety eats_flies) True)
            (= (Fritz eats_flies) True)
        ''')

        fritz_frog = metta.interpret('(if (and ($x croaks) ($x eats_flies)) (= ($x frog) True) nop)')
        self.assertEqual(metta.parse_all('(= (Fritz frog) True)'), fritz_frog)
        metta.space.add_atom(fritz_frog[0])

        self.assertEqual(metta.parse_all('(= (Fritz green) True)'),
                metta.interpret('(if ($x frog) (= ($x green) True) nop)'))

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
        self.assertEqual(output, metta.parse_all('(eq 5 4)'))

        output = metta.interpret('(eq (plus Z $n) $n)')
        self.assertEqual(output, [ValueAtom(True)])

        output = metta.interpret('(eq (plus (S Z) $n) $n)')
        assert_atoms_are_equivalent(self, output, metta.parse_all('(eq (S $y) $y)'))

    def test_multi_space(self):
        # NOTE: it is not recommended to split code into multiple spaces, because
        # query chaining by the interpreter can behave in a tricky way
        # (putting data without equalities in a separate space and querying it
        # explicitly from another space should be safe, though)
        # NOTE: these tests are not indended to remain valid, but are needed to
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
        # The following doesn't work: `(:? (HumansAreMortal (Human Time)))` is matched against `(:? $c)`
        # Then, `(:= $c $t)` is matched against `(:= (HumansAreMortal (Human $t)) (Mortal $t))`
        # immediately resulting in `(Mortal Time)`. Thus, it doesn't matter that matching against
        # `(= (:? ($c $a))` doesn't work (since `(:? $a)` is incorrect).
        # self.assertEqual(metta.interpret("(:? (HumansAreMortal (Human Time)))"),
        #                                  [])
        # It should be noted that `(HumansAreMortal (Human Socrates))` is also an incorrectly typed
        # expression, since HumansAreMortal expects an element of (Human Socrates) - not the type itself

        # Another syntax
        metta = MeTTa()
        metta.add_parse('''
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
        self.assertEqual(metta.interpret("(:? (Human Plato))"), [S('Type')])
        self.assertEqual(metta.interpret("(:? (Human Time))"), [])
        self.assertEqual(metta.interpret("(:? (HumansAreMortal SocratesIsHuman))"),
                                        [E(S('Mortal'), S('Socrates'))])
        self.assertEqual(metta.interpret("(:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))"),
                                        [S('Type')])
        self.assertEqual(metta.interpret("(:? (= Human Entity))"), [])
        self.assertEqual(metta.interpret("(:? (= (Human Socrates) Plato))"), [])
        self.assertEqual(metta.interpret("(:? (= SocratesIsHuman SocratesIsMortal))"), [])
        # `(Human Socrates)` and `(Human Plato)` are different types, but they are elements
        # of the same Type, so they can be equated
        self.assertEqual(metta.interpret("(:? (= (Human Socrates) (Human Plato)))"),
                                        [S('Type')])
        self.assertEqual(metta.interpret("(:? (= Human Mortal))"),
                                        [S('Type')])
        self.assertEqual(metta.interpret("(:? (= HumansAreMortal Mortal))"), [])
        # Interestingly, the following example works correctly in this syntax, because
        # application `(Human Socrates)` is not mixed up with dependent type definition
        self.assertEqual(metta.interpret("(:? (HumansAreMortal (Human Socrates)))"), [])

    def test_visit_kim(self):
        # legacy test
        # can be moved to b4_nondeterm.metta or removed
        metta = MeTTa()
        metta.add_parse('''
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
        self.assertEqual(metta.interpret('(perform (visit Kim))'),
            metta.parse_all('(perform (lunch-order Kim)) (perform (health-check Kim))'))
        self.assertEqual(metta.interpret('(achieve (visit Kim))'),
            metta.parse_all('(do (lunch-order Kim) (health-check Kim))'))
        metta.add_parse('''
            (= (do $goal1 $goal2) (achieve $goal1))
            (= (do $goal1 $goal2) (achieve $goal2))
        ''')
        self.assertEqual(metta.interpret('(achieve (visit Kim))'),
            metta.parse_all('False True'))

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


if __name__ == "__main__":
    unittest.main()
