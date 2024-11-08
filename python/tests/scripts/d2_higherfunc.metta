;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Higher-order functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Type definition for `curry` function
(: curry (-> (-> $a $b $c) (-> $a (-> $b $c))))
; A trick to define `curry` in MeTTa without `lambda`
(= (((curry $f) $x) $y) ($f $x $y))

; Type checks
!(assertEqual
  (get-type (curry +))
  (-> Number (-> Number Number)))
!(assertEqual
  (get-type ((curry +) 2))
  (-> Number Number))

; Partial application is not reduced, basically
!(assertEqualToResult
   ((curry +) 2)
  (((curry +) 2)))
; Full application will be reduced
!(assertEqual
  (((curry +) 2) 3)
  5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Curry with first application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definitions
(: curry-a (-> (-> $a $b $c) $a (-> $b $c)))
(= ((curry-a $f $a) $b) ($f $a $b))

; Tests
!(assertEqual
  (get-type (curry-a + 2))
  (-> Number Number))
!(assertEqual
  (get-type ((curry-a + 2) 3))
  Number)
; Badly typed
!(assertEqualToResult
  (get-type ((curry-a + 2) "S"))
  ())
; Full application works
!(assertEqual
  ((curry-a + 2) 3)
  5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; More tests with partial application wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definitions
(: Socrates Entity)
(: Human Entity)
(: is (-> Entity Entity Bool))
; Facts
(= (is Socrates Human) True)
(= (is-socrates) (curry-a is Socrates))

; Tests
!(assertEqualToResult
  (is-socrates)
  ((curry-a is Socrates)))
; not reduced
!(assertEqual
  ((curry-a is Socrates) Human)
  (is Socrates Human))
; reduced
!(assertEqual
  ((is-socrates) Human)
  True)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The same trick works for defining lambda
; (basically, `lambda` is curried `let`)
; FIXME: how to make parameterized type here?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definitions
(: lambda (-> Atom $t (-> $a $t)))
(= ((lambda $var $body) $arg)
   (let $var $arg $body))

; Tests
!(assertEqual
  ((lambda $x (+ $x 1)) 2)
  3)
!(assertEqual
  ((lambda ($x $y) (+ $x $y)) (2 7))
  9)

; Another example
(= (part-appl $f $x)
   (lambda $y ($f $x $y)))
(= (inc) (part-appl + 1))
!(assertEqual
  ((inc) 5)
  6)
; REM:
;   (get-type (lambda ($x $y) (+ $x $y)))
; returns (-> %Undefined% Number), because the type of `($x $y)` is not defined.
; It is also not clear how to check that the type of `$var` is suitable for `$body`
; (it should somehow be done on`let` side)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's introduce some functors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: Maybe (-> $t Type))
(: Nothing (-> (Maybe $t)))
(: Something (-> $t (Maybe $t)))

(: Either (-> $t Type))
(: Left (-> $t (Either $t)))
(: Right (-> $t (Either $t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We can implement a generic `fmap`, but it requires
; concrete patterns in the type constructors above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Instead, we implement `fmap` as:
(: fmap (-> (-> $a $b) ($F $a) ($F $b)))
; Mapping over empty functor, returns empty functor
(= (fmap $f ($C0))
  ($C0))
; Inductive case for singleton functor: apply $f to the
; element and rewrap in $C.
(= (fmap $f ($C $x))
  ($C ($f $x)))
; Inductive case for non-empty functor: apply to the
; head and recurse on the tail.
(= (fmap $f ($C $x $xs))
  ($C ($f $x) (fmap $f $xs)))

; NOTE: We had to define `Nothing` as a functional constructor.
;       Otherwise, we'd have to define `(= (fmap $f $C0) $C0)`,
;       and `$C0` would match both `Nothing` and `(Something 2)`
; TODO? This could be avoided if we could indicate that $C0
;       is a `Symbol` (not `Expression` or `Atom` in general)

; Tests
!(assertEqual
  (fmap (curry-a + 2) (Something 5))
  (Something 7))
!(assertEqual
  (fmap (curry-a + 2) (Nothing))
  (Nothing))

; Type inference works
!(assertEqual
  (get-type (fmap (curry-a + 1) (Left 5)))
  (Either Number))

; It works for untyped constructors as well, if they
; follow the patterns in `fmap` equalities
!(assertEqual
  (fmap (curry-a + 2) (UntypedC 5))
  (UntypedC 7))
!(assertEqual
  (fmap (curry-a + 2) (UntypedC 5 (UntypedC 8 (Null))))
  (UntypedC 7 (UntypedC 10 (Null))))

; Type mismatches are detected:
!(assertEqualToResult
  (get-type (fmap (curry-a + 2) (Left "5")))
  ())
; TODO: Two examples below are type-checked successfully because, (UntypedC "5")
; can return result which has an appropriate type. But type returned contains
; variable, for instance ($F#3287 Number). There is no function to compare such
; atom easily with pattern yet. Uncomment after equivalent-atom operation is
; introduced.
;!(assertEqualToResult
;  (get-type (fmap (curry-a + 2) (UntypedC "5")))
;  ())
;!(assertEqualToResult
;  (get-type (fmap (curry-a + 2) (UntypedC (Null) 5)))
;  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; It is possible to implement `fmap` only as an interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Type definition is the same as `fmap` above
(: fmap-i (-> (-> $a $b) ($F $a) ($F $b)))
; Custom implementations for different functors will be
; possible (and needed):
(= (fmap-i $f (Left $x)) (Left ($f $x)))
(= (fmap-i $f (Right $x)) (Right ($f $x)))
!(assertEqual
  (fmap-i (curry-a - 7) (Right 3))
  (Right 4))

; More examples
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))
(= (fmap-i $f Nil) Nil)
(= (fmap-i $f (Cons $x $xs))
   (Cons ($f $x) (fmap-i $f $xs)))
!(assertEqual
  (fmap-i (curry-a * 2) (Cons 3 (Cons 4 Nil)))
  (Cons 6 (Cons 8 Nil)))

; Thus, there is no problem in having different implementations
; of the same function for different types. But it will not
; work "for free" (it requires explicit implementation for each type)
!(assertEqualToResult
   (fmap-i (curry-a + 2) (Untyped 5))
  ((fmap-i (curry-a + 2) (Untyped 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; As per issue
;
; https://github.com/trueagi-io/hyperon-experimental/issues/104
;
; Test if adding type declaration to List data structure does
; not interfere with executing functions operating on List.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Insert an element in a presumably sorted list
(= (insert $x Nil) (Cons $x Nil))
(= (insert $x (Cons $head $tail))
  (case (< $x $head)
      ((True  (Cons $x (Cons $head $tail)))
       (False (Cons $head (insert $x $tail))))))

; Sort a list
(= (sort Nil) Nil)
(= (sort (Cons $head $tail)) (insert $head (sort $tail)))
!(assertEqual
  (insert 3 (insert 2 (insert 1 Nil)))
  (Cons 1 (Cons 2 (Cons 3 Nil))))
!(assertEqual
  (sort (Cons 3 (Cons 1 (Cons 2 Nil))))
  (Cons 1 (Cons 2 (Cons 3 Nil))))
