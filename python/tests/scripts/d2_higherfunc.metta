; Higher-order functions

(: curry (-> (-> $a $b $c) (-> $a (-> $b $c))))
; A trick to define `curry` in MeTTa without `lambda`
(= (((curry $f) $x) $y) ($f $x $y))
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

; Curry with first application
(: curry-a (-> (-> $a $b $c) $a (-> $b $c)))
(= ((curry-a $f $a) $b) ($f $a $b))

!(assertEqual
  (get-type (curry-a + 2))
  (-> Number Number))
!(assertEqual
  (get-type ((curry-a + 2) 3))
  Number)
; Badly typed:
!(assertEqualToResult
  (get-type ((curry-a + 2) "S"))
  ())
; Full application works
!(assertEqual
  ((curry-a + 2) 3)
  5)

; More tests with partial application wrapping
(: Socrates Entity)
(: Human Entity)
(: is (-> Entity Entity Bool))
(= (is Socrates Human) True)
(= (is-socrates) (curry-a is Socrates))
!(assertEqualToResult
   (is-socrates)
  ((curry is Socrates)))
!(assertEqual
  ((curry is Socrates) Human)
  (is Socrates Human))
!(assertEqual
  ((is-socrates) Human)
  True)

; Let's introduce some functors
(: Maybe (-> $t Type))
(: Nothing (-> (Maybe $t)))
(: Something (-> $t (Maybe $t)))

(: Either (-> $t Type))
(: Left (-> $t (Either $t)))
(: Right (-> $t (Either $t)))

; We can implement a generic `fmap`, but it requires
; concrete patterns in type constructors
(: fmap (-> (-> $a $b) ($F $a) ($F $b)))
(= (fmap $f ($C0)) ($C0))
(= (fmap $f ($C $x)) ($C ($f $x)))
(= (fmap $f ($C $x $xs))
   ($C ($f $x) (fmap $f $xs)))
; NOTE: We had to define `Nothing` as a functional constructor.
;       Otherwise, we'd have to define `(= (fmap $f $C0) $C0)`,
;       and `$C0` would match both `Nothing` and `(Something 2)`
; TODO? This could be avoided if we could indicate that $C0
;       is a `Symbol` (not `Expression` or `Atom` in general)

; But it works
!(assertEqual
  (fmap (curry-a + 2) (Something 5))
  (Something 7))
!(assertEqual
  (fmap (curry-a + 2) (Nothing))
  (Nothing))
; And its type is inferred
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
; And here type mismatch will be detected:
!(assertEqualToResult
  (get-type (fmap (curry-a + 2) (Left "5")))
  ())
!(assertEqualToResult
  (get-type (fmap (curry-a + 2) (UntypedC "5")))
  ())
!(assertEqualToResult
  (get-type (fmap (curry-a + 2) (UntypedC (Null) 5)))
  ())

; It is possible to implement `fmap` only as an interface
(: fmap-i (-> (-> $a $b) ($F $a) ($F $b)))

; Custom implementations for different functors will be
; possible (and needed)
(= (fmap-i $f (Left $x)) (Left ($f $x)))
(= (fmap-i $f (Right $x)) (Right ($f $x)))
!(assertEqual
  (fmap-i (curry-a - 7) (Right 3))
  (Right 4))

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
; work "for free"
!(assertEqualToResult
   (fmap-i (curry-a + 2) (Untyped 5))
  ((fmap-i (curry-a + 2) (Untyped 5))))
