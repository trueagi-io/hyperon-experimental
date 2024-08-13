!(import! &self compile)

; creating a space for repetitive use
!(bind! &jspace (new-jetta-space))

; checking a program with a passing lambdas
!(jetta &jspace
   "(: foo (-> Int Int (-> Int Int Int) Int))
    (= (foo $x $y $f) ($f $x $y))
    (: bar (-> Int Int))
    (= (bar $z)
        (foo 10 20 (\\ ($x $y) (+ $x $y $z (foo 10 20 (\\ ($k $v) (+ $k $v)))
        )))
    )
    (: goo (-> Int Int Int))
    (= (goo $x $y) (* $x $y))"
)
; passing a function
!(assertEqual
  (jetta &jspace "(foo 2 3 goo)")
  6)
; checking that passing lamdas directly works
!(assertEqual
  (jetta &jspace "(foo 2 3 (\\ ($x $y) (* $x $y)))")
  6)
; checking that calling functions with internal use of
; lambdas work
!(assertEqual
  (jetta &jspace "(bar 1)")
  61)

; check that passing expressions with lambdas also works
(: foo-f (-> Int Int (-> Int Int Int) Int))
(= (foo-f $x $y $f) ($f $x $y))
(: bar-lamb (-> Int Int))
(= (bar-lamb $z)
   (foo-f 10 30
      (\ ($x $y)
         (+ $x $y $z
            (foo-f 10 30 (\ ($k $v) (+ $k $v)))
         )
      )
   )
)
!(compile &jspace foo-f)
!(compile &jspace bar-lamb)
!(assertEqual
  (bar-lamb-gnd 1)
  81)
!(assertEqual
  (foo-f-gnd 1 2 (\ ($x $y) (+ $x $y)))
  3)


; TODO: nondeterminism
(jetta &jspace
    "(: doo (-> Int Int Int))
     (= (doo $x $y) (+ $x $y))
     (= (doo $x $y) (- $x $y))")
(jetta &jspace "(doo 1 2)")
