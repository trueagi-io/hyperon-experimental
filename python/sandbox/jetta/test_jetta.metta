!(import! &self compile)

!(jetta (new-jetta-space) "(+ 1 2)") ; 3

!(bind! &jspace (new-jetta-space))

; passing code text directly
! (jetta &jspace "(: foo (-> Int Int Int))
    (= (foo $x $y) (+ $x $y 1))")
! (jetta &jspace "(foo 11 2)") ; 14
! (jetta &jspace "(foo 11 3)") ; 15

; works with type inference
! (jetta &jspace ; (: fact (-> Int Int))
    "(= (fact $n)
        (if (== $n 0) 1 (* $n (fact (- $n 1)))))")
! (jetta &jspace "(fact 10)") ; 3628800

(: boo (-> Int Int))
(= (boo $x) (* $x 2))
!(compile &jspace "boo")
!(jetta &jspace "(boo 3)") ; 6
!(boo-gnd 4) ; 8

; Can type definitions be loaded separately?
; ! (jetta &jspace "(: foo (-> Int Int Int))")
; ! (jetta &jspace "(= (foo $x $y) (+ $x $y 1))")
; Doesn't work
;! (jetta &jspace "(: foo (-> Int Int Int))
;    (= (foo 0 0) 0)
;    (foo 0 0)
;    ")


