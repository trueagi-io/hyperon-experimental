!(import! &self compile)

; testing evaluation of a simple expression
; in a new jetta space using textual api
!(assertEqual
   (jetta (new-jetta-space) "(+ 1 2)")
   3)

; creating a space for repetitive use
!(bind! &jspace (new-jetta-space))

; passing text code of a typed function definition
!(jetta &jspace
  "(: foo (-> Int Int Int))
   (= (foo $x $y) (+ $x $y 1))")
; checking that the function can be evaluated multiple times,
; and it doesn't block evaluation of other expressions
!(assertEqual
  (jetta &jspace "(foo 11 2)")
  14)
!(assertEqual
  (jetta &jspace "(foo 11 3)")
  15)
!(assertEqual
  (jetta &jspace "(+ 11 12)")
  23)

; TODO: fix the behavior (one or multiple results)
;(jetta &jspace
;  "(foo 11 3)
;   (foo 11 4)")

; TODO: type inference doesn't work in this simple case
;!(jetta &jspace
;  "(= (goo $x) (+ $x 1))
;   (goo 1)")

; checking if recursion with type definitions work
; also checking `-` in names
; TODO: use <= and /
!(assertEqual
  (jetta &jspace
    "(: log-int (-> Int Int))
     (= (log-int $x)
        (if (== $x 1) 0 (+ 1 (log-int (- $x 1))))
     )
     (log-int 8)")
   7)

; checking recursion with type inference
!(jetta &jspace
   "(= (fact $n)
       (if (== $n 0) 1 (* $n (fact (- $n 1)))))")
!(assertEqual
  (jetta &jspace "(fact 10)")
  3628800)

; Can type definitions be loaded separately?
; ! (jetta &jspace "(: foo (-> Int Int Int))")
; ! (jetta &jspace "(= (foo $x $y) (+ $x $y 1))")
; Doesn't work
;! (jetta &jspace "(: foo (-> Int Int Int))
;    (= (foo 0 0) 0)
;    (foo 0 0)
;    ")

; TODO: add floats and mixed