;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The examples in this file is inspired by OpenCog Classic examples
; from github.com/opencog/pln, in particular from
; examples/pln/conjunction, extended with implication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some helpful definitions
(= (min $a $b) (if (< $a $b) $a $b))
(= (s-tv (stv $s $c)) $s)
(= (c-tv (stv $s $c)) $c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `TV` "metarule" based on `match` to run
; `.tv` is a custom symbol, which is used to connect
; PLN-like expressions with their truth values.
; Some `.tv` facts will be defined below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (TV $x)
   (match &self
     (.tv $x $stv)
     $stv
   )
)

; Definition of TV for a conjunction
; we could use `(.tv (And $a $b) (stv ...))` instead of this
(= (TV (And $a $b))
   (stv (min (s-tv (TV $a)) (s-tv (TV $b)))
        (min (c-tv (TV $a)) (c-tv (TV $b)))
   )
)

; Definition of TV for $x when it's the consequent of an implication
(= (TV $x)
   (match &self
     (.tv (Implication $y $x)
          (stv $s $c))
     (stv (* $s (s-tv (TV $y)))
          (* $c (c-tv (TV $y))))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; First example
; .tv serves as a special type of equality when
; matched by the TV "metarules" above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some facts
(.tv (Evaluation (Predicate P) (Concept A))
     (stv 0.5 0.8))
(.tv (Evaluation (Predicate P) (Concept B))
     (stv 0.3 0.9))
(.tv (Implication (Evaluation (Predicate P) (Concept A))
                  (Evaluation (Predicate F) (Concept A)))
     (stv 0.8 1.0))

; FIXME? `(PA)` and `(PB)` are used because otherwise
; substitution is not invoked (atm of test creation)
(= (PA) (Evaluation (Predicate P) (Concept A)))
(= (PB) (Evaluation (Predicate P) (Concept B)))
(= (FA) (Evaluation (Predicate F) (Concept A)))

; Tests
!(assertEqual (TV (And (PA) (PB))) (stv 0.3 0.8))
!(assertEqual (TV (FA)) (stv 0.4 0.8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Second example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Implication rule
(.tv
  (Implication
     (And (croaks $x)
          (eat_flies $x))
     (frog $x))
  (stv 0.9 0.8)
)

; Some facts
(.tv (croaks Fritz)
     (stv 0.95 0.85))
(.tv (eat_flies Fritz)
     (stv 0.87 0.95))

(.tv
  (Implication
     (frog $x)
     (green $x))
  (stv 0.9 1.0)
)

; Tests
!(assertEqual
  (TV (croaks Fritz))
  (stv 0.95 0.85))
!(assertEqual
  (TV (frog Fritz))
  (stv 0.783 0.68))
!(assertEqual
  (TV (green Fritz))
  (stv 0.7047 0.68))
