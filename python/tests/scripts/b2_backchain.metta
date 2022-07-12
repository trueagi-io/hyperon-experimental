; `match` can be used in equalities, which is typically
; used for querying and reasoning over declarative knwoledge

(Frog Sam)
(= (frog $x) (match &self (Frog $x) T))
; `(Frog Sam)` is not reduced; it is just a declaration
!(assertEqualToResult
   (Frog Sam)
  ((Frog Sam)))
; `frog` uses this declaration
!(assertEqual
  (frog Sam)
  T)
!(assertEqualToResult
  (frog Fritz)
  ())


; The result of matching is also chained
; Example from OpenCog Classic wiki on PLN Backward Chaining
; `And` and `T` are custom symbols (they are used here
; to avoid mixing them up with symbols from common lib)

(Evaluation (philosopher Plato))
(Evaluation (likes-to-wrestle Plato))

(Implication
   (And (Evaluation (philosopher $x))
        (Evaluation (likes-to-wrestle $x)))
   (Evaluation (human $x)))

(Implication
   (Evaluation (human $x))
   (Evaluation (mortal $x)))

(= (deduce (Evaluation ($P $x)))
   (match &self (Evaluation ($P $x)) T))
(= (deduce (Evaluation ($P $x)))
   (match &self
     (Implication $a (Evaluation ($P $x)))
     (deduce $a)))
(= (deduce (And $a $b))
   (And (deduce $a) (deduce $b)))
(= (And T T) T)

!(assertEqual
  (deduce (Evaluation (mortal Plato)))
  T)

; TODO : Some variagles are not substituted
(= (ift T $then) $then)
(ift (deduce (Evaluation (mortal $x))) $x)


(= (explain (Evaluation ($P $x)))
   (match &self (Evaluation ($P $x)) ($P $x)))

(= (explain (Evaluation ($P $x)))
   (match &self
     (Implication $a (Evaluation ($P $x)))
     (($P $x) proven by (explain $a))))

(= (explain (And $a $b))
   (And (explain $a) (explain $b)))

(explain (Evaluation (mortal $x)))

