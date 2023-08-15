;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `match` can be used inside equalities, which is typically
; used for querying and reasoning over declarative knowledge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fact
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The result of matching is also chained
; Example from OpenCog Classic wiki on PLN Backward Chaining
; `And` and `T` are custom symbols (they are used here
; to avoid mixing them up with symbols from common lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some facts in the knowledge base
(Evaluation (philosopher Plato))
(Evaluation (likes-to-wrestle Plato))

; An implication rule
(Implication
   (And (Evaluation (philosopher $x))
        (Evaluation (likes-to-wrestle $x)))
   (Evaluation (human $x)))

; Another implication rule
(Implication
   (Evaluation (human $x))
   (Evaluation (mortal $x)))

; Deduction case when the desired evaluation is present in
; the knowledge base
(= (deduce (Evaluation ($P $x)))
   (match &self (Evaluation ($P $x)) T))

; Deduction case when the desired evaluation is the result
; of an implication, which implies a recursion
(= (deduce (Evaluation ($P $x)))
   (match &self
     (Implication $a (Evaluation ($P $x)))
     (deduce $a)))

; Deduction case for generic "And" expressions;
; also recursive
(= (deduce (And $a $b))
   (And (deduce $a) (deduce $b)))

; True & True = True
(= (And T T) T)

; Test deduction that Plato is mortal
!(assertEqual
  (deduce (Evaluation (mortal Plato)))
  T)

; TODO : Some variables are not substituted
(= (ift T $then) $then)
(ift (deduce (Evaluation (mortal $x))) $x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Another example of backchaining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definition of explain when the desired Evaluation is
; in the knowledge base
(= (explain (Evaluation ($P $x)))
   (match &self (Evaluation ($P $x)) ($P $x)))

; Definition of explain when the desired Evaluation is
; the result of an implication
(= (explain (Evaluation ($P $x)))
   (match &self
     (Implication $a (Evaluation ($P $x)))
     (($P $x) proven by (explain $a))))

; Definition of explain for And
(= (explain (And $a $b))
   (And (explain $a) (explain $b)))

; Example of explain why Plato is mortal
!(assertEqual
  (explain (Evaluation (mortal Plato)))
  ((mortal Plato)
    proven by ((human Plato)
      proven by (And
        (philosopher Plato)
        (likes-to-wrestle Plato)))))
