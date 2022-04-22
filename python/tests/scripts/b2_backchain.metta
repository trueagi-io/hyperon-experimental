;(example from OpenCog Classic wiki on PLN Backward Chaining)

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
   (match &self (Evaluation ($P $x)) ($P $x)))

(= (deduce (Evaluation ($P $x)))
   (match &self
     (Implication $a (Evaluation ($P $x)))
     ($x proven by (deduce $a))))

(= (deduce (And $a $b))
   (And (deduce $a) (deduce $b)))

!(println! (deduce (Evaluation (mortal $x))))
