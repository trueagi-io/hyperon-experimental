;(example from OpenCog Classic wiki on PLN Backward Chaining)
;(no explicit backward chaining is needed)

(= (And True True) True)
(= (frog $x)
   (And (croaks $x)
        (eat_flies $x)))
(= (croaks Fritz) True)
(= (eat_flies Fritz) True)
(= (green $x)
   (frog $x))

!(println! (green Fritz))

;(= (if True $then) $then)
;!(println! (if (green $x) $x))
