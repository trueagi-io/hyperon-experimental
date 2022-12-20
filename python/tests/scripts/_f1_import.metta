(: g (-> Number Numer))
(= (g $x) (+ $x 100))

(: f (-> Number Number))
(= (f $x) (if (< $x 0) (- 0 $x) (g $x)))
