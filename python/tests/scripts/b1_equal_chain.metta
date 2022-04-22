(= (I $x) $x)
(= ((K $x) $y) $x)
(= (K $x $y) $x)
(= (S $x $y $z) ($x $z ($y $z)))

!(println! (S K K x))

!(println! "------------------------------")

(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

!(println! (Add (S Z) (S Z)))

;(In the current implementation, the argument will be reduced)
!(println! (Something? (Add (S Z) (S Z))))

