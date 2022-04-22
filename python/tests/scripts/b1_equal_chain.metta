(= (I $x) $x)
(= ((K $x) $y) $x)
(= (K $x $y) $x)
(= (S $x $y $z) ($x $z ($y $z)))

!(println! (S K K x))

;(------------------------------)

(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

!(println! (Add (S Z) (S Z)))

;(In the current implementation, it will be reduced)
!(println! (Something? (Add (S Z) (S Z))))

