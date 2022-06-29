(= (I $x) $x)
(= ((K $x) $y) $x)
(= (K $x $y) $x)
(= (S $x $y $z) ($x $z ($y $z)))

!(S K K x)

!("------------------------------")

(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

!(Add (S Z) (S Z))

;(In the current implementation, the argument will be reduced)
!(Something? (Add (S Z) (S Z)))
