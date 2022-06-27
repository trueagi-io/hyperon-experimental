(:= (I $x) $x)
(:= ((K $x) $y) $x)
(:= (K $x $y) $x)
(:= (S $x $y $z) ($x $z ($y $z)))


!(match &self
   (:= (S K K x) $r)
   $r)

!(match &self
   (:= (K x (K x)) $r)
   $r)

!("------------------------------")

(:= (Add $x Z) $x)
(:= (Add $x (S $y)) (Add (S $x) $y))

!(match &self
   (:= (Add (S Z) (S Z)) $r)
   $r)

!(match &self
   (:= (Add (S (S Z)) Z) $r)
   $r)
