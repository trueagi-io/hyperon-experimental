; From matching to rule chaining

; `:=` is a custom symbol
; Combinatory logic rules
(:= (I $x) $x)
(:= ((K $x) $y) $x)
(:= (K $x $y) $x)
(:= (S $x $y $z) ($x $z ($y $z)))

; First step
!(assertEqualToResult
  (match &self
    (:= (S K K x) $r)
    $r)
  ((K x (K x))))

; Second step
!(assertEqualToResult
  (match &self
    (:= (K x (K x)) $r)
    $r)
  (x))

; Combine reduction via matching into two steps
!(assertEqualToResult
  (match &self
    (:= (S K K x) $r)
    (match &self (:= $r $r2)
      $r2))
  (x))

; Another example (Peano summation)
(:= (Add $x Z) $x)
(:= (Add $x (S $y)) (Add (S $x) $y))

; First step
!(assertEqualToResult
  (match &self
    (:= (Add (S Z) (S Z)) $r)
    $r))
  ((Add (S (S Z)) Z))

; Two steps
!(assertEqualToResult
  (match &self
    (:= (Add (S Z) (S Z)) $r)
    (match &self (:= $r $r2)
      $r2))
  ((S (S Z))))
