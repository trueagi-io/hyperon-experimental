;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; From matching to evaluation of expressions via equality queries chaining
; It's possible to chain together matching queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some knowledge base entries
; `:=` is a custom symbol
; Combinatory logic rules
(:= (I $x) $x)
(:= ((K $x) $y) $x)
(:= (K $x $y) $x)
(:= (S $x $y $z) ($x $z ($y $z)))

; A single matching step
!(assertEqualToResult
  (match &self
    (:= (S K K x) $r)
    $r)
  ((K x (K x))))

; A second matching step; the result of the previous
; step is manually used as input for the next query
!(assertEqualToResult
  (match &self
    (:= (K x (K x)) $r)
    $r)
  (x))

; Combined reduction via matching in two steps
!(assertEqualToResult
  (match &self
    (:= (S K K x) $r)
    (match &self (:= $r $r2)
      $r2))
  (x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Another example (Peano summation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(:= (Add $x Z) $x)
(:= (Add $x (S $y)) (Add (S $x) $y))

; First step
!(assertEqualToResult
  (match &self
    (:= (Add (S Z) (S Z)) $r)
    $r)
  ((Add (S (S Z)) Z)))

; Two steps together
!(assertEqualToResult
  (match &self
    (:= (Add (S Z) (S Z)) $r)
    (match &self (:= $r $r2)
      $r2))
  ((S (S Z))))
