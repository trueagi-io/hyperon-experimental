; Equality sign is specially treated by the interpreter
(= (I $x) $x)
(= ((K $x) $y) $x)
(= (K $x $y) $x)
(= (S $x $y $z) ($x $z ($y $z)))

; Evaluation of (expr) is done via constructing
; the equality query (match &self (= (expr) $r) $r)
; The result $r is then evaluated in the same way
; until the match is not empty (if equality match is
; empty, the expression is evaluated to itself)
!(assertEqual
  (S K K x)
  x)

!(assertEqual
  (expression without known equalities)
  (expression without known equalities))


; Another example
(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

!(assertEqual
  (Add (S Z) (S Z))
  (S (S Z)))

; In the current implementation, the child expression will be reduced
!(assertEqual
  (Something? (Add (S Z) (S Z)))
  (Something? (S (S Z))))
