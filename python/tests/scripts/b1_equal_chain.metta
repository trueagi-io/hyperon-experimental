;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The equality sign is specially treated by the interpreter.

; The evaluation of any (expr) in MeTTa is done via
; constructing the following equality query:
; (match &self (= (expr) $r) $r)
; The result $r is recursively evaluated in the same way,
; until the match is empty (when the equality match is
; empty, the expression is evaluated to itself).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (I $x) $x)
(= ((K $x) $y) $x) ; Note that `(K $x)` is allowed here
(= (K $x $y) $x)
(= (S $x $y $z) ($x $z ($y $z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `assertEqual` compares (sets of) results of evaluation
; of two expressions (both expressions can have empty
; sets of results, so `assertEqualToResult` is safer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (S K K x)
  x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Here, we use `assertEqualToResult` to make sure
; that the expression is evaluated to itself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqualToResult
   (expression without known equalities)
  ((expression without known equalities)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Another example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

!(assertEqual
  (Add (S Z) (S Z))
  (S (S Z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In the current implementation, child expressions
; are reduced
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (Something? (Add (S Z) (S Z)))
  (Something? (S (S Z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note that the same variable can be used multiple times on the left
; side of expressions, and that the "function" doesn't need to
; cover all possible inputs (i.e. it can be "non-total")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (eq $x $x) T)
!(assertEqual
  (eq (S K K x) x)
  T)
!(assertEqualToResult
   (eq Green Blue)
  ((eq Green Blue)))
