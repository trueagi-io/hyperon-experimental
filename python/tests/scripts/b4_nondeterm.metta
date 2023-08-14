;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Nondeterminism in matching and interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (color) green)
(= (color) yellow)
(= (color) red)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In `a1_symbols` we saw that `match` can return multiple or no results
; The same is true for equality queries
; `superpose` turns a tuple into a non-deterministic result (for comparison here)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (match &self (= (color) $x) $x)
  (superpose (red yellow green)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `collapse` converts a nondeterministic result into a tuple
; We don't use it above, because the order of non-deterministic results
; is not guaranteed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (collapse
    (match &self (= (shape) $x) $x))
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Multiple results from equality queries
; are also returned by the interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (color)
  (superpose (red yellow green)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `superpose` reverts `collapse`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `let` is used to introduce temporary variable bindings within an expression
!(assertEqual
  (color)
  (let $x (collapse (color)) (superpose $x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In contrast to `match`, if the equality query returns an empty result
; the interpreter doesn't reduce a symbolic expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (collapse (shape))
  ((shape)))
!(assertEqualToResult
   (shape)
  ((shape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If the expression is composed over nondeterministic
; expressions, all combinations will be produced
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (bin) A)
(= (bin) B)
(= (pair $x $y) ($x $y))
!(assertEqualToResult
  (pair (bin) (bin))
  ((A A) (A B) (B A) (B B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For the ordinary symbolic expression, if its subexpressions
; are nondeterministic, and no reduction rule is available
; for some combinations of their values, the whole expression
; for these values will appear in the set of final results,
; but without reduction. However, `match` can be called
; directly to filter out non-reducible results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (foo) red)
(= (foo) boo)
(= (eq $x $x) T)
(= (find-equal $x $y)
   (match &self (= (eq $x $y) T) $x))
!(assertEqual
  (find-equal (color) (foo))
  red)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; One can pass nondeterministic expressions to an ordinary
; function and get multiple results.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (rev $x $y) ($y $x))
!(assertEqualToResult
  (rev A (superpose (B C D)))
  ((B A) (C A) (D A)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extended example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some rules
(= (ift T $then) $then)
(= (And T T) T)
(= (make $x) (ift (makes $y $x) (start $y)))
(= (make $x) (ift (And (prevents (making $y) (making $x))
                       (makes $z $y)) (stop $z)))

; Add facts to knowledge base
(= (prevents (making (air dry)) (making (air wet))) T)
(= (prevents (making (air wet)) (making (air dry))) T)
(= (makes humidifier (air wet)) T)
(= (makes kettle (air wet)) T)
(= (makes ventilation (air dry)) T)
(= (is (air dry)) (make (air wet)))
(= (is (air wet)) (make (air dry)))

; the results are not sequential in sense that their order is arbitrary
!(assertEqual
  (is (air dry))
  (superpose ((stop ventilation) (start kettle) (start humidifier))))
!(assertEqual
  (is (air wet))
  (superpose ((stop kettle) (stop humidifier) (start ventilation))))
