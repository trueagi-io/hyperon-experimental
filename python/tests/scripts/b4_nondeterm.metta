; Nondeterminism in matching and interpretation

(= (color) green)
(= (color) yellow)
(= (color) red)

; `match` can return multiple or no results
; `collapse` converts a nondeterministic result into a tuple
(: = (-> Atom Atom Atom)) ; FIXME
!(assertEqual
  (collapse
    (match &self (= (color) $x) $x))
  (green yellow red))
!(assertEqual
  (collapse
    (match &self (= (shape) $x) $x))
  ())

; Multiple results from equality queries
; are also returned by the interpreter
!(assertEqual
  (collapse (color))
  (green yellow red))
; Instead of collapsing the result of nondeterministic expression,
; one can create a nondeterministic result with `superpose`
!(assertEqual
  (color)
  (superpose (green yellow red)))

; However, if the equality query returns an empty result
; the interpreter doesn't reduce a symbolic expression
!(assertEqual
  (collapse (shape))
  ((shape)))
!(assertEqualToResult
   (shape)
  ((shape)))


; If the expression is composed over nondeterministic
; expressions, all combinations will be produced
(= (bin) A)
(= (bin) B)
(= (pair $x $y) ($x $y))
!(assertEqualToResult
  (pair (bin) (bin))
  ((A A) (A B) (B A) (B B)))

; For the ordinary symbolic expression, if its subexpressions
; are nondeterministic, and no reduction rule is available
; for some combinations of their values, the whole expression
; for these values will appear in the set of final results,
; but without reduction. However, `match` can be called
; directly to filter out non-reducible results
(= (foo) red)
(= (foo) boo)
(= (eq $x $x) T)
(= (find-equal $x $y)
   (match &self (= (eq $x $y) T) $x))
!(assertEqual
   (find-equal (color) (foo))
   red)

; One can pass nondeterministic expressions to an ordinary
; function and get multiple results.
(= (rev $x $y) ($y $x))
!(assertEqualToResult
  (rev A (superpose (B C D)))
  ((B A) (C A) (D A)))

; Extended example
(= (ift T $then) $then)
(= (And T T) T)
(= (make $x) (ift (makes $y $x) (start $y)))
(= (make $x) (ift (And (prevents (making $y) (making $x))
                       (makes $z $y)) (stop $z)))

(= (is (air dry)) (make (air wet)))
(= (is (air wet)) (make (air dry)))
(= (prevents (making (air dry)) (making (air wet))) T)
(= (prevents (making (air wet)) (making (air dry))) T)

(= (makes humidifier (air wet)) T)
(= (makes kettle (air wet)) T)
(= (makes ventilation (air dry)) T)
!(assertEqual
  (collapse (is (air dry)))
  ((start humidifier) (start kettle) (stop ventilation)))
!(assertEqual
  (collapse (is (air wet)))
  ((start ventilation) (stop humidifier) (stop kettle)))
