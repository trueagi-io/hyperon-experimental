;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `match` searches for all expressions corresponding to
; the given pattern and produces the output pattern.
; It doesn't search in subexpressions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some expressions to be matched
((leaf1 leaf2) leaf3)
(((leaf0 leaf1) leaf2) leaf3)
; This one contains `((leaf1 leaf2) leaf3)` as a subexpression
; and thus will not be matched
(top ((leaf1 leaf2) leaf3))
; `assertEqualToResult` checks all the results of the first
; expression; it doesn't evaluate the second expression,
; which is treated as a set of expected results.
!(assertEqualToResult
  (match &self (($x leaf2) leaf3) $x)
  (leaf1
   (leaf0 leaf1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Symbols can be arranged in arbitrary expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(Sam is a frog)
(Tom is a cat)
(Sophia is a robot)
; `match` can bind more that one variable
!(assertEqualToResult
  (match &self ($who is a $what) ($who the $what))
  ((Sam the frog)
   (Tom the cat)
   (Sophia the robot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; More examples of pattern matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `:=` is a custom symbol. These are still purely symbolic expressions
(:= (Green Sam) T)
(:= (White Tom) T)
(:= (Green Tom) F)
!(assertEqualToResult
  (match &self (:= (Green $who) T) ($who is really green))
  ((Sam is really green)))
!(assertEqualToResult
  (match &self (:= ($color $who) T) ($who is really $color))
  ((Sam is really Green)
   (Tom is really White)))
!(assertEqualToResult
  (match &self (:= ($color $who) $tv) (It's $tv that $who is $color))
  ((It's T that Sam is Green)
   (It's T that Tom is White)
   (It's F that Tom is Green)))
; This type of query works as a sort of evaluation/reduction
!(assertEqualToResult
  (match &self (:= (Green Tom) $tv) $tv)
  (F))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; One more example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isa red color)
(isa green color)
(isa blue color)
!(assertEqualToResult
  (match &self (isa $color color) $color)
  (red green blue))
