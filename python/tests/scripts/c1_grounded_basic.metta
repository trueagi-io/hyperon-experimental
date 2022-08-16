; Standard library contains a few basic grounded operations

; Implementation of grounded arithmetics may change location,
; but these examples should work
!(assertEqualToResult
  (+ 2 (* 3 5.5))
  (18.5))

!(assertEqualToResult
  (- 8 (/ 4 6.4))
  (7.375))

!(assertEqualToResult
  (% 21 17)
  (4))

!(assertEqualToResult
  (< 4 (+ 2 (* 3 5)))
  (True))

!(assertEqualToResult
  (and (> 4 2) (< 4 3))
  (False))

; Currently, `ln` is not a grounded symbol.
; If we don't define its type, then its application
; will not be refuse, but will be unreduced.
; Currently, its grounded argument will be reduced.
!(assertEqualToResult
  (ln (+ 2 2))
  ((ln 4)))

; Similarly, passing ordinary symbols to grounded
; operations will not cause errors and simply remain
; unreduced, if it type-checks.
!(assertEqualToResult
  (> 4 (+ ln 2))
  ((> 4 (+ ln 2))))

; This will also remain unreduced unless
; grounded operations can do pattern matching
!(assertEqualToResult
  (> 4 (+ $x 2))
  ((> 4 (+ $x 2))))

; `==` is now implemented to work with both grounded and
; symbolic atoms (while remaining a grounded operation)
!(assertEqualToResult
  (== 4 (+ 2 2))
  (True))
!(assertEqualToResult
  (== 2 3)
  (False))
!(assertEqualToResult
  (== (A B) (A B))
  (True))
!(assertEqualToResult
  (== (A B) (A (B C)))
  (False))
; Unlike `<` or `>`, it will not remain unreduced if one
; of its arguments is gounded, while another is not.
; Instead, it will return `False`
!(assertEqualToResult
  (== 4 (+ ln 2))
  (False))

; Grounded symbols have predefined types.
; If automatic type-checking is not turned on,
; evaluation of ill-typed expressions produces
; empty result
!(assertEqualToResult
  (+ 2 "String")
  ())
; Custom symbols as arguments of grounded operations
; work similarly
(: ln LN)
!(assertEqualToResult
  (== 4 (+ ln 2))
  ())

; Custom functions over grounded functions behave ordinarily
(= (sqr $x) (* $x $x))
!(assertEqualToResult
  (sqr 4)
  (16))

; Evaluation of grounded operations over nondeterministic
; expressions work in the same way as of custom symbols
!(assertEqual
  (+ (superpose (1 2 3)) 1)
  (superpose (2 3 4)))

(= (bin) 0)
(= (bin) 1)
!(assertEqualToResult
  (+ 1 (bin))
  (1 2))

!(assertEqualToResult
  (let $x (+ 2 3)
    (* $x $x))
  (25))

; Grounded symbols work with non-determinism based "reasoning"

; `$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)
(= (gen $n)
   (if (> $n 0)
       (:: (bin) (gen (- $n 1)))
       nil))
(= (subsum nil nil) 0)
(= (subsum (:: $x $xs) (:: $b $bs))
   (+ (* $x $b)
      (subsum $xs $bs)))

!(assertEqualToResult
  (let $t (gen 3)
    (if (== (subsum (:: 3 (:: 7 (:: 5 nil))) $t) 8) $t (nop)))
  ((:: 1 (:: 0 (:: 1 nil)))))
