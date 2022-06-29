;(a few basic grounded operations)

!(+ 2 (* 3 5.5))

!(< 4 (+ 2 (* 3 5)))

!(== 4 (+ 2 2))

!(and (> 4 2) (< 4 3))

;(this will not work unless grounded operations can do pattern matching)
!(== 4 (+ $x 2))
!(ln (+ 2 2))

; (grounded symbols work with non-determinism based "reasoning")

; `$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)
(= (bin) 0)
(= (bin) 1)
(= (gen $n)
   (if (> $n 0)
       (:: (bin) (gen (- $n 1)))
       nil))
(= (subsum nil nil) 0)
(= (subsum (:: $x $xs) (:: $b $bs))
   (+ (* $x $b)
      (subsum $xs $bs)))

!(let $t (gen 3)
    (if (== (subsum (:: 3 (:: 7 (:: 5 nil))) $t) 8) $t (nop)))
