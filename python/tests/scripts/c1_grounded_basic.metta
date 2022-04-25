;(a few basic grounded operations)

!(println!
  (+ 2 (* 3 5.5))
)

!(println!
  (< 4 (+ 2 (* 3 5)))
)

!(println!
  (== 4 (+ 2 2))
)

!(println!
  (and (> 4 2) (< 4 3))
)

;(this will not work unless grounded operations can do pattern matching)
!(println!
  (== 4 (+ $x 2))
)
!(println!
  (ln (+ 2 2))
)

;(grounded symbols work with non-determinism based "reasoning")

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

!(println!
   (let $t (gen 3)
      (if (== (subsum (:: 3 (:: 7 (:: 5 nil))) $t) 8) $t (nop ()))))
