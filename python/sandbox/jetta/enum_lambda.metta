!(import! &self compile)

!(bind! &jspace (new-jetta-space))

!(jetta &jspace "
(: err (-> Int Int Int))
(= (err $a $b)
   (* (- $a $b) (- $a $b)))
(: calc_err (-> (-> Int Int) Int))
(= (calc_err $f)
   (+ (err ($f 1) 8)
      (err ($f 2) 13)
      (err ($f 5) 40)
      (err ($f 10) 125)
   )
")

(= (bin) 0)
(= (bin) 1)
(= (num Z) (bin))
(= (num (S $x))
   (+ (* 2 (num $x)) (bin)))
(= (num8) (num (S (S (S Z)))))

(= (rnd-square)
   (\ ($x)
      (+ (* (bin) (* $x $x))
         (+ (* (num8) $x) (num8))))
)

!(let $f (rnd-square)
  (if (== (jetta &jspace (calc_err $f)) 0)
      $f
      Empty))
