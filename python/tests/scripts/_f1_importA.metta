!(import! &self _d1_importC.metta)

(: dup (-> Number Number))
(= (dup $x) (if (== $x 0) (- $x 100) (g $x)))

(: f (-> Number Number))
(= (f $x) (if (< $x 0) (- 0 $x) (g $x)))
