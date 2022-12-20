!(import! &self _d1_importC.metta)

(: dup (-> Number Number))
(= (dup $x) (if (== $x 0) (g $x) (- $x 100)))
