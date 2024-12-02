;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example file used by f1_imports.metta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(import! &self f1_moduleC)

!(assertEqual (+ 1 2) 3)
!(assertEqual (if True "S" "F") "S")

(= (dup $x) (if (== $x 0) (+ $x 10) (g $x)))

(: f (-> Number Number))
(= (f $x) (if (< $x 0) (- 0 $x) (g (+ 1 $x))))
