;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example file used by f1_imports.metta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(import! &self f1_moduleC.metta)

; FIXME? stdlib space is not available at importing time -
; only tokens / grounded operations work, while symbolic functions
; (like `if`) don't work.
; FIXME? Exceptions are not caught by `importOp`
!(assertEqual (+ 1 2) 3)
; !(assertEqual (if True "S" "F") "S")

(= (dup $x) (if (== $x 0) (+ $x 10) (g $x)))

(: f (-> Number Number))
(= (f $x) (if (< $x 0) (- 0 $x) (g (+ 1 $x))))
