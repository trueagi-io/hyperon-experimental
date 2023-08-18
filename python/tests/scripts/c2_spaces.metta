;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Spaces are also represented as grounded symbols in the program.
; Importing another script into a separate space
; TODO: replace symbol by string
!(import! &kb c2_spaces_kb.metta)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We can `match` over `&self` or another space
!(assertEqualToResult
  (match &kb
    (, ($obj is $prop)
       ($prop is-a Color)
       ($obj is-a Planet))
    (Color of Planet $obj is $prop))
  ((Color of Planet Mars is Red)))

; The knowledge in `&kb` is not in `&self`
!(assertEqualToResult
  (match &self ($p is-a Color) $p)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Checking that grounded symbols from different spaces are compatible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (match &kb
    (, (Venus orbit $x au)
       (Mars orbit $y au))
    (- $y $x))
  0.8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Different spaces can be used
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(Ball is Red)
(Chair is High)

!(assertEqualToResult
  (match &kb ($prop is-a Color)
    (match &self ($obj is $prop)
       $obj))
  (Ball))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We can import into `&self`
; TODO: replace symbol by string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(import! &self c2_spaces_kb.metta)
!(assertEqualToResult
  (match &self
    (, ($obj is $prop)
       ($prop is-a Color)
       ($obj is-a Planet))
    (Color of Planet $obj is $prop))
  ((Color of Planet Mars is Red)))
