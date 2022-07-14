; Spaces are also represented as grounded symbols in the program.
; Importing another script into a separate space
!(import! &kb "c2_spaces_kb.metta")

; We can `match` over `&self` or another space
!(assertEqualToResult
  (match &kb
    (, ($obj is $prop)
       ($prop is-a Color)
       ($obj is-a Planet))
    (Color of Planet $obj is $prop))
  ((Color of Planet Mars is Red)))
; These are not in `&self`
!(assertEqualToResult
  (match &self ($p is-a Color) $p)
  ())


(Ball is Red)
(Chair is High)

; Different spaces can be used
!(assertEqualToResult
  (match &kb ($prop is-a Color)
    (match &self ($obj is $prop)
       $obj))
  (Ball))

; We can import into `&self`
!(import! &self "c2_spaces_kb.metta")
!(assertEqualToResult
  (match &self
    (, ($obj is $prop)
       ($prop is-a Color)
       ($obj is-a Planet))
    (Color of Planet $obj is $prop))
  ((Color of Planet Mars is Red)))
