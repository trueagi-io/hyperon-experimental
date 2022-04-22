!(import! &kb "c2_spaces_kb.metta")

!(println! (match &kb
   (, ($obj is $prop)
      ($prop is-a Color)
      ($obj is-a Planet)) $obj))
