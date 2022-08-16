; Variables can appear in knowledge base entries
(implies (Frog $x) (Green $x))
(implies (Frog $x) (Eats-flies $x))

; Bound variables on both sides will be substituted
!(assertEqualToResult
  (match &self
    (implies (Frog Sam) $what)
    $what)
  ((Green Sam)
   (Eats-flies Sam)))

!(assertEqualToResult
  (match &self
    (implies ($P $x) (Green Sam))
    ($x might be $P))
  ((Sam might be Frog)))

; Some facts
(Frog Sam)
(Robot Sophia)

; Composite queries
!(assertEqualToResult
  (match &self
    (, (Frog $x)
       (implies (Frog $x) $y))
    $y)
  ((Green Sam)
   (Eats-flies Sam)))

; Variables in queries can remain unbound
!(assertEqualToResult
  (match &self
    (, (implies ($P $x) (Green $x))
       (implies ($P $x) (Eats-flies $x)))
    (Then it is definitely $P))
  ((Then it is definitely Frog)))
