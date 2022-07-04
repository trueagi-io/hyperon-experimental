; Variables can appear in knowledge base entries
(implies (Frog $x) (Green $x))
(implies (Frog $x) (Eats-flies $x))

; Bound variables on both sides will be substituted
!(assertEqual
  (collapse
    (match &self
      (implies (Frog Sam) $what)
      $what))
  ((Green Sam)
   (Eats-flies Sam)))

!(assertEqual
  (collapse
    (match &self
      (implies ($P $x) (Green Sam))
      ($x might be $P)))
  ((Sam might be Frog)))

; Some facts
(Frog Sam)
(Robot Sophia)

; Composite queries
!(assertEqual
  (collapse
    (match &self
      (, (Frog $x)
         (implies (Frog $x) $y))
      $y))
  ((Green Sam)
   (Eats-flies Sam)))

; Variables in queries can remain unbound
!(assertEqual
  (collapse
    (match &self
      (, (implies ($P $x) (Green $x))
         (implies ($P $x) (Eats-flies $x)))
      (Then it is definitely $P)))
   ((Then it is definitely Frog)))
