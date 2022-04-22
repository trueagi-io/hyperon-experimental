;(variables in knowledge base entries)
(implies (Frog $x) (Green $x))
(implies (Frog $x) (Eats-flies $x))

!(println!
  (match &self
    (implies (Frog Sam) $what) $what))

!(println!
  (match &self
    (implies ($P $x) (Green Sam)) ($x might be $P)))

!(println!
  (match &self
    (, (implies ($P $x) (Green $x))
       (implies ($P $x) (Eats-flies $x)))
    ($x is definitely $P)))
