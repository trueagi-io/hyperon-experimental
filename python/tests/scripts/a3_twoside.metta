;(variables in knowledge base entries)
(implies (Frog $x) (Green $x))
(implies (Frog $x) (Eats-flies $x))

!(match &self
   (implies (Frog Sam) $what) $what)

!(match &self
   (implies ($P $x) (Green Sam)) ($x might be $P))

!(match &self
   (, (implies ($P $x) (Green $x))
      (implies ($P $x) (Eats-flies $x)))
   ($x is definitely $P))
