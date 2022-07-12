(= (if True $then $else) $then)
(= (if False $then $else) $else)
(= (min $a $b) (if (< $a $b) $a $b))
(= (s-tv (stv $s $c)) $s)
(= (c-tv (stv $s $c)) $c)

(= (TV $x)
   (match &self
     (.tv $x $stv)
     $stv
   )
)

(= (TV (And $a $b))
   (stv (min (s-tv (TV $a)) (s-tv (TV $b)))
        (min (c-tv (TV $a)) (c-tv (TV $b)))
   )
)

(= (TV $x)
   (match &self
     (.tv (Implication $y $x)
          (stv $s $c))
     (stv (* $s (s-tv (TV $y)))
          (* $c (c-tv (TV $y))))
   )
)

(.tv
  (Implication
     (And (croaks $x)
          (eat_flies $x))
     (frog $x))
  (stv 0.9 0.8)
)

(.tv (croaks Fritz)
     (stv 0.95 0.85))
(.tv (eat_flies Fritz)
     (stv 0.87 0.95))

(.tv
  (Implication
     (frog $x)
     (green $x))
  (stv 0.9 1.0)
)

!(assertEqual
  (TV (croaks Fritz))
  (stv 0.95 0.85))
!(assertEqual
  (TV (frog Fritz))
  (stv 0.783 0.68))
!(assertEqual
  (TV (green Fritz))
  (stv 0.7047 0.68))
