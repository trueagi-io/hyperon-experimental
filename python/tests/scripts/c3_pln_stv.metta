(= (min $a $b) (if (< $a $b) $a $b))
(= (s-tv (stv $s $c)) $s)
(= (c-tv (stv $s $c)) $c)

; .tv serves as a special type of equality that requires
; `TV` "metarule" based on `match` to run
(= (TV $x)
   (match &self
     (.tv $x $stv)
     $stv
   )
)

; we could use `(.tv (And $a $b) (stv ...))` instead of the equality
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


; Reproducing examples/pln/conjunction (extended with implication)
; from github.com/opencog/pln in OpenCog Classic style... kind of
(.tv (Evaluation (Predicate P) (Concept A))
     (stv 0.5 0.8))
(.tv (Evaluation (Predicate P) (Concept B))
     (stv 0.3 0.9))
(.tv (Implication (Evaluation (Predicate P) (Concept A))
                  (Evaluation (Predicate F) (Concept A)))
     (stv 0.8 1.0))
; FIXME? `(PA)` and `(PB)` are used because otherwise
; substitution is not invoked (atm of test creation)
(= (PA) (Evaluation (Predicate P) (Concept A)))
(= (PB) (Evaluation (Predicate P) (Concept B)))
(= (FA) (Evaluation (Predicate F) (Concept A)))
!(assertEqual (TV (And (PA) (PB))) (stv 0.3 0.8))
!(assertEqual (TV (FA)) (stv 0.4 0.8))

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
