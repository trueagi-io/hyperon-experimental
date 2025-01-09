!(import! &self compile)

!(bind! &jspace (new-jetta-space))
!(jetta &jspace "
            (@ foo multivalued)
            (: foo (-> Int))
            (= (foo) (seq 1 2 3))

            (@ bar multivalued)
            (: bar (-> Int))
            (= (bar) (+ 1 (if (>= (foo) 2) (+ 1 (foo)) 0)))")
!(assertEqualToResult
   (jetta &jspace "(foo)")
   (1 2 3))
!(assertEqualToResult
   (jetta &jspace "(bar)")
   (1 3 4 5 3 4 5))

(@ goo multivalued)
(: goo (-> Int))
(= (goo) (seq 1 2 3))
; FIXME: `(= (goo) (+ 1 (seq 1 2 3)))` doesn't work
!(compile &jspace goo)
;(@ doo multivalued)
(: doo (-> Int Int))
(= (doo $x) (+ $x (goo)))
!(compile &jspace doo)
!(assertEqualToResult
  (doo-gnd 1)
  (2 3 4))
