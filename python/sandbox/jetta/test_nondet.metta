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
