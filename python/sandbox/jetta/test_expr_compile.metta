!(import! &self compile)

; creating a space for repetitive use
!(bind! &jspace (new-jetta-space))

; check that compiling expressions work
(: boo (-> Int Int))
(= (boo $x) (* $x 2))
!(compile &jspace "boo")
!(assertEqual
  (jetta &jspace "(boo 3)")
  6)
!(assertEqual
  (boo-gnd 4)
  8)

; TODO: remove type for testing, when it works
(: my-goo (-> Int Int Int))
(= (my-goo $x $y) (+ 1 (- $x $y)))
!(compile &jspace my-goo 2)
!(assertEqual
  (jetta &jspace "(my-goo 4 3)")
  2)
!(assertEqual
  (my-goo-gnd 4 5)
  0)

; error: compile expects a function name
!(assertEqual
  (< 0
     ((py-dot
       (case (compile &jspace (ffff a b))
         (((Error $1 (JettaCompileError $2)) $2)))
      "find") "function name"))
  True)
