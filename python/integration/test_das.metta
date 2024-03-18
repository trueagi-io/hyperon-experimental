!(import! &self das_gate)

!(bind! &das (new-das))
;!(bind! &das (new-remote-das (Error "Put your IP here" ()) 8081))

!(add-atom &das (Test (Test 2)))
!(add-atom &das (Best (Test 2)))
!(add-atom &das Test)

; The simplest match with grounding the variable in the node
!(match &das ($v1 (Test 2)) (This $v1 works))
; The simplest match with grounding the variable in the link
!(match &das (Test $v2) (This $v2 works))

!(add-atom &das (Rest (Test 3)))
!(add-atom &das (Test (Test 3)))

; Compositional (And) query test
!(match &das (, (Best $x) ($v $x)) ($v $x))
; !(match &das (, ($v1 $x) (Test $x)) ($v1 Test $x))

; !(match &das ($v1 ($v2 2)) (This $v1 works))
