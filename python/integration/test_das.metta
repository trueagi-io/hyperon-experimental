!(import! &self das_gate)

!(bind! &das (new-das))
;!(bind! &das (new-remote-das (Error "Put your IP here" ()) 8081))

(= (cdr $x)
    (cdr-atom $x))

(= (match_res_len $match_res)
    (if ( == (cdr $match_res) ())
        1
        (+ 1 (match_res_len (cdr $match_res)))))

;!(match_res_len(collapse (match &space (Test $x) (This $x works))))

!(add-atom &das (Test (Test A)))
!(add-atom &das (Best (Test A)))
!(add-atom &das Test)

; The simplest match with grounding the variable in the node
!(match &das ($v1 (Test A)) (This $v1 works))

!(assertEqual
    (match &das ($v1 (Test A)) (This $v1 works))
    (superpose ((This Test works) (This Best works))))

;!(assertEqualToResult
;    (match &das ($v1 (Test A)) (This $v1 works))
;   ((This Test works) (This Best works)))

; The simplest match with grounding the variable in the link
!(match &das (Test $v2) (This $v2 works))

!(assertEqualToResult
    (match &das (Test $v2) (This $v2 works))
    ((This (Test A) works)))

!(add-atom &das (Rest (Test B)))
!(add-atom &das (Test (Test B)))

; Compositional (And) query test
!(match &das (, (Best $x) ($v $x)) ($v $x))
; !(match &das (, ($v1 $x) (Test $x)) ($v1 Test $x))

; !(match &das ($v1 ($v2 A)) (This $v1 works))