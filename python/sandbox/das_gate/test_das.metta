!(extend-py! dasgate)

!(bind! &das (new-das))

!(add-atom &das (Test (Test 2)))
!(add-atom &das Test)

!(match &das ($v1 (Test 2)) (This $v1 works))
; !(match &das ($v1 ($v2 2)) (This $v1 works))
