!(bind! simple_fun (py-atom example_01.simple_fun))
!(bind! SimpleObject (py-atom example_01.SimpleObject))

!(bind! so (SimpleObject))


; it is important that obj will have type SimpleObject when passed to simple_fun!
!(simple_fun 1 2 "3" (Kwargs (kwarg1 2) (obj so)) )

!( (py-dot so method) "arg1" "arg2" (Kwargs (arg3 3)) )
