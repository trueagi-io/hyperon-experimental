!(import! &self simple_import)

!(import_from example_01 import simple_fun)
!(import_from example_01 import SimpleObject)

!(bind! so (SimpleObject))

; it is important that obj will have type SimpleObject when passed to simple_fun!
!(simple_fun 1 2 "3" (kwarg1 2) (obj so) )

!(call_dot so method "arg1" "arg2" (arg3 3))