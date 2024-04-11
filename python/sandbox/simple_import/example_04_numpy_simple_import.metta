!(import! &self simple_import)

; with simple "import" it is rather common that we import something
; twice because of submodules in python
; So let's import twice to make sure that it does not cause any problems
!(import numpy)
!(import numpy)
!(import numpy.linalg)


!(bind! m1 (call_dot2 numpy random rand 3 3 ))
!(bind! m1_inv (call_dot2 numpy linalg inv m1))

!(__unwrap (call_dot numpy matmul m1 m1_inv))
