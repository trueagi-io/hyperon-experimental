!(import! &self simple_import)

!(import numpy)
!(import numpy)
!(import numpy.linalg)


!(bind! m1 (call_dot2 numpy random rand 3 3 ))
!(bind! m1_inv (call_dot2 numpy linalg inv m1))

!(__unwrap (call_dot numpy matmul m1 m1_inv))
