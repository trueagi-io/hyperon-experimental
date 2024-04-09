!(import! &self simple_import)

!(import_as numpy as np)

!(bind! a1 (call_dot np array (ptuple 1 2 3) ))
!(bind! a2 (call_dot a1 __mul__ 3))
!(bind! a3 (call_dot a1 __add__ a2))


!(__unwrap a1)
!(__unwrap a2)
!(__unwrap a3)

!(bind! m1 (call_dot np array (ptuple (1 2 3) (4 4 5) (6 7 8)) ))
!(import_as numpy.linalg as linalg)
!(bind! m1_inv (call_dot linalg inv m1))

!(__unwrap (call_dot np matmul m1 m1_inv))
