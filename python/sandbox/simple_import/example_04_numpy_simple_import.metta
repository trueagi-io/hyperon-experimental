!(bind! linalg (py-atom numpy.linalg))
!(bind! numpy (py-atom numpy))

!(bind! m1 ((py-dot numpy random.rand) 3 3 ))
!(bind! m1_inv ( (py-dot linalg inv) m1))
!( (py-dot numpy matmul) m1 m1_inv)
