!(bind! np (py-atom numpy))

!(bind! a1 ( (py-dot np array) (py-atom (py-tuple (1 2 3)) )))
!(bind! a2 ( (py-dot a1 __mul__) 3))
!(bind! a3 ( (py-dot a1 __add__) a2))


!(a1)
!(a2)
!(a3)

!(bind! m1 ((py-dot np array) (py-atom (py-list ((1 2 3) (py-list (4 4 5)) (py-tuple (6 7 8))) ))))
!(bind! linalg (py-atom numpy.linalg))
!(bind! m1_inv ( (py-dot linalg inv) m1))

!( (py-dot np matmul) m1 m1_inv)
