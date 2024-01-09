!(extend-py! torchme)

; bind a bunch of numpy objects to tokens for convenience
;!(bind! &x (torch.tensor (3 2 3)))
!(bind! &m (torch.tensor ((6 1) (7 2) (8 3))))
!(bind! &a (torch.tensor ((1 1 1) (0 0 0))))
!(bind! &b (torch.tensor ((-1 -1 -1) (1 1 1))))

;!(&m)
;!(&a)
;!(&b)

;!(torch.empty (1 2 3))
;!(torch.tensor (1 2 3))
;!(torch.tensor ((6 1) (7 2) (8 3)))
;!(torch.zeros (2 3))

!(torch.manual_seed 1730)
!(bind! &q (torch.rand (10 3 4)))
!(bind! &w (torch.rand (4 5)))

!(torch.matmul (&q &w))

