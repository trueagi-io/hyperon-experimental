!(extend-py! torchme)

; bind a bunch of numpy objects to tokens for convenience
;!(bind! &y (torch.Tensor (3 2 3)))
;!(bind! &m (torch.Tensor ((6 1) (7 2) (8 3))))
;!(bind! &mt (torch.Tensor ((1 0 0) (0 1 0))))

;!(torch.empty (1 2 3))
;!(torch.tensor (1 2 3))
;!(torch.tensor ((6 1) (7 2) (8 3)))
;!(torch.zeros (2 3))

;!(torch.manual_seed 1729)
!(torch.rand (2 3))