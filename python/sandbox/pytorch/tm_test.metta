!(extend-py! torchme)

;-------------------------------------
; Tensor creation
;-------------------------------------
; bind a bunch of tensor objects to tokens for convenience
!(bind! &x (torch.tensor (3 2 3)))
!(bind! &m (torch.tensor ((6 1) (7 2) (8 3))))
!(bind! &a (torch.tensor ((1 1 1) (0 0 0))))
!(bind! &b (torch.tensor ((-1 -1 -1) (1 1 1))))

; torch.tensor(t) is equivalent to t.clone().detach()
!(torch.tensor (&x))

!(torch.empty (1 2 3))
;!(torch.tensor (1 2 3))
;!(torch.tensor ((6 1) (7 2) (8 3)))
;!(torch.zeros (2 3))

!(torch.manual_seed 1730)
!(bind! &q (torch.rand (3 4)))
!(bind! &w (torch.rand (4 5)))

;-------------------------------------
; Arithmetic and linear algebra operations
;-------------------------------------
!(torch.add ((torch.zeros (2 2)) 5))
!(torch.mul ((torch.ones (2 2)) -3))
!(torch.matmul (&q &w))

;-------------------------------------
; autograd handling
;-------------------------------------
!(torch.requires_grad_status &q)
!(torch.requires_grad_status &w)

!(torch.requires_grad &q True)
!(torch.requires_grad &w True)

!(torch.requires_grad_status &q)
!(torch.requires_grad_status &w)

!(bind! &tm (torch.matmul (&q &w)))
!(torch.requires_grad_status &tm)

!(bind! &mtm (torch.mean &tm))
!(torch.requires_grad_status &mtm)
!(torch.backward &mtm)


;-------------------------------------
; Instantiate a pytorch (or python) module
;-------------------------------------
;!(bind! &foo_model torch.instantiate_module FooModel tm_test))
;!(torch.to_device &foo_model cuda)
;!(torch.to_device &tm cuda)


;-------------------------------------
; kwargs handling
;-------------------------------------
!(kwargs ((a "a value") (b "b value")))
!(bind! &kwargs_foo (kwargs ((param1 1) (param2 None) (param3 foo))) )
!(torch.instantiate_module Foo tm_test &kwargs_foo)


;-------------------------------------
; Training a Pytorch model
;-------------------------------------

; Define a transform for inputs
!(bind! &to_tensor_transform (torch.instantiate_module ToTensor torchvision.transforms))

; Download training and test data and create datasets
!(bind! &train_data (torch.instantiate_module FashionMNIST torchvision.datasets data True &to_tensor_transform None True))
!(bind! &test_data (torch.instantiate_module FashionMNIST torchvision.datasets data False &to_tensor_transform None True))

; Create data loaders
!(bind! &train_dataloader (torch.instantiate_module DataLoader torch.utils.data &train_data 64))
!(bind! &test_dataloader (torch.instantiate_module DataLoader torch.utils.data &test_data 64))


; Import a Pytorch model
!(bind! &model (torch.instantiate_module NeuralNetwork tm_test))
!(torch.to_device &model cuda)
!(bind! &model_params (torch.get_model_params &model))

; Creating an optimizer
!(bind! &optimizer (torch.instantiate_module SGD torch.optim &model_params 0.001))

; Creating a loss function
!(bind! &loss_fn (torch.instantiate_module CrossEntropyLoss torch.nn))

; Creating a trainer
; Params: class to import, pymodule with a class to import, Dataloader, Pytorch model, loss function optimizer
!(bind! &trainer (torch.instantiate_module Trainer tm_test &train_dataloader &model &loss_fn &optimizer))

; Run train cycle
; Params: instance of the Trainer class, number of epochs
;!(torch.run_trainer &trainer 3)


