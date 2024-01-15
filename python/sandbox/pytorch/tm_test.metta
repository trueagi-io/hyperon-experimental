!(extend-py! torchme)

; bind a bunch of tensor objects to tokens for convenience
!(bind! &x (torch.tensor (3 2 3)))

; torch.tensor(t) is equivalent to t.clone().detach()
;!(torch.tensor (&x))

;!(bind! &m (torch.tensor ((6 1) (7 2) (8 3))))
;!(bind! &a (torch.tensor ((1 1 1) (0 0 0))))
;!(bind! &b (torch.tensor ((-1 -1 -1) (1 1 1))))

;!(&m)
;!(&a)
;!(&b)

;!(torch.empty (1 2 3))
;!(torch.tensor (1 2 3))
;!(torch.tensor ((6 1) (7 2) (8 3)))
;!(torch.zeros (2 3))

;!(torch.manual_seed 1730)
;!(bind! &q (torch.rand (3 4)))
;!(bind! &w (torch.rand (4 5)))
;!(torch.requires_grad &q True)
;!(torch.requires_grad &w True)

;!(torch.requires_grad_status &q)
;!(torch.requires_grad_status &w)

;!(bind! &tm (torch.matmul (&q &w)))
;!(torch.requires_grad_status &tm)

;!(torch.requires_grad &tm True)
;!(torch.requires_grad_status &tm)

;!(bind! &mtm (torch.mean &tm))
;!(torch.requires_grad_status &mtm)

;!(torch.backward &mtm)

;!(bind! &foo_model (torch.import_module FooModel tm_test))
;!(torch.to_device &foo_model cuda)
;!(torch.to_device &tm cuda)

;-------------------------------------
; Training a Pytorch model
;-------------------------------------

; Define a transform for inputs
!(bind! &to_tensor_transform (torch.import_module ToTensor torchvision.transforms))

; Download training and test data and create datasets
!(bind! &train_data (torch.import_module FashionMNIST torchvision.datasets data True &to_tensor_transform None True))
!(bind! &test_data (torch.import_module FashionMNIST torchvision.datasets data False &to_tensor_transform None True))

; Create data loaders
!(bind! &train_dataloader (torch.import_module DataLoader torch.utils.data &train_data 64))
!(bind! &test_dataloader (torch.import_module DataLoader torch.utils.data &test_data 64))


; Import a Pytorch model
!(bind! &model (torch.import_module NeuralNetwork tm_test))
!(torch.to_device &model cuda)
!(bind! &model_params (torch.get_model_params &model))

; Creating an optimizer
!(bind! &optimizer (torch.import_module SGD torch.optim &model_params 0.001))

; Creating a loss function
!(bind! &loss_fn (torch.import_module CrossEntropyLoss torch.nn))

; Creating a trainer
; Params: class to import, pymodule with a class to import, Dataloader, Pytorch model, loss function optimizer
!(bind! &trainer (torch.import_module Trainer tm_test &train_dataloader &model &loss_fn &optimizer))

; Params: instance of the Trainer class, number of epochs
!(torch.run_trainer &trainer 3)


