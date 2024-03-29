!(import! &self torchme)

;-------------------------------------
; Tensor creation
;-------------------------------------
!(torch:tensor ())
!(torch:tensor ((0. 1.) (1. 0.) (2. 0.) (2. 2.)))
!(bind! &th (torch:tensor (3. 2. 3.)))


; bind a bunch of tensor objects to tokens for convenience
!(bind! &x (torch:tensor (3 2 3)))
!(bind! &m (torch:tensor ((6 1) (7 2) (8 3))))
!(torch:arange (0 100))
!(torch:conj (torch:tensor (-1+1j -2+2j 3-3j)))
!(torch:zeros_like (&x))
!(bind! &a (torch:tensor ((1 1 1) (0 0 0))))
!(bind! &b (torch:tensor ((-1 -1 -1) (1 1 1))))

; torch.tensor(t) is equivalent to t.clone().detach()
!(torch:tensor (&m))

!(torch:empty (1 2 3))
!(torch:zeros (2 3))
!(torch:linspace (50 75 20))


!(torch:manual_seed 1730)
!(bind! &q (torch:rand (3 4)))
!(bind! &w (torch:rand (4 5)))

!(torch:std_mean (torch:rand (3 3)))

!(torch:topk (&q 3))

!(torch:mean (&w))
!(torch:mean (&w 1 True))
!(torch:aminmax (&q))
;-------------------------------------
; Arithmetic and linear algebra operations
;-------------------------------------
!(torch:add ((torch:zeros (2 2)) 5))
!(torch:mul ((torch:ones (2 2)) -3))
!(torch:matmul (&q &w))


;-------------------------------------
; Miscellaneous
;-------------------------------------
!(torch:result_type ( (torch:rand (4 4)) (torch:rand (4 4)) ))
!(bind! &p (torch:rand (4 4)))
!(torch:unbind (&p))
!(torch:hsplit (&p 2))
!(bind! &k (torch:tensor ((0. 1.) (1. 0.) (2. 0.) (2. 2.))))
!(torch:histogramdd (&k (3 3)) )
!(bind! &c (torch:tensor (1 2 3)))
!(torch:combinations (&c))


;-------------------------------------
; autograd handling
;-------------------------------------
!(torch:requires_grad_status &q)
!(torch:requires_grad_status &w)

!(torch:requires_grad_ &q True)
!(torch:requires_grad_ &w True)

;!(torch:requires_grad_status &q)
;!(torch:requires_grad_status &w)

!(bind! &tm (torch:matmul (&q &w)))
!(torch:requires_grad_status &tm)

!(bind! &mtm (torch:mean (&tm)))
!(torch:requires_grad_status &mtm)
!(torch:backward &mtm)


;-------------------------------------
; Instantiate a pytorch (or python) module
;-------------------------------------
!(bind! &foo_model (torch:instantiate_module FooModel tm_test))
!(torch:to_device &foo_model cuda)
!(torch:to_device &tm cuda)


;-------------------------------------
; kwargs handling
; !!! NOTE for the current implementation:
; if you want to pass kwargs you should pass positional args also as kwargs
; In the example below 'start' and 'end' are positional arguments
;-------------------------------------
!(bind! &kwargs_arange (kwargs ((start 0) (end 100) (dtype torch.float) (requires_grad True))) )
!(torch:arange &kwargs_arange)

!(bind! &kwargs_foo (kwargs ((param1 1) (param2 None) (param3 foo))) )
!(torch:instantiate_module Foo tm_test &kwargs_foo)


;-------------------------------------
; Training a Pytorch model
;-------------------------------------

; Define a transform for inputs
!(bind! &to_tensor_transform (torch:instantiate_module ToTensor torchvision.transforms))

; Download training and test data and create datasets
!(bind! &train_data (torch:instantiate_module FashionMNIST torchvision.datasets data True &to_tensor_transform None True))
!(bind! &test_data (torch:instantiate_module FashionMNIST torchvision.datasets data False &to_tensor_transform None True))

; Create data loaders
!(bind! &train_dataloader (torch:instantiate_module DataLoader torch.utils.data &train_data 64))
!(bind! &test_dataloader (torch:instantiate_module DataLoader torch.utils.data &test_data 64))


; Import a Pytorch model
!(bind! &model (torch:instantiate_module NeuralNetwork tm_test))
!(torch:to_device &model cuda)
!(bind! &model_params (torch:get_model_params &model))

; Creating an optimizer
!(bind! &optimizer (torch:instantiate_module SGD torch.optim &model_params 0.001))

; Creating a loss function
!(bind! &loss_fn (torch:instantiate_module CrossEntropyLoss torch.nn))

; Creating a trainer
; Params: class to import, pymodule with a class to import, Dataloader, Pytorch model, loss function optimizer
!(bind! &trainer (torch:instantiate_module Trainer tm_test &train_dataloader &model &loss_fn &optimizer))

; Run train cycle
; Params: instance of the Trainer class, number of epochs
;!(torch:run_trainer &trainer 3)

