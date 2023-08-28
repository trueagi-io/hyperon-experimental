import unittest

import torch
from hyperon import *

class DummyModel(torch.nn.Module):

    def __init__(self):
        super(DummyModel, self).__init__()
        self.linear = torch.nn.Linear(2, 2)
        self.softmax = torch.nn.Softmax(dim=1)

    def forward(self, x):
        x = self.linear(x)
        x = self.softmax(x)
        return x

def get_labels(inputs):
    inputs = inputs.clone()
    inputs -= inputs.min(1, keepdim=True)[0]
    inputs /= inputs.max(1, keepdim=True)[0]
    return inputs

def do_step_loss(optimizer, loss):
    loss.backward()
    optimizer.step()
    return loss.item()

class TorchDiffTest(unittest.TestCase):

    def test_torch_diff(self):
        '''
        This test is not intended as an example of integration of metta and torch.
        It's purpose is to make sure that gradient information is not lost, when
        the results are wrapped into atoms and passed to grounded functions.
        '''
        metta = MeTTa()
        model = DummyModel()
        optimizer = torch.optim.RMSprop(model.parameters(), lr=0.1)
        loss_fn = torch.nn.CrossEntropyLoss()
        data = torch.rand(10, 2, generator = torch.manual_seed(1))
        if torch.cuda.is_available():
            model = model.to("cuda")
            data = data.to("cuda")
        metta.register_atom("classify", OperationAtom("classify", lambda x: model(x)))
        metta.register_atom("&inputs", ValueAtom(data))
        metta.register_atom("get-labels", OperationAtom("get-labels", get_labels))
        metta.register_atom("loss-fn", OperationAtom("loss-fn", lambda outputs, labels: loss_fn(outputs, labels)))
        metta.register_atom("do-step-loss", OperationAtom("do-step-loss",
            lambda loss: do_step_loss(optimizer, loss)))
        losses = []
        for i in range(10):
            optimizer.zero_grad()
            losses += [metta.run('''
                ! (do-step-loss (loss-fn (classify &inputs) (get-labels &inputs)))
            ''')[0][0].get_object().value]
        self.assertTrue(losses[0] > losses[-1] + 0.01)


if __name__ == "__main__":
    unittest.main()
