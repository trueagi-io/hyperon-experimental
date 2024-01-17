import torch
from torch import nn


class FooModel(torch.nn.Module):

    def __init__(self):
        super(FooModel, self).__init__()
        self.linear = torch.nn.Linear(2, 2)
        self.softmax = torch.nn.Softmax(dim=1)

    def forward(self, x):
        x = self.linear(x)
        x = self.softmax(x)
        return x


# Test model for MNIST
class NeuralNetwork(nn.Module):
    def __init__(self):
        super().__init__()
        self.flatten = nn.Flatten()
        self.linear_relu_stack = nn.Sequential(
            nn.Linear(28*28, 512),
            nn.ReLU(),
            nn.Linear(512, 512),
            nn.ReLU(),
            nn.Linear(512, 10)
        )

    def forward(self, x):
        x = self.flatten(x)
        logits = self.linear_relu_stack(x)
        return logits


class Trainer():
    def __init__(self, dataloader, model, loss_fn, optimizer):
        self.dataloader = dataloader
        self.model = model
        self.loss_fn = loss_fn
        self.optimizer = optimizer

    def train(self):
        device = 'cuda' if torch.cuda.is_available() else 'cpu'
        size = len(self.dataloader.dataset)
        self.model.train()

        for batch, (X, y) in enumerate(self.dataloader):
            X, y = X.to(device), y.to(device)

            # Compute prediction error
            pred = self.model(X)
            loss = self.loss_fn(pred, y)

            # Backpropagation
            loss.backward()
            self.optimizer.step()
            self.optimizer.zero_grad()

            if batch % 100 == 0:
                loss, current = loss.item(), (batch + 1) * len(X)
                print(f"loss: {loss:>7f}  [{current:>5d}/{size:>5d}]")

    def test(self):
        device = 'cuda' if torch.cuda.is_available() else 'cpu'
        size = len(self.dataloader.dataset)
        num_batches = len(self.dataloader)
        self.model.eval()
        test_loss, correct = 0, 0
        with torch.no_grad():
            for X, y in self.dataloader:
                X, y = X.to(device), y.to(device)
                pred = self.model(X)
                test_loss += self.loss_fn(pred, y).item()
                correct += (pred.argmax(1) == y).type(torch.float).sum().item()
        test_loss /= num_batches
        correct /= size
        print(f"Test: \n Accuracy: {(100 * correct):>0.1f}%, Avg loss: {test_loss:>8f} \n")


class Foo():
    def __init__(self, param1=None, param2=None, param3=None):
        self.param1 = param1
        self.param2 = param2
        self.param3 = param3

