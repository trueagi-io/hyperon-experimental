import importlib
from hyperon.atoms import *
from kwargsme import *


def instantiate_module(*args):
    torch_module_name = args[0].get_name()
    pymodule_name = args[1].get_name()
    pymodule = importlib.import_module(pymodule_name)
    module_class = getattr(pymodule, torch_module_name)

    if len(args) > 2:
        a = []
        kw = []
        for arg in args[2:]:
            if isinstance(arg, GroundedAtom):
                if isinstance(arg.get_object(), GroundedObject):
                    obj_cont = arg.get_object().content
                    if isinstance(obj_cont, Kwargs):
                        kw = obj_cont.content
                    else:
                        a.append(arg.get_object().content)
                else:
                    a.append(arg.get_object().value)
            elif isinstance(arg, SymbolAtom):
                if arg.get_name() == 'None':
                    a.append(None)
                else:
                    a.append(arg.get_name())
        if len(kw) > 0:
            module_instance = module_class(**kw)
        else:
            module_instance = module_class(*a)
    else:
        module_instance = module_class()

    return [G(GroundedObject(module_instance))]


def to_device(*args):
    torch_object = None
    device = None
    if isinstance(args[0], GroundedAtom):
        if isinstance(args[0].get_object(), GroundedObject):
            torch_object = args[0].get_object().content
        else:
            torch_object = args[0].get_object().value

    if isinstance(args[1], SymbolAtom):
        device = args[1].get_name()

    torch_object.to(device=device)

    return [G(GroundedObject(torch_object))]


def run_trainer(*args):
    trainer = args[0]
    nepochs = args[1]
    for t in range(nepochs):
        print(f"Epoch {t + 1}\n-------------------------------")
        trainer.train()
        trainer.test()
    return



parsing_exceptions = {
        'torch:requires_grad_status': (lambda x: x.requires_grad, True),
        'torch:instantiate_module': (instantiate_module, False),
        'torch:to_device': (to_device, False),
        'torch:get_model_params': (lambda x: x.parameters(), True),
        'torch:run_trainer': (run_trainer, True)
    }