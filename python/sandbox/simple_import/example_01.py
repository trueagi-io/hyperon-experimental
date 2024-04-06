def print_args(*args, **kwargs):
    print("arguments:")
    for a in args:
        print(a, type(a))
    print("keyword arguments:")
    for k,v in kwargs.items():
        print(k, v, type(v))

def simple_fun(*args, **kwargs):
    print("Call simple function")
    print_args(*args, **kwargs)
    print("")
    return 0

class SimpleObject:
    def method(self, *args, **kwargs):
        print("Call Method of simple Object")
        print_args(*args, **kwargs)
        print("")
        return "0"
    def __str__(self):
        return "simple_object"
