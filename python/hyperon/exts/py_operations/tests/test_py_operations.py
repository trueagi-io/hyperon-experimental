
from hyperon import *
metta = MeTTa()
#metta.run('!(import! &self py_operations)')
#t = metta.run("!(+ 1 2)")
plus = metta.parse_single('+')
print(type(plus.get_object())) # OperationObject
print(plus.get_object().op) # some lambda
print(plus.get_object()) # + as a representation of this operation
calc = metta.run('! (+ 1 2)')[0][0]
print(type(calc.get_object())) # ValueObject
print(calc.get_object().value) # 3