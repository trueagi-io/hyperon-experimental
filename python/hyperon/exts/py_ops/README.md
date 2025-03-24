# When this library is needed

Import this library in your MeTTa script, when you want
such operations as `+` to work on non-core types, e.g.
```
! (import! &self py_ops)
! (bind! abs (py-atom numpy.absolute))
! (+ (abs -5) 10)
```
