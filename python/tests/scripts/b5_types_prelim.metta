; Symbols are typed. If no type definition is provided,
; the type is %Undefined%, which can match against any
; other type in a gradual typing fashion.
; Automatic type checking is off by default ATM, so
; incorrectly typed expressions can be added to Atomspace.
; However, the interpreter checks types in runtime,
; and refuses to reduce expressions of wrong types.
(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

; `(Add S Z)` will be reduced to `S` without type definitions
!(assertEqual
  (collapse (Add S Z))
  (S))

; Adding type definitions. Arrow is a built-in symbol of the type system
; `Nat` is a custom symbol
(: Z Nat)
(: S (-> Nat Nat))
(: Add (-> Nat Nat Nat))
; Now the result of its evaluation will be empty, because `S` is not `Nat`
!(assertEqual
  (collapse (Add S Z))
  ())
; This will still work, because `Something` is of undefined type,
; and it might be `Nat`
!(assertEqual
  (collapse (Add Something Z))
  (Something))
; This should work normally
!(assertEqual
  (collapse (Add Z (S Z)))
  ((S Z)))

; Types can be nondeterministic
; The following will be accepted by the interpreter
(: Ten Nat)
(: Ten Int)
!(assertEqual
  (collapse (Add Z Ten))
  ((Add Z Ten)))

; Types can be parameterized
(: List (-> Type Type))
(: Nil (List $t))
(: Cons (-> $t (List $t) (List $t)))
; This list is well typed and evaluated to itself
!(assertEqual
  (collapse (Cons (S Z) (Cons Z Nil)))
  ((Cons (S Z) (Cons Z Nil))))
; This list is badly typed
!(assertEqual
  (collapse (Cons S (Cons Z Nil)))
  ())

; Another example
(: T Type)
(: eq (-> $t $t Type))
(= (eq $x $x) T)
; Accepted and reduced
!(assertEqual
  (collapse (eq (Add Z Z) Z))
  (T))
; Accepted, not reduced
!(assertEqual
  (collapse (eq Z (S Z)))
  ((eq Z (S Z))))
; Not accepted
!(assertEqual
  (collapse (eq Z S))
  ())

; Type (symbols) are not prohibited from being elements
; of other custom types
(: Color Property)
(: Green Color)
(: Red Color)
(: Shape Property)
(: Square Shape)
(: Circle Shape)
(: of-same-type (-> $t $t Type))
(= (of-same-type $x $y) T)
!(assertEqual
  (collapse
    (of-same-type Color Shape))
  (T))
!(assertEqual
  (collapse
    (of-same-type Green Red))
  (T))
!(assertEqual
  (collapse
    (of-same-type Green Color))
  ())
!(assertEqual
  (collapse
    (of-same-type Green Circle))
  ())
