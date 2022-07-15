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
  (Add S Z)
  S)

; Adding type definitions. Arrow is a built-in symbol of the type system
; `Nat` is a custom symbol
(: Z Nat)
(: S (-> Nat Nat))
(: Add (-> Nat Nat Nat))
; Now the result of its evaluation will be empty, because `S` is not `Nat`
!(assertEqualToResult
  (Add S Z)
  ())
; This will still work, because `Something` is of undefined type,
; and it might be `Nat`
!(assertEqual
  (Add Something Z)
  Something)
; This should work normally
!(assertEqual
  (Add Z (S Z))
  (S Z))

; Types can be nondeterministic
; The following will be accepted by the interpreter
(: Ten Nat)
(: Ten Int)
!(assertEqualToResult
   (Add Z Ten)
  ((Add Z Ten)))

; Types can be parameterized
(: List (-> Type Type))
(: Nil (List $t))
(: Cons (-> $t (List $t) (List $t)))
; This list is well typed and evaluated to itself
!(assertEqualToResult
   (Cons (S Z) (Cons Z Nil))
  ((Cons (S Z) (Cons Z Nil))))
; This list is badly typed
!(assertEqualToResult
  (Cons S (Cons Z Nil))
  ())

; Another example
(: T Type)
(: eq (-> $t $t Type))
(= (eq $x $x) T)
; Accepted and reduced
!(assertEqual
  (eq (Add Z Z) Z)
  T)
; Accepted, not reduced
!(assertEqualToResult
   (eq Z (S Z))
  ((eq Z (S Z))))
; Not accepted
!(assertEqualToResult
  (eq Z S)
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
  (of-same-type Color Shape)
  T)
!(assertEqual
  (of-same-type Green Red)
  T)
!(assertEqualToResult
  (of-same-type Green Color)
  ())
!(assertEqualToResult
  (of-same-type Green Circle)
  ())

; There are some meta-types, which may influence
; the process of interpretation. One of them is `Atom`.
; If a function expects `Atom` as its argument,
; this argument is not reduced before passing to
; the function, so the function can process the
; original expression itself.

; `match` expects the pattern of `Atom` type.
; In the following example, `(Green $who)` is evaluated
; to `F`. However, it is not reduced as `match` argument,
; and the query returns `Sam`.
(Green Sam)
(= (Green Sam) T)
!(assertEqual
  (Green $who)
  T)
!(assertEqual
  (match &self (Green $who) $who)
  Sam)
; It should be noted that the result of `match` will be
; evaluated further. In the following, `match` first
; returns `(Green Sam)`, which is then evaluated to `T`.
!(assertEqual
  (match &self (Green $who) (Green $who))
  T)

; If one wants to evaluate an expression before passing it
; to `match`, `let` can be used:
!(assertEqual
  (let $r (Add (S Z) Z)
    (match &self (= (eq $r (S Z)) $tv) $tv))
  T)
; It will not work inside `match`:
!(assertEqualToResult
  (match &self (= (eq (Add (S Z) Z) (S Z)) $tv) $tv)
  ())
; The second argument of `let` is not `Atom` and evaluated
; by the interpreter before executing `let`, while
; the third argument is `Atom`, which is transformed by `let`
; as an expression and then returned for further evaluation
; `match` and `let` are implemented as an ordinary grounded operations

; One can use `let*` for convenience, when multiple consequent
; substitutions are needed
!(assertEqual
  (let* (($r1 (Add Z Z))
         ($r2 (Add $r1 (S Z)))
         ($r3 (Add $r2 (S Z))))
        (match &self (= (eq (S (S Z)) $r3) $tv) $tv))
  T)

; Atom type can be used in custom functions
; `eqa` will compare expressions:
(: eqa (-> Atom Atom Type))
(= (eqa $x $x) T)
; These expressions are identical:
!(assertEqual
   (eqa (Add Z Z) (Add Z Z))
   T)
; While this will not be reduced
; (`eqa` is not total)
!(assertEqualToResult
    (eqa Z (Add Z Z))
   ((eqa Z (Add Z Z))))
; In contrast to this:
!(assertEqual
   (eq Z (Add Z Z))
   T)
; Or this:
!(assertEqual
   (let $z (Add Z Z)
     (eqa Z $z))
   T)

