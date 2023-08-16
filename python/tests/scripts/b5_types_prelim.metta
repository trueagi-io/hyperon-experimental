;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Symbols are typed. If no type definition is provided,
; the type is %Undefined%, which can match against any
; other type in a gradual typing fashion.
; Automatic type checking is off by default ATM, so
; incorrectly typed expressions can be added to Atomspace.
; However, the interpreter checks types in runtime, and
; evaluation of badly typed expressions leads to a error expression.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

; `(Add S Z)` will be reduced to `S` without type definitions
!(assertEqual
  (Add S Z)
  S)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Adding type definitions.
; Arrow (->) is a built-in symbol of the type system
; `Nat` is a custom symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: Z Nat)
(: S (-> Nat Nat))
(: Add (-> Nat Nat Nat))
; Now the result of its evaluation will be a error expression, because `S` is not `Nat`
!(assertEqualToResult
  (Add S Z)
  ((Error S BadType)))
; But these other examples works normally
!(assertEqual
  (Add (S Z) Z)
  (S Z))
!(assertEqual
  (Add Z (S Z))
  (S Z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Adding to an untyped symbol (Something) will still work,
; because it might be `Nat`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (Add Something Z)
  Something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Types can be nondeterministic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: Ten Nat)
(: Ten Int)
; The following will be accepted by the interpreter
!(assertEqualToResult
   (Add Z Ten)
  ((Add Z Ten)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Types can be parameterized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; List has a function type. Its argument should be of Type type.
; The type of the whole expression, e.g. (List String), will also have Type type.
(: List (-> Type Type))

; Nil's type is parametrized as the type returned by List when it
; takes parameter $t (which, per the definition above,
; is the same type as $t)
(: Nil (List $t))

; Cons is defined as a parametrized List constructor
; Based on the definition of List above, Cons' arguments
; are equally typed, and it returns the same type
; as its arguments.
(: Cons (-> $t (List $t) (List $t)))

; This list is well typed and evaluated to itself
!(assertEqualToResult
   (Cons (S Z) (Cons Z Nil))
  ((Cons (S Z) (Cons Z Nil))))

; This list is badly typed, because S and Z are not the same type
!(assertEqualToResult
  (Cons S (Cons Z Nil))
  ((Error Z BadType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Another example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: T Type)
; The type definition of `eq` specifies both its arguments
; must be of the same type, but its own type is left undefined,
; just as a Type.
(: eq (-> $t $t Type))
; The definition of `eq` specifies its actual type (T)
(= (eq $x $x) T)
; Accepted and reduced
!(assertEqual
  (eq (Add Z Z) Z)
  T)
; Accepted, not reduced because no definition is known
; for `eq` when its arguments are not the same
!(assertEqualToResult
   (eq Z (S Z))
  ((eq Z (S Z))))
; Not accepted, because Z and S are not the same type
!(assertEqualToResult
  (eq Z S)
  ((Error S BadType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Type (symbols) are not prohibited from being elements
; of other custom types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: Color Property)
(: Green Color)
(: Red Color)
(: Shape Property)
(: Square Shape)
(: Circle Shape)
; This type definition is the same as that of `eq` above
(: of-same-type (-> $t $t Type))
; It returns True for any pair of arguments, as long as there
; is not Type error

; Accepted
(= (of-same-type $x $y) T)
!(assertEqual
  (of-same-type Color Shape)
  T)
!(assertEqual
  (of-same-type Green Red)
  T)

; Not accepted
!(assertEqualToResult
  (of-same-type Green Color)
  ((Error Color BadType)))
!(assertEqualToResult
  (of-same-type Green Circle)
  ((Error Circle BadType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; There are some meta-types, which may influence
; the process of interpretation. One of them is `Atom`.
; If a function expects `Atom` as its argument,
; this argument is not reduced before passing to
; the function, so the function can process the
; original expression itself.
; The `match` function expects an `Atom` type as argument.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In the following example, `(Green $who)` is evaluated
; to `T`. However, it is not reduced as `match` argument,
; and the query returns `Sam`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(Green Sam)
(= (Green Sam) T)
!(assertEqual
  (Green $who)
  T)
!(assertEqual
  (match &self (Green $who) $who)
  Sam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; It should be noted that the result of `match` will be
; evaluated further. In the following, `match` first
; returns `(Green Sam)`, which is then evaluated to `T`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (match &self (Green $who) (Green $who))
  T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If one wants to evaluate an expression before passing it
; to `match`, `let` can be used:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (let $r (Add (S Z) Z)
    (match &self (= (eq $r (S Z)) $tv) $tv))
  T)
; but it will not work inside `match`:
!(assertEqualToResult
  (match &self (= (eq (Add (S Z) Z) (S Z)) $tv) $tv)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The second argument of `let` is not `Atom` and evaluated
; by the interpreter before executing `let`, while
; the third argument is `Atom`, which is transformed by `let`
; as an expression, and then returned for further evaluation.
; `match` and `let` are implemented as an ordinary grounded operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In fact, one can use patterns instead of variables in `let`
; in order to deconstruct the result immediately.
; This example binds the result of (Add (S Z) (S Z)) to
; the whole pattern (S (S $r)), and extracts the inner variable $r
; This is cleaner than binding and re-matching.
!(assertEqual
  (let (S (S $r)) (Add (S Z) (S Z)) $r)
  Z)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; One can use `let*` for convenience, when multiple consequent
; substitutions are needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (let* (($r1 (Add Z Z))
         ($r2 (Add $r1 (S Z)))
         ($r3 (Add $r2 (S Z))))
        (match &self (= (eq (S (S Z)) $r3) $tv) $tv))
  T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Atom type can be required in custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `eqa` will compare expressions:
(: eqa (-> Atom Atom Type))
(= (eqa $x $x) T)
; These expressions are identical:
!(assertEqual
   (eqa (Add Z Z) (Add Z Z))
   T)
; while this will not be reduced because the expressions
; are not the same (even if their evaluation would be)
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

