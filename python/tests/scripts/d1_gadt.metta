;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic types of grounded symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `get-type` returns the type of its argument
!(assertEqual
  (get-type 5)
  Number)
!(assertEqual
  (get-type (+ 5 7))
  Number)

; It works for built-in symbols too
!(assertEqual
  (get-type +)
  (-> Number Number Number))

; If its argument is a badly typed expression,
; `get-type` returns the empty set
!(assertEqualToResult
  (get-type (+ 5 "4"))
  ())
!(assertEqualToResult
  (get-type (+ -))
  ())

; Also works for custom types
(: Either Type)
!(assertEqual
  (get-type Either)
  Type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Arrow types of functions and constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: Left (-> %Undefined% Either))
(: Right (-> %Undefined% Either))

!(assertEqual
  (get-type (Left 5))
  Either)

(: isLeft (-> Either Bool))
(= (isLeft (Left $x)) True)
(= (isLeft (Right $x)) False)

!(assertEqual
  (get-type (isLeft (Right 5)))
  Bool)

; Badly typed expression
!(assertEqualToResult
  (get-type (isLeft 5))
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parameterized types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: EitherP (-> $t Type))
(: LeftP (-> $t (EitherP $t)))
(: RightP (-> $t (EitherP $t)))

!(assertEqual
  (get-type (LeftP 5))
  (EitherP Number))

(: Pair (-> $a $b Type))
(: pair (-> $a $b (Pair $a $b)))

!(assertEqual
  (get-type (pair (LeftP 5) "String"))
  (Pair (EitherP Number) String))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recursively parameterized types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

!(assertEqual
  (get-type (Cons 5 (Cons 6 Nil)))
  (List Number))
!(assertEqualToResult
  (get-type (Cons 5 (Cons "6" Nil)))
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `let` can be convenient to extract information from `get-type` results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (let (List $t) (get-type (Cons 5 (Cons 6 Nil)))
       $t)
  Number)
