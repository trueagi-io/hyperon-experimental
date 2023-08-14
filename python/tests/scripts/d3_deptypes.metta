;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A classical example of dependent types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple type definitions:
; Defines the natural numbers `Nat` for counting
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

; `Vec` takes a `$t` type and a `Nat` length.
(: Vec (-> $t Nat Type))
; `Cons` and `Nil` construct `Vec` values, with `Cons` incrementing the length by 1.
(: Cons (-> $t (Vec $t $x) (Vec $t (S $x))))
(: Nil (Vec $t Z))

; We can query the derived type
!(assertEqual
  (get-type (Cons 0 (Cons 1 Nil)))
  (Vec Number (S (S Z))))

; `drop` removes the head from a `Vec`, returning a `Vec` with length decrease by 1
(: drop (-> (Vec $t (S $x)) (Vec $t $x)))
(= (drop (Cons $x $xs)) $xs)

; Query type
!(assertEqual
  (get-type (drop (Cons 1 Nil)))
  (Vec Number Z))
; Check length after dropping
!(assertEqual
  (drop (Cons 2 (Cons 1 Nil)))
  (Cons 1 Nil))

; Types can protect against misuse, like dropping from empty Vec
!(assertEqualToResult
  (get-type (drop Nil))
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The result returned by `get-type` is reduced, thus,
; grounded operations in type declarations work (kind of)
; NOTE: tests below is the current behavior, which may be changed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `VecN` and `ConsN` use grounded `+` to increment the length.
(: VecN (-> $t Number Type))
(: ConsN (-> $t (VecN $t $x) (VecN $t (+ $x 1))))
(: NilN (VecN $t 0))

; When querying types, grounded ops are reduced
!(assertEqual
  (get-type (ConsN "1" (ConsN "2" NilN)))
  (VecN String 2))

; Similarly, `-` reduces the length in `dropN` type.
(: dropN (-> (VecN $t $x) (VecN $t (- $x 1))))
(= (dropN (ConsN $x $xs)) $xs)

!(assertEqual
  (get-type (dropN (ConsN "1" NilN)))
  (VecN String 0))

; TODO: doesn't work after type checking fixes
; However, without any validation, invalid lengths like -1 can be derived.
(assertEqual
  ((get-type (dropN (dropN (ConsN "1" NilN)))))
   (VecN String -1))

; Applying `dropN` to `NilN` is not badly typed, but it
; doesn't reduce, since there are no definitions for this case.
!(assertEqualToResult
   (dropN NilN)
  ((dropN NilN)))
