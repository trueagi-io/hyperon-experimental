; A classical example of dependent types

(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

(: Vec (-> $t Nat Type))
(: Cons (-> $t (Vec $t $x) (Vec $t (S $x))))
(: Nil (Vec $t Z))

!(assertEqual
  (get-type (Cons 0 (Cons 1 Nil)))
  (Vec Number (S (S Z))))

(: drop (-> (Vec $t (S $x)) (Vec $t $x)))
(= (drop (Cons $x $xs)) $xs)

!(assertEqual
  (get-type (drop (Cons 1 Nil)))
  (Vec Number Z))
!(assertEqual
  (drop (Cons 2 (Cons 1 Nil)))
  (Cons 1 Nil))
; Wrong type
!(assertEqualToResult
  (get-type (drop Nil))
  ())


; The result returned by `get-type` is reduced, thus,
; grounded operations in type declarations work (kind of)
; NOTE: tests below is the current behavior that may be changed
(: VecN (-> $t Number Type))
(: ConsN (-> $t (VecN $t $x) (VecN $t (+ $x 1))))
(: NilN (VecN $t 0))

!(assertEqual
  (get-type (ConsN "1" (ConsN "2" NilN)))
  (VecN String 2))

(: dropN (-> (VecN $t $x) (VecN $t (- $x 1))))
(= (dropN (ConsN $x $xs)) $xs)

!(assertEqual
  (get-type (dropN (ConsN "1" NilN)))
  (VecN String 0))
; ...but without constraints (and may require additional application)
; TODO: doesn't work after type checking fixes
(assertEqual
  ((get-type (dropN (dropN (ConsN "1" NilN)))))
   (VecN String -1))
; This is not badly typed, but will just be not reduced
!(assertEqualToResult
   (dropN NilN)
  ((dropN NilN)))
