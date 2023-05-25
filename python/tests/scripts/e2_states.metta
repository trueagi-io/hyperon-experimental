!(bind! &state-token (new-state (A B)))

; &state-token is replace by the state atom in the code below its creation
; we wrap it into a function to show that it's not the token that changes, but
; the content of the state atom
(= (get-token) &state-token)

; Expected content of the state atom
!(assertEqual
   (get-state (get-token))
   (A B))

; Interestingly, it will not be shown as (State (A B)), because @@@@@
; ! (get-token)

; States can be equal even if they are wrapped into different state atoms
!(assertEqual
   (get-token)
   (new-state (A B)))

; State atoms are of StateMonad type
; These are inferred types based on new-state and change-state! signatures
!(assertEqual
  (get-type (new-state 2))
  (StateMonad Number))
!(assertEqual
  (get-type (change-state! (new-state "S") "V"))
  (StateMonad String))

; These are the types of State grounded atom
!(assertEqual
  (let $v (new-state 1) (get-type $v))
  (StateMonad Number))
; atm, meta-types for states of non-grounded types are used
!(assertEqual
  (get-type &state-token)
  (StateMonad Expression))

!(assertEqual
  (change-state! &state-token 1)
  (Error 1 BadType))
!(assertEqual
  (change-state! (new-state 1) "S")
  (Error "S" BadType))

; Changing the content of the state atom
!(nop (change-state! &state-token (C D)))

; The same state atom has different content now
!(assertEqual
   (get-state (get-token))
   (C D))


!(assertEqual
  (let $x (new-state 1)
     (change-state! $x (+ (get-state $x) 1)))
  (new-state 2))

; Chcking that there is no conflict between variable names in let-expression
; and type definition of new-state
!(assertEqual
   (let $tnso (new-state 1) $tnso)
   (new-state 1))

; FIXME: doesn't work as for 25 May 2023
; !(get-state (let $x (new-state 1) $x))

