;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State atoms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `new-state` creates a new state atom wrapping the initial value (A B).
; Then it binds this state atom to the token `&state-token`,
; which is replaced by the corresponding atom at parse-time.
!(bind! &state-token (new-state (A B)))

; &state-token is replaced by the state atom in the code below its creation
; we wrap it into a function to show that it's not the token that changes, but
; the content of the state atom
(= (get-token) &state-token)

; `new-state $x` creates a `(State $x)` structure,
; and `get-token` will show its content
!(assertEqual
   (get-state (get-token))
   (A B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; States can be equal even if they are wrapped into different state atoms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
   (get-token)
   (new-state (A B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State atoms are of StateMonad type
; These are inferred types based on new-state and change-state! signatures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State atoms have a defined type based on initial state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; &state-token was initialized with state of type (A B), so
; it cannot be changed to int
!(assertEqual
  (change-state! &state-token 1)
  (Error 1 BadType))

; the new state here is int, so it cannot be changed to string
!(assertEqual
  (change-state! (new-state 1) "S")
  (Error "S" BadType))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changing the content of the state atom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (nop ...) provides a way to obtain side-effects of a statement
; while ignoring unwanted return values.
!(nop (change-state! &state-token (C D)))

; The same state atom has different content now
!(assertEqual
   (get-state (get-token))
   (C D))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MeTTa provides proper encapsulated state manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scoping doesn't conflict between let bound variables and state atom types.
!(assertEqual
  (let $x (new-state 1)
     (change-state! $x (+ (get-state $x) 1)))
  (new-state 2))

; Checking that there is no conflict between variable names in
; let-expression and type definition of new-state
!(assertEqual
   (let $tnso (new-state 1) $tnso)
   (new-state 1))

; FIXME: doesn't work as for 25 May 2023
; !(get-state (let $x (new-state 1) $x))
