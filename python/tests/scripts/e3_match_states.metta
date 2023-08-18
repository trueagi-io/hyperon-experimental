;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This function creates a symbolic expression, which wraps a novel state,
; and puts it into &self.
; This is a way to address particular state atoms without creating
; unique tokens for them (it can be used for any grounded atom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (new-goal-status! $goal $status)
    (let $new-state (new-state $status)
         (add-atom &self (= (status (Goal $goal)) $new-state))
    )
)

! (new-goal-status! lunch-order inactive)
! (new-goal-status! meditation inactive)

; We can use `status` as a function to access corresponding state
!(assertEqual
  (get-state (status (Goal lunch-order)))
  inactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `status` returns a `State`, which can be altered
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `nop` is used only to get () for passing unit tests
!(nop (change-state! (status (Goal lunch-order)) active))

; The result is now different. This might be surprising, since
; the content of Space is not changed. However, this is the whole
; intention behind states: the atom remains the same, and we don't
; need to reinsert it into Space, but the state it wraps changes
; (even if it is inside equality).
!(assertEqual
  (get-state (status (Goal lunch-order)))
  active)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Introducing tokens for states is not recommended, because these states are mutable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(bind! &state-active (new-state active))
!(nop (change-state! &state-active inactive))
; We cannot put `(new-state active)` inside `match`, because it doesn't
; reduce the input pattern expression. Instead, we should use
; `(let $state-active (new-state active)) ...) here.

; In any case, direct matching against expressions with states work:
!(assertEqual
    (match &self (= (status (Goal $goal)) &state-active) $goal)
    meditation)

; But we can also find all goal statuses, unwrap them from State,
; and compare to the desirable result
!(assertEqual
  (if (== (get-state (status (Goal $goal)))
          active)
      $goal
      (superpose ()))
  lunch-order)
