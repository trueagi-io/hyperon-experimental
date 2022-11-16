; This example rewritten with `case` works (as per 19 Oct 22),
; but it might not be good as a unit test:
; - what's going on is not obvious
; - there might be a better way to implement what the code is doing
; - processing commands with side effect and empty results and
;   chaining them might not be idiomatic
; - it's relatively slow (why? FIXME?)

!(bind! &memory (new-space))

; catching empty results
(: do (-> Atom %Undefined%))
(= (do $expr)
   (case $expr
     (($x $x)
      (%void% None))))

; `remove-state` will return an empty result if there is no
; corresponding state in the memory
(= (remove-state $var)
   (match &memory (state $var $y)
         (call:remove_atom &memory (state $var $y))))

; using `do` assures that the next clause will be executed
; even if the result of the previous clause is empty
(= (change-state $var $value)
   (nop (do (remove-state $var))
        (do (add-atom &memory (state $var $value)))))

(= (get-state $var)
   (match &memory (state $var $value) $value))

; Earlier, `(get-state (person name))` got cached, and it changes
; in one equality for `listen` didn't affect other equalities.
; `case` accepts an `Atom` and forcefully evaluates it.
; However, the idea of using equalities as sequentially processed
; rules with side effects is still questionable
(= (listen (My name is $x))
   (case (get-state (person name))
       (($x (Yes, I remember))
        ; $y cannot be %void%
        ($y (I thought you are $y)))
   )
)

(= (listen (My name is $x))
   (change-state (person name) $x))

(= (listen $_)
   (case (get-state greeted)
      ((%void%
          (case (get-state (person name))
                ((%void% (Hi. What is your name?))
                 ($p-name (Nice to meet you $p-name)))))
      )
    )
)

(= (listen $_)
   (nop (change-state greeted True)))

;!(listen (Hello))
;!(listen (My name is Alexey))
;!(listen (My name is Alexey))
;!(listen (My name is Ben))

;!(call:get_atoms &memory)

