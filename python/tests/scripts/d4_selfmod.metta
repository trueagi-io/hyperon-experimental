;TODO not a good way to create an Atomspace
!(import! &memory "memory.metta")

(= (remove-state $var)
   (match &memory (state $var $y)
         (call:remove_atom &memory (state $var $y))))

(= (change-state $var $value)
   (, (remove-state $var)
      (call:add_atom &memory (state $var $value))))

(= (get-state $var)
   (match &memory (state $var $value) $value))

(= (answer $x)
   (println! $x))

(: if (-> Bool Atom Atom $t))
(= (if True $x $y) $x)
(= (if False $x $y) $y)

!(call:add_atom &memory (state greeted False))
!(call:add_atom &memory (state (person name) "None"))
;!(change-state greeted False)
;!(change-state (person name) "None")

(= (listen (My name is $x))
   (if (== (get-state (person name)) "None")
       (nop)
       (if (== (get-state (person name)) $x)
           (Yes, I remember)
           (I thought you are (get-state (person name))))))

(= (listen (My name is $x))
   (nop (change-state (person name) $x)))

(= (listen $_)
   (if (not (get-state greeted))
       (if (== (get-state (person name)) "None")
           (Hi. What is your name?)
           (Nice to meet you (get-state (person name))))
       (nop)
    )
)

(= (listen $_)
   (nop (change-state greeted True)))

;!(answer (listen (Hello)))
!(answer (listen (My name is "Alexey"))) ;doesn't work correctly now due to caching?
;!(answer (listen (My name is "Alexey")))
;!(answer (listen (My name is "Ben")))

;!(call:get_atoms &memory)

