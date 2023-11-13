!(extend-py! llm_gate)

!(import! &msgs test_guide_prompt.metta)

; Function for calls still cannot be put into the prompt space, because
; they will not be evaluated, when the functional call is evoked here
(= (doc calc_math)
   (Doc
     (description "You should call this function with a mathematical expression in Scheme")
     (parameters
      (expression "Mathematical expression in Scheme" ())
     ))
)
; This is another limitation: LLM output should somehow be transormed to MeTTa expressions
(= (calc_math $expr $msgs)
   ($expr is not evaluated, because it is a string atm))

(= (user-query) "What is the result of 111102 + 18333?")

! (llm &msgs)
