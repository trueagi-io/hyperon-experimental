!(extend-py! llm_gate)

! (bind! &messages (new-space))

; All these functions might not be needed if the prompt program is in a separate script
(: system! (-> Atom Atom))
(: user! (-> Atom Atom))
(: assistant! (-> Atom Atom))
(= (system! $msg) (add-atom &messages (system $msg)))
(= (user! $msg) (add-atom &messages (user $msg)))
(= (assistant! $msg) (add-atom &messages (assistant $msg)))

(= (proverb) "Where there is no guidance, a people falls,\\nbut in an abundance of counselors there is safety.")
(= (book) "Proverbs")
(= (chapter) 11)
(= (verse) 14)

; Somewhat inconvenient way via states to make in-text replacements with LLM calls
! (let $rewrite (new-state "[continue here]")
     (add-atom &self (= (rewrite) $rewrite)))

! (user! ("Tweak this proverb to apply to model instructions instead.\\n"
          (proverb)
          "\\n-" (book) (chapter) ":" (verse)
          "\\nUPDATED\\n"
          "Where there is no guidance" (get-state (rewrite))))

! (nop (change-state! (rewrite) (llm (Messages (get-atoms &messages)))))

! (atom2msg (get-atoms &messages))

