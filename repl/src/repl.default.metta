
; !(change-state! &ReplHistoryMaxLen 500) ; TODO: enable this when I have value bridging implemented.

!(change-state! &ReplPrompt "> ")
; !(change-state! &ReplStyledPrompt "\x1b[1;32m> \x1b[0m") ; TODO: currently the MeTTa string parser doesn't resolve escape chars, although perhaps it should

; TODO: somebody with better design sense should tweak these, and also provide dark-mode setings
; ANSI escape codes to configure the syntax highlighter
!(change-state! &ReplBracketStyles ("36" "35" "33" "32")) ; 36: cyan, 35: magenta, 33: yellow, 32: green
!(change-state! &ReplCommentStyle "90")                   ; 90: light gray
!(change-state! &ReplVariableStyle "33")                  ; 33: yellow
!(change-state! &ReplSymbolStyle "0")                     ; 0:  default text (as per terminal setting)
!(change-state! &ReplStringStyle "31")                    ; 31: dark red
!(change-state! &ReplErrorStyle "91")                     ; 91: bright red
!(change-state! &ReplBracketMatchStyle "1;7")             ; 1:  bold, 7: reverse (swap background and foreground colors)
; !(change-state! &ReplBracketMatchEnabled True) ;TODO: enable this when I have a reliable value interchange path built.  Another use for https://github.com/trueagi-io/hyperon-experimental/issues/351
