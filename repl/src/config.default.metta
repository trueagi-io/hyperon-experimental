
; TODO: Let the "includePaths" be modifiable, but I want better string manipulation atoms

(= DefaultPrompt "> ")
; (= StyledPrompt "\x1b[1;32m> \x1b[0m") ; TODO: currently the MeTTa string parser doesn't resolve escape chars, although perhaps it should

; TODO: somebody with better design sense should tweak these, and also provide dark-mode setings
; ANSI escape codes to configure the syntax highlighter
(= BracketStyles ("94" "93" "95" "96"))
(= CommentStyle "32")
(= VariableStyle "33")
(= SymbolStyle "34")
(= StringStyle "31")
(= ErrorStyle "91")
(= BracketMatchStyle "1;7")
; (= BracketMatchEnabled True) ;TODO: enable this when I have a reliable value interchange path built.  Another use for https://github.com/trueagi-io/hyperon-experimental/issues/351
