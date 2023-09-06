
; TODO: Let the "includePaths" be modifiable, but I want better string manipulation atoms

(= ReplDefaultPrompt "> ")
; (= ReplStyledPrompt "\x1b[1;32m> \x1b[0m") ; TODO: currently the MeTTa string parser doesn't resolve escape chars, although perhaps it should

; TODO: somebody with better design sense should tweak these, and also provide dark-mode setings
; ANSI escape codes to configure the syntax highlighter
(= ReplBracketStyles ("94" "93" "95" "96"))
(= ReplCommentStyle "32")
(= ReplVariableStyle "33")
(= ReplSymbolStyle "34")
(= ReplStringStyle "31")
(= ReplErrorStyle "91")
(= ReplBracketMatchStyle "1;7")
; (= ReplBracketMatchEnabled True) ;TODO: enable this when I have a reliable value interchange path built.  Another use for https://github.com/trueagi-io/hyperon-experimental/issues/351
