!(import! &self snet_io)

(include snet:naint:code-generation)
((py-dot (code-generation) get_service_messages))
; add a wrapper
(add-reduct &self
   (snet-sdk get_service_callers (code-generation)))
; code-generation requires passing data in json format,
; so it will return a parse error
;!(generate "Write Hello World in C#")
; FIXME: the following is converted to a Symbol, not String
;!(generate "{\"query\": \"Write Hello World in C#\"}"))
; A hacky way, which works for some reason atm
(generate (repr {"query":"Write_Hello_World_in_C#"}))

; Doesn't work atm
(include snet:naint:image-generation)
((image-generation)
     "Gen" "Text" (Kwargs (sentence "Hello World") (type False)))

(include snet:naint:generative-lms)
((generative-lms)
     "generate" "Query" (Kwargs (request "Write Hello World in C#")))

(include snet:naint:abstractive-summarisation)
((abstractive-summarisation)
     "neural_summarisation" "Query" (Kwargs (text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ullamcorper vehicula augue. Curabitur maximus aliquet ex sed fringilla. Mauris id erat dolor.")))

!(include snet:naint:text-generation)
!(add-reduct &self
   (snet-sdk get_service_callers (text-generation)))
!(gen_gpt_2 "What was the largest dinosaur?" "universal" 0.5 2 100)
