; !(import! &self snet_io)
; !(snet-service "naint" "image-generation")

;!(import! &self snet:naint:image-generation)
;!((image-generation)
;     "Gen" "Text" (Kwargs (sentence "Hello World") (type False)))

!(import! &self snet:naint:code-generation)
;((code-generation)
;     "generate" "Query" (Kwargs (request "Write Hello World in C#")))
!(generate "Write Hello World in C#")

;!(import! &self snet:naint:generative-lms)
;!((generative-lms)
;     "generate" "Query" (Kwargs (request "Write Hello World in C#")))

;!(import! &self snet:naint:abstractive-summarisation)
;!((abstractive-summarisation)
;     "neural_summarisation" "Query" (Kwargs (text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ullamcorper vehicula augue. Curabitur maximus aliquet ex sed fringilla. Mauris id erat dolor.")))

(import! &self snet:naint:text-generation)
((text-generation)
     "gen_gpt_2" "Query"
     (Kwargs (start_text "What was the largest dinosaur?")
             (run_name "universal")
             (temperature 0.5)
             (top_k 2)
             (length 100)))
