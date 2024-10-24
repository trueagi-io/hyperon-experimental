!(import! &self snet_io)

; low-level API: we can create service client as an operation atom
(bind! &code-generation
  (snet-sdk create_service_client "naint" "code-generation"
       ;(Kwargs
       ;  (free_call_auth_token_bin FREE_CALL_AUTH_TOKEN_BIN)
       ;  (free_call_token_expiry_block FREE_CALL_TOKEN_EXPIRE_BLOCK)
  ))
; and make calls to it as a Python object
((py-dot &code-generation get_service_messages))
; and call a service as an operation atom
; NOTE: code-generation requires passing data in json format,
; so it will return a parse error for input "Write Hello World in C#"
; FIXME: the following is converted to a Symbol, not String
; "{\"query\": \"Write Hello World in C#\"}"
; A hacky way, which works for some reason atm
(&code-generation "generate" "Query"
   (Kwargs (request (repr {"query":"Write_Hello_World_in_C#"}))))

; Doesn't work atm
(bind! &image-generation
  (snet-sdk create_service_client "naint" "image-generation"))
(&image-generation
    "Gen" "Text" (Kwargs (sentence "Hello World") (type False)))

(bind! &generative-lms
  (snet-sdk create_service_client "naint" "generative-lms"))
; we can add a wrapper for the service
(add-reduct &self
  (snet-sdk get_service_callers &generative-lms))
; so instead of writing
;(&generative-lms
;   "generate" "Query" (Kwargs (request "Write Hello World in C#")))
;we can just call
(generate "Write Hello World in C#")

; but all this can be done with one command
(add-reduct &self
  (snet-sdk create_service_space naint abstractive-summarisation))
(neural_summarisation "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ullamcorper vehicula augue. Curabitur maximus aliquet ex sed fringilla. Mauris id erat dolor.")

(add-reduct &self
  (snet-sdk create_service_space naint text-generation))
(gen_gpt_2 "What was the largest dinosaur?" "universal" 0.5 2 100)

; possible usage of open_channel_and_deposit function. 100 is the amount of AGIX and 10000 is a payment channel's TTL in blocks
(snet-sdk open_channel_and_deposit &generative-lms 100 10000)