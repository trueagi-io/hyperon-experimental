!(add-reduct &self
  (= (text-generation)
     (snet-sdk create_service_client "naint" "text-generation"
       ; Put your data here or use environment variables
       ;(Kwargs
       ;  (free_call_auth_token_bin FREE_CALL_AUTH_TOKEN_BIN)
       ;  (free_call_token_expiry_block FREE_CALL_TOKEN_EXPIRE_BLOCK)
       ;)
     )))

; cons: we can't have differently definded parameters
; with the same names in different methods
(: start_text (-> String string))
(: run_name (-> String string))
(: temperature (-> Number float))
(: top_k (-> Number int32))
(: length (-> Number int32))

; Should be generated automatically
;(: gen_gpt_2 (-> String String Number Number Number String))
;(= (gen_gpt_2 $start_text $run_name $temperature $top_k $length)
;   ((text-generation) "gen_gpt_2" "Query"
;    (Kwargs (start_text $start_text)
;            (run_name $run_name)
;            (temperature $temperature)
;            (top_k $top_k)
;            (length $length))
;   )
;)

; A sketch of ontological relations
(:<proto AI:Text string)
(:<metta AI:Text String)
(domain AI:Text AI:NLP)
(inherit AI:Prompt AI:Text)
(is-a AI:LLM AI:Function)
(has-param AI:LLM AI:Prompt)
(alias AI:UnitRange (Range Float 0 1))
(inherit AI:Temperature AI:UnitRange)
(has-param AI:LLM AI:Temperature)

; A sketch of service function description
(inherit gen_gpt_2 AI:LLM)
(named-param gen_gpt_2 start_text AI:Prompt)
(named-param gen_gpt_2 temperature AI:Temperature)