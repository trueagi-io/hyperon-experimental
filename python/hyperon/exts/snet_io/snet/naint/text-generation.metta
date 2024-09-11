; This file serves for illustration and discussion
; It should not be included/imported
; since its code is generated automatically

; ==== This part is generated ====
!(add-reduct &self
  (= (naint text-generation)
     (snet-sdk create_service_client "naint" "text-generation"
       ; Put your data here or use environment variables
       ;(Kwargs
       ;  (free_call_auth_token_bin FREE_CALL_AUTH_TOKEN_BIN)
       ;  (free_call_token_expiry_block FREE_CALL_TOKEN_EXPIRE_BLOCK)
       ;)
     )))

; atm the most straightforward wrapper with basic types is generated 
(: gen_gpt_2 (-> String String Number Number Number String))
(= (gen_gpt_2 $start_text $run_name $temperature $top_k $length)
   ((text-generation) "gen_gpt_2" "Query"
    (Kwargs (start_text $start_text)
            (run_name $run_name)
            (temperature $temperature)
            (top_k $top_k)
            (length $length))
   )
)


; === This part is speculative ===

; OPTION 1
; cons: we can't have differently definded parameters
; with the same names in different methods
(: start_text (-> String string))
(: run_name (-> String string))
(: temperature (-> Number float))
(: top_k (-> Number int32))
(: length (-> Number int32))

; OPTION 2
; is to have custom types (with metta->proto
; type conversion under the hood) and unwrap them
(: start_text (-> String StartText))
(: run_name (-> String RunName))
(: temperature (-> Number Temperature))
(: gen_gpt_2_typed (-> StartText RunName Temperature TopK Length String))
(= (gen_gpt_2_typed (start_text $start_text) (run_name $run_name) (temperature $run_name) ...))
; gen_gpt_2 could call gen_gpt_2_typed as a syntactic sugar
; by wrapping its arguments of basic types into corresponding constructors
; here we run into the same issue that variable name in proto
; are not enough for introducing types
; The issue with run_name is also that it can have only specific
; values, which are not defined via its type

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