!(add-reduct &self
  (= (code-generation)
     (snet-sdk create_service_client "naint" "code-generation"     
       ; Put your data here or use environment variables
       ;(Kwargs
       ;  (free_call_auth_token_bin FREE_CALL_AUTH_TOKEN_BIN)
       ;  (free_call_token_expiry_block FREE_CALL_TOKEN_EXPIRE_BLOCK)
       ;)
     )))

(: generate (-> String String))
(= (generate $request)
   ((code-generation) "generate" "Query"
    (Kwargs (request $request))
   )
)
